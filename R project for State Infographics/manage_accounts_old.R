# =========================================================================
# Project: AOTH State Infographics v2
# Script:  manage_accounts.R
# Path:    /manage_accounts.R
# Version: 2026-05-06-v01
# Purpose: Execute the data pipeline for master list of funding accounts
# =========================================================================


library(tidyverse)
library(janitor)
library(fs)

source("scripts/data_utils.R") 


# ===========================================================================
# DOCUMENTATION: manage_accounts.R
# ===========================================================================
#' DATA DICTIONARY:
#' @field tas_code [chr] Unique Treasury Account Symbol (e.g., "072-1010").
#' @field agency_code [chr] 3-digit federal agency identifier (USAID = 072).
#' @field account_code [chr] Specific fund account identifier.
#' @field account_name [chr] Formal title of the funding account.
#' @field total_obligations [dbl] Cumulative financial commitment (USD).
#' @field analysis_status [fct] Inclusion logic: INCLUDE | EXCLUDE | PENDING.
#' @field pull_timestamp [dttm] Date/time of the literal API transaction.
#' 
#' WORKFLOW:
#' @workflow Sequence 1: Fetch raw data from USASpending API.
#' @workflow Sequence 2: Map raw data to Master Ledger with user assignment.
# ===========================================================================


# -------------------------------------
# --- 1. Configuration & Parameters ---
# -------------------------------------

## 1A. Extract Global Parameters
params_from_registry <- set_parameters() 

## 1B. Unified Parameter Object
params <- list(
  
  # Time periods for easy reference in module
  start_date = params_from_registry$analysis_start, 
  end_date   = params_from_registry$analysis_end, 
  
  # Derived filter for API ingestion and registry logging
  api_filter = list(
    time_period = list(list(
      start_date = params_from_registry$analysis_start, 
      end_date   = params_from_registry$analysis_end
    )),
    agencies = list(
      list(type = "awarding", tier = "toptier", name = "Agency for International Development"),
      list(type = "awarding", tier = "toptier", name = "Department of State")
    )
  ),
  
  # Structural paths
  paths = list(
    api_raw = "data/raw/account_list_from_api.csv",
    master  = "data/reference/account_list_master.csv"
  ),
  
  # Registry keys for sequence updates
  datasets = list(
    api_raw = "account_list_from_api",
    master  = "account_list_master"
  ),
  
  # Registry keys for sequence updates
  mod_names = list(
    api_raw = "manage_accounts.R",
    master  = "manage_accounts.R"
  ),
  
  # Global parameter reference for registry consistency
  params_from_registry = params_from_registry
)


# ----------------------------------------------
# --- 2. FUNCTION: get_user_assignment ---------
# ------ user input for iterative assignment ---
# ----------------------------------------------

get_user_assignment <- function(account_name) {
  cat(paste0("\nAccount: ", account_name))
  choice <- readline("Assign Status (i=INCLUDE, e=EXCLUDE, s=Skip/PENDING): ")
  
  status <- switch(tolower(choice),
                   "i" = "INCLUDE",
                   "e" = "EXCLUDE",
                   "s" = "PENDING",
                   "PENDING") 
  return(status)
}

# -------------------------------------------
# --- 3. FUNCTION: ingest_accounts ----------
# ------ API -> account_list_from_api.csv ---
# -------------------------------------------

ingest_accounts <- function() {
  message("Executing API Ingestion...")
  
  # Use filter already defined in params
  full_data <- call_usaspending_api(
    endpoint = "/api/v2/search/spending_by_category/federal_account/",
    query_body = list(
      filters = params$api_filter,
      limit = 100
    )
  )
  
  # Clean and separate per schema
  full_data <- full_data %>%
    rename(tas_code = code, account_name = name, total_obligations = amount) %>%
    separate(tas_code, into = c("agency_code", "account_code"), 
             sep = "-", remove = FALSE, extra = "drop", fill = "right") %>%
    mutate(pull_timestamp = Sys.time()) %>%
    select(tas_code, agency_code, account_code, account_name, 
           total_obligations, pull_timestamp)
  
  write_csv(full_data, params$paths$api_raw)
  
  # Update registry
  update_registry(
    d_name   = params$datasets$api_raw,
    mod_name = params$mod_names$api_raw,
    ts       = max(full_data$pull_timestamp),
    global_p = params$params_from_registry,
    local_p  = params$api_filter 
  )
  
  message(paste("API transfer packet saved to:", params$paths$api_raw))
}


# ---------------------------------------------------------------
# --- 4. FUNCTION: assign_accounts ------------------------------
# ------ account_list_from_api.csv -> account_list_master.csv ---
# ---------------------------------------------------------------

assign_accounts <- function() {
  if(!file_exists(params$paths$api_raw)) return(message("Error: Run Fetch (1) first."))
  
  api_data <- read_csv(params$paths$api_raw, show_col_types = FALSE)
  master   <- if(file_exists(params$paths$master)) read_csv(params$paths$master, show_col_types = FALSE) else tibble()
  
  unassigned_delta <- api_data %>% anti_join(master, by = "tas_code")
  
  if (nrow(unassigned_delta) == 0) {
    message("Check complete: No new accounts require assignment.")
    return(NULL)
  }
  
  # Automation: Auto-include USAID (072)
  auto_include <- unassigned_delta %>% 
    filter(agency_code == "072") %>%
    mutate(analysis_status = "INCLUDE")
  
  manual_review <- unassigned_delta %>% 
    filter(agency_code != "072")
  
  if(nrow(auto_include) > 0) message(paste("Auto-assigning", nrow(auto_include), "USAID accounts."))
  
  if(nrow(manual_review) > 0) {
    manual_review <- manual_review %>%
      mutate(analysis_status = map_chr(account_name, get_user_assignment))
  }
  
  new_assignments <- bind_rows(auto_include, manual_review) %>%
    select(tas_code, agency_code, account_code, account_name, 
           total_obligations, analysis_status, pull_timestamp)
  
  updated_master <- bind_rows(master, new_assignments)
  write_csv(updated_master, params$paths$master)
  
  # Update registry
  update_registry(
    d_name   = params$datasets$master,
    mod_name = params$mod_names$master,
    ts       = Sys.time(),
    global_p = params$params_from_registry,
    local_p  = list(new_assignments_count = nrow(new_assignments))
  )
  
  message(paste("Master ledger updated at:", params$paths$master))
}


# -------------------------------------------------------------
# --- 5. FUNCTION: refine_accounts ----------------------------
# ------ account_list_master.csv -> account_list_master.csv ---
# -------------------------------------------------------------

refine_accounts <- function() {
  if(!file_exists(params$paths$master)) return(message("Error: Master file does not exist."))
  
  master <- read_csv(params$paths$master, show_col_types = FALSE)
  target_tas <- readline("Enter TAS Code to refine: ")
  
  if (!(target_tas %in% master$tas_code)) return(message("Error: TAS Code not found."))
  
  current_name <- master %>% filter(tas_code == target_tas) %>% pull(account_name)
  new_status   <- get_user_assignment(current_name)
  
  master <- master %>%
    mutate(analysis_status = if_else(tas_code == target_tas, new_status, analysis_status))
  
  write_csv(master, params$paths$master)
  
  update_registry(
    d_name   = params$datasets$master,
    mod_name = params$mod_names$master,
    ts       = Sys.time(),
    global_p = params$params_from_registry
  )
}


# ----------------------------------------
# --- 6. FUNCTION: run_account_manager ---
# ------ Dashboard controller ------------
# ----------------------------------------

run_account_manager <- function() {

    # Verify sequence is in sync with global params
  verify_pipeline_sync(params$datasets$api_raw)
  
  while (TRUE) {
    master <- if(file_exists(params$paths$master)) read_csv(params$paths$master, show_col_types = FALSE) else tibble()
    pending_count <- if(nrow(master) > 0) sum(master$analysis_status == "PENDING", na.rm = TRUE) else 0
    
    cat("\n==========================================")
    cat("\n   AOTH ACCOUNT MANAGEMENT DASHBOARD")
    cat("\n==========================================")
    cat(paste0("\nProject Window: ", params$start_date, " to ", params$end_date))
    cat("\n1. FETCH NEW DATA   (Seq 1)")
    cat("\n2. ASSIGN ACCOUNTS  (Seq 2)")
    cat("\n3. REFINE ACCOUNT")
    cat("\n4. EXIT")
    cat("\n------------------------------------------")
    
    choice <- readline("\nSelect an option (1-4): ")
    
    if (choice == "1") ingest_accounts()
    else if (choice == "2") assign_accounts()
    else if (choice == "3") refine_accounts()
    else if (choice == "4") {
      message("Exiting. Remember to save final materials to the repository.")
      clean_up()
      break
    }
  }
}


# -------------------------------
# --- 7. STANDALONE EXECUTION ---
# -------------------------------

if (interactive()) run_account_manager()