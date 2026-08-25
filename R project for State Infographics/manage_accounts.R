# =========================================================================
# Project: AOTH State Infographics v2
# Script:  manage_accounts.R
# Path:    /manage_accounts.R
# Version: 2026-05-13-v01
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
#' @workflow Sequence 1: Ingest [ingest_accounts] - Batch pull from USASpending.
#' @workflow Sequence 2: Assign [assign_accounts] - Delta-check and USAID auto-filter.
#' @workflow Sequence 3: Refine [refine_accounts] - Atomic metadata preservation for manual overrides.
#' 
#' REGISTRY INTEGRITY:
#' @note All functions update 'data_registry.csv' via [update_registry].
#' @note Lineage is preserved in Section 5 by pulling 'input_params' from existing registry state.
# ===========================================================================


# ---------------------------------------------------------------------------
# --- 1. CONFIGURATION & PARAMETERS -----------------------------------------
# ---------------------------------------------------------------------------

## 1.1. Extract Global Parameters
params_from_registry <- set_parameters() 

## 1.2. Unified Parameter Object
params <- list(
  
  # 1.2.1. Time periods for easy reference in module
  start_date = params_from_registry$analysis_start, 
  end_date   = params_from_registry$analysis_end, 
  
  # 1.2.2. Derived filter for API ingestion and registry logging
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
  
  # 1.2.3. Structural paths
  paths = list(
    api_raw = "data/raw/account_list_from_api.csv",
    master  = "data/reference/account_list_master.csv"
  ),
  
  # 1.2.4. Registry keys for sequence updates
  datasets = list(
    api_raw = "account_list_from_api",
    master  = "account_list_master"
  ),
  mod_names = list(
    api_raw = "manage_accounts.R",
    master  = "manage_accounts.R"
  ),
  
  # 1.2.5. Global parameter reference for registry consistency
  params_from_registry = params_from_registry
)


# ---------------------------------------------------------------------------
# --- 2. FUNCTION_01: [ get_user_assignment ] -------------------------------
# ------ user input for iterative assignment --------------------------------
# ---------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------
# --- 3. FUNCTION_02: [ ingest_accounts ] -----------------------------------
# ------ API -> account_list_from_api.csv -----------------------------------
# ---------------------------------------------------------------------------

ingest_accounts <- function() {
  message("Executing API Ingestion...")
  
  # 3.1. Structure & execute API call
  api_packet <- try_api(

    # 3.1.1. try_api controls
    retries      = 3,
    retry_pause  = 2,
        
    # 3.1.1. Specify provider-specific API function
    api_function = fedspender,
    
    # 3.1.2. Primary API arguments
    endpoint     = "/api/v2/search/spending_by_category/federal_account/",
    params       = list(
      filters = params$api_filter,
      limit   = 100
    ),
    method       = "POST",
    
    # 3.1.3. API pagination controls
    timeout      = 60,
    page_pause   = 1
  )
  
  # 3.2. Extract data payload
  api_data <- unbundle_data(api_bundle = api_packet)
  
  if (nrow(api_data) == 0) {
    message("Warning: No accounts returned for the selected filter.")
    return(NULL)
  }
  
  # 3.3. Clean and separate per schema
  api_data <- api_data %>%
    rename(
      tas_code          = code, 
      account_name      = name, 
      total_obligations = amount) %>%
    separate(
      tas_code, 
      into   = c("agency_code", "account_code"),
      sep    = "-", 
      remove = FALSE, 
      extra  = "drop", 
      fill   = "right") %>%
    mutate(pull_timestamp = Sys.time()) %>%
    select(tas_code, 
           agency_code, 
           account_code, 
           account_name, 
           total_obligations, 
           pull_timestamp)
  
  # 3.4. Write to file
  write_csv(api_data, params$paths$api_raw)
  
  # 3.5. Update registry
  update_registry(
    d_name   = params$datasets$api_raw,
    mod_name = params$mod_names$api_raw,
    ts       = max(api_data$pull_timestamp),
    global_p = params$params_from_registry,
    input_p  = list(
      static_query = params$api_filter
      ),
    output_p = list(
      data_status = "complete",
      data_counts = nrow(api_data)
    )
  )
  
  message(paste(nrow(api_data), "accounts retrieved from API and saved to:", params$paths$api_raw))
}


# ---------------------------------------------------------------------------
# --- 4. FUNCTION_03: [ assign_accounts ] -----------------------------------
# ------ account_list_from_api.csv -> account_list_master.csv ---------------
# ---------------------------------------------------------------------------

assign_accounts <- function() {
  if(!file_exists(params$paths$api_raw)) return(message("Error: Run Fetch (1) first."))
  
  api_data <- read_csv(params$paths$api_raw, show_col_types = FALSE)
  master   <- if(file_exists(params$paths$master)) read_csv(params$paths$master, show_col_types = FALSE) else tibble()
  
  unassigned_delta <- api_data %>% anti_join(master, by = "tas_code")
  
  if (nrow(unassigned_delta) == 0) {
    message("Check complete: No new accounts require assignment.")
    return(NULL)
  }
  
  # 4.1. Automation: Auto-include USAID (072)
  auto_include <- unassigned_delta %>% 
    filter(agency_code == "072") %>%
    mutate(analysis_status = "INCLUDE")
  
  manual_review <- unassigned_delta %>% 
    filter(agency_code != "072")
  
  if(nrow(auto_include) > 0) message(paste("Auto-assigning", nrow(auto_include), "USAID accounts."))
  
  if(nrow(manual_review) > 0) {
    manual_review <- manual_review %>%
      mutate(analysis_status = purrr::map_chr(account_name, get_user_assignment))
  }
  
  new_assignments <- bind_rows(auto_include, manual_review) %>%
    select(tas_code, 
           agency_code, 
           account_code, 
           account_name, 
           total_obligations, 
           analysis_status, 
           pull_timestamp)
  
  updated_master <- bind_rows(master, new_assignments)
  write_csv(updated_master, params$paths$master)
  
  # 4.2. Update registry
  update_registry(
    d_name   = params$datasets$master,
    mod_name = params$mod_names$master,
    ts       = Sys.time(),
    global_p = params$params_from_registry,
    input_p  = list(
      input_dataset = params$datasets$api_raw,
      input_count   = nrow(api_data)
      ), 
    output_p = list(
      data_status = "complete",
      data_counts  = list(
        output_count  = nrow(updated_master),
        last_assigned = nrow(new_assignments) 
      )
    )
  )
  
  message(
    paste("Success:", 
          params$datasets$master, 
          "updated with", 
          nrow(new_assignments), 
          "new assignments. Total records:", 
          nrow(updated_master)
          )
    )
  }


# ---------------------------------------------------------------------------
# --- 5. FUNCTION_04: [ refine_accounts ] -----------------------------------
# ------ account_list_master.csv -> account_list_master.csv -----------------
# ---------------------------------------------------------------------------

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
  
  existing_input_p <- get_registry_config(params$datasets$master) %>% 
    pull(input_params)
  
  update_registry(
    d_name   = params$datasets$master,
    mod_name = params$mod_names$master,
    ts       = Sys.time(),
    global_p = params$params_from_registry,
    input_p  = existing_input_p,
    output_p = list(
      data_status = "complete",
      data_counts = list(
        output_count = nrow(master), 
        last_refined = 1
      )
    )
  )
  
  message(paste("Success: TAS", target_tas, "updated from", current_name, "to", new_status))
}


# ---------------------------------------------------------------------------
# --- 6. FUNCTION_05: [ run_account_manager ] -------------------------------
# ------ Dashboard controller -----------------------------------------------
# ---------------------------------------------------------------------------

run_account_manager <- function() {

    # 6.1. Verify sequence is in sync with global params
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


# ---------------------------------------------------------------------------
# --- 7. STANDALONE EXECUTION -----------------------------------------------
# ---------------------------------------------------------------------------

if (interactive()) run_account_manager()