# =========================================================================
# Project: AOTH State Infographics v2
# Script:  manage_accounts.R
# Purpose: Execute the data pipeline for master list of funding accounts
# =========================================================================

library(tidyverse)
library(janitor)

# Load modular library components
source("scripts/data_utils.R") 

# --- 1. Configuration & Parameters ---
params <- list(
  start_date = "2022-10-01",
  end_date   = "2026-09-30",
  agencies = list(
    list(api_name = "Agency for International Development", short_name = "USAID"),
    list(api_name = "Department of State", short_name = "State")
  ),
  # Verbatim file names as per user reference
  api_path    = "data/raw/account_list_from_api.csv",
  master_path = "data/reference/account_list_master.csv",
  # Registry Metadata
  seq_id = 1,
  dataset_name = "account_master"
)

# --- 2. Helper Logic ---
api_agency_filter <- map(params$agencies, ~list(
  type = "awarding", 
  tier = "toptier", 
  name = .x$api_name
))

metadata_label <- paste0(
  "Agencies: ", paste(map_chr(params$agencies, "short_name"), collapse = "|"), 
  "; Range: ", params$start_date, " to ", params$end_date
)

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

# --- 3. Functional Gates ---

# GATE 1: API -> account_list_from_api.csv
ingest_accounts <- function() {
  message("Executing API Ingestion...")
  full_data <- call_usaspending_api(
    endpoint = "/api/v2/search/spending_by_category/federal_account/",
    query_body = list(
      filters = list(
        time_period = list(list(start_date = params$start_date, end_date = params$end_date)),
        agencies = api_agency_filter
      ),
      limit = 100
    )
  )
  
  # Align to established account_list_from_api.csv schema verbatim
  full_data <- full_data %>%
    rename(
      tas_code = code,
      account_name = name,
      total_obligations = amount
    ) %>%
    # Logic: Split on the hyphen to isolate Agency (072) and Account (1037)
    separate(tas_code, into = c("agency_code", "account_code"), 
             sep = "-", remove = FALSE, extra = "drop", fill = "right") %>%
    mutate(
      query_filter = metadata_label
    ) %>%
    select(tas_code, agency_code, account_code, account_name, 
           total_obligations, pull_timestamp, query_filter)
  
  write_csv(full_data, params$api_path)
  
  # Register the pull in long-term memory
  update_registry(params$seq_id, params$dataset_name, "pull_timestamp", max(full_data$pull_timestamp))
  message(paste("API transfer packet saved to:", params$api_path))
}

# GATE 2: account_list_from_api.csv -> account_list_master.csv
assign_accounts <- function() {
  if(!file.exists(params$api_path)) return(message("Error: Run Fetch (1) first."))
  
  api_data <- read_csv(params$api_path, show_col_types = FALSE)
  master   <- if(file.exists(params$master_path)) read_csv(params$master_path, show_col_types = FALSE) else tibble()
  
  unassigned_delta <- api_data %>%
    anti_join(master, by = "tas_code")
  
  if (nrow(unassigned_delta) == 0) {
    message("Check complete: No new accounts require assignment.")
    return(NULL)
  }
  
  message(paste("Found", nrow(unassigned_delta), "new accounts for review."))
  
  # Logic: 072 Auto-Include
  auto_include <- unassigned_delta %>% 
    filter(agency_code == 72) %>%
    mutate(analysis_status = "INCLUDE")
  
  manual_review <- unassigned_delta %>% 
    filter(agency_code != 72)
  
  if(nrow(auto_include) > 0) message(paste("Auto-assigning", nrow(auto_include), "USAID accounts."))
  
  if(nrow(manual_review) > 0) {
    manual_review <- manual_review %>%
      mutate(analysis_status = map_chr(account_name, get_user_assignment))
  }
  
  # Final alignment to account_list_master.csv schema verbatim
  new_assignments <- bind_rows(auto_include, manual_review) %>%
    select(tas_code, agency_code, account_code, account_name, 
           total_obligations, analysis_status, query_filter, pull_timestamp)
  
  updated_master <- bind_rows(master, new_assignments)
  write_csv(updated_master, params$master_path)
  
  # Register the assignment
  update_registry(params$seq_id, params$dataset_name, "assign_timestamp", Sys.time())
  message(paste("Master ledger updated at:", params$master_path))
}

# GATE 3: Refine account_list_master.csv
refine_accounts <- function() {
  if(!file.exists(params$master_path)) return(message("Error: Master file does not exist."))
  
  master <- read_csv(params$master_path, show_col_types = FALSE)
  target_tas <- readline("Enter TAS Code to refine: ")
  
  if (!(target_tas %in% master$tas_code)) return(message("Error: TAS Code not found."))
  
  current_name <- master %>% filter(tas_code == target_tas) %>% pull(account_name)
  new_status   <- get_user_assignment(current_name)
  
  master <- master %>%
    mutate(analysis_status = if_else(tas_code == target_tas, new_status, analysis_status))
  
  write_csv(master, params$master_path)
  update_registry(params$seq_id, params$dataset_name, "assign_timestamp", Sys.time())
}

# --- 4. Dashboard Controller ---
run_account_manager <- function() {
  verify_pipeline_sync(params$seq_id)
  
  while (TRUE) {
    master <- if(file.exists(params$master_path)) read_csv(params$master_path, show_col_types = FALSE) else tibble()
    pending_count <- if(nrow(master) > 0) sum(master$analysis_status == "PENDING", na.rm = TRUE) else 0
    last_pull     <- if(nrow(master) > 0) max(master$pull_timestamp, na.rm = TRUE) else "None"
    
    status_stats  <- if(nrow(master) > 0) {
      master %>% count(analysis_status) %>% mutate(s = paste0(analysis_status, ": ", n)) %>% pull(s) %>% paste(collapse = " | ")
    } else { "Empty" }
    
    cat("\n==========================================")
    cat("\n   AOTH ACCOUNT MANAGEMENT DASHBOARD")
    cat("\n==========================================")
    cat(paste0("\n1. FETCH NEW DATA   (Last Pull: ", last_pull, ")"))
    cat(paste0("\n2. ASSIGN ACCOUNTS  (", pending_count, " PENDING entries)"))
    cat(paste0("\n3. REFINE ACCOUNT   (", status_stats, ")"))
    cat("\n4. EXIT")
    cat("\n------------------------------------------")
    
    choice <- readline("\nSelect an option (1-4): ")
    
    if (choice == "1") {
      ingest_accounts()
    } else if (choice == "2") {
      assign_accounts()
    } else if (choice == "3") {
      refine_accounts()
    } else if (choice == "4") {
      message("Exiting. Remember to save final materials to the repository.")
      break
    }
  }
}

if (interactive()) {
  run_account_manager()
}