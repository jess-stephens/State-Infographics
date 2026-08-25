# =========================================================================
# Project: AOTH State Infographics v2
# Script:  manage_awards.R
# Path:    /manage_awards.R
# Version: 2026-05-06-v06
# Purpose: Execute the data pipeline for master list of awards
# =========================================================================


library(tidyverse)
library(janitor)
library(fs)
library(lubridate)
library(rlang)

source("scripts/data_utils.R")


# ===========================================================================
# DOCUMENTATION: manage_awards.R
# ===========================================================================
#' DATA DICTIONARY:
#' @field award_id [chr] Unique federal award identifier (FAIN/PIID).
#' @field recipient_name [chr] Normalized legal name of the entity.
#' @field recipient_uei [chr] Unique Entity Identifier for the recipient.
#' @field award_amount [dbl] Total value of the obligation (USD).
#' @field start_date [date] Performance period start.
#' @field end_date [date] Performance period end.
#' @field is_active [lgl] Logical flag if the award end_date >= Today.
#' 
#' WORKFLOW:
#' @workflow Sequence 3: Extract raw award details for all "INCLUDE" accounts.
#' @workflow Sequence 4: Prune results to the performance-period window.
# ===========================================================================


# -------------------------------------
# --- 1. Configuration & Parameters ---
# -------------------------------------

## 1A. Extract Global Parameters
params_from_registry <- set_parameters()

## 1B. Unified Parameter Object
params <- list(
  target_start     = as.Date(params_from_registry$target_start),
  target_end       = as.Date(params_from_registry$target_end),
  comparison_start = as.Date(params_from_registry$comparison_start),
  comparison_end   = as.Date(params_from_registry$comparison_end),
  analysis_start   = as.Date(params_from_registry$analysis_start),
  analysis_end     = as.Date(params_from_registry$analysis_end),
  discovery_start  = as.Date(params_from_registry$analysis_start) - (as.Date(params_from_registry$analysis_end)-as.Date(params_from_registry$analysis_start)),
  discovery_end    = as.Date(params_from_registry$analysis_end)
)
  
  # Derived filter for API ingestion and registry logging
  params$api_template <- list(
    time_period = list(list(
      start_date = as.character(params$discovery_start),
      end_date   = as.character(params$discovery_end)
    )),
    recipient_scope = "domestic",
    fields = c("Award ID", "Recipient Name", "Recipient UEI", "recipient_id",
               "Award Amount", "Start Date", "End Date"),
    limit = 50
  )
  
  params$award_groups <- list(
    contracts = c("A", "B", "C", "D"),
    grants    = c("02", "03", "04", "05")
  )
  
  params$paths <- list(
    api_raw  = "data/raw/award_list_from_api.csv",
    master   = "data/reference/award_list_master.csv",
    accounts = "data/reference/account_list_master.csv"
  )
  
  params$datasets <- list(
    api_raw = "award_list_from_api",
    master  = "award_list_master"
  )
  
  params$mod_names <- list(
    api_raw = "manage_awards.R",
    master  = "manage_awards.R"
  )
  
  params$params_from_registry <- params_from_registry



# -----------------------------------------
# --- 2. FUNCTION: ingest_awards ----------
# ------ API -> award_list_from_api.csv ---
# -----------------------------------------

ingest_awards <- function() {
  message("Action: Verifying Account Master synchronization...")
  if(!file_exists(params$paths$accounts)) stop("Account Master not found. Run manage_accounts.R first.")
  
  target_accounts <- read_csv(params$paths$accounts, show_col_types = FALSE) %>%
    filter(analysis_status == "INCLUDE")
  
  if(nrow(target_accounts) == 0) stop("No accounts assigned as 'INCLUDE'. Run manage_accounts.R to assign.")
  
  # Generate TAS hash for the registry
  target_tas_list <- sort(target_accounts$tas_code)
  tas_hash <- rlang::hash(target_tas_list)
  
  message(paste("Executing API Ingestion for", length(target_tas_list), "accounts..."))
  
  # Extraction Loop using Template-Injection
  raw_collection <- purrr::map_df(target_tas_list, function(current_tas) {
    acc_info <- target_accounts %>% filter(tas_code == current_tas)
    
    purrr::map_dfr(names(params$award_groups), function(group_name) {
      message(paste("  ... Processing TAS:", current_tas, "| Group:", group_name))
      
      query_body <- list(
        filters = list(
          time_period = params$api_template$time_period,
          treasury_account_components = list(list(
            aid = acc_info$agency_code, 
            main = acc_info$account_code
          )),
          recipient_scope = params$api_template$recipient_scope,
          award_type_codes = params$award_groups[[group_name]]
        ),
        fields = params$api_template$fields,
        limit  = params$api_template$limit, 
        sort   = "Award ID", 
        order  = "asc"
      )
      
      batch <- call_usaspending_api(
        endpoint = "/api/v2/search/spending_by_award/",
        query_body = query_body
      )
      
      if (nrow(batch) > 0) {
        batch <- batch %>%
          mutate(
            tas_reference = current_tas,
            api_group = group_name,
            across(contains("date"), as.Date),
            award_amount = as.numeric(award_amount)
          )
      }
      return(batch)
    })
  })
  
  final_raw <- raw_collection %>% distinct(award_id, .keep_all = TRUE)
  write_csv(final_raw, params$paths$api_raw)
  
  # Update registry with Template and Execution Metadata
  update_registry(
    d_name   = params$datasets$api_raw,
    mod_name = params$mod_names$api_raw,
    ts       = Sys.time(),
    global_p = params$params_from_registry,
    local_p  = list(
      api_template = params$api_template,
      execution_metadata = list(
        account_count = length(target_tas_list),
        account_hash = tas_hash
      )
    )
  )
  message(paste("Success: API transfer packet saved with", nrow(final_raw), "records."))
}


# -----------------------------------------------------------
# --- 3. FUNCTION: prune_awards -----------------------------
# ------ award_list_from_api.csv -> award_list_master.csv ---
# -----------------------------------------------------------

prune_awards <- function() {
  if(!file_exists(params$paths$api_raw)) return(message("Error: Run Ingest (3) first."))
  
  message("Action: Pruning awards based on performance overlap...")
  raw_data <- read_csv(params$paths$api_raw, show_col_types = FALSE)
  
  master_awards <- raw_data %>%
    filter(
      end_date >= params$analysis_start,
      start_date <= params$analysis_end
    ) %>%
    mutate(
      recipient_name = str_to_upper(recipient_name),
      is_active = if_else(end_date >= Sys.Date(), TRUE, FALSE)
    ) %>%
    arrange(desc(award_amount))
  
  write_csv(master_awards, params$paths$master)
  
  update_registry(
    d_name   = params$datasets$master,
    mod_name = params$mod_names$master,
    ts       = Sys.time(),
    global_p = params$params_from_registry,
    local_p  = list(pruned_count = nrow(master_awards))
  )
  message(paste("Success: Master Award list curated at:", params$paths$master))
}


# --------------------------------------
# --- 4. FUNCTION: run_award_manager ---
# ------ Dashboard controller ----------
# --------------------------------------

run_award_manager <- function() {
  verify_pipeline_sync(params$datasets$api_raw)
  
  while (TRUE) {
    # Retrieve latest registry states for UI summary
    reg <- read_csv("data/reference/data_registry.csv", show_col_types = FALSE)
    
    s3_entry <- reg %>% filter(dataset_name == params$datasets$api_raw)
    s4_entry <- reg %>% filter(dataset_name == params$datasets$master)
    
    # Extract counts using jsonlite if they exist
    s3_acc_count <- "0"
    if(nrow(s3_entry) > 0 && !is.na(s3_entry$local_params)) {
      # Safe parse: only triggers if data exists
      s3_acc_count <- jsonlite::fromJSON(s3_entry$local_params)$execution_metadata$account_count
    }
    
    s3_award_count <- "0"
    if(file_exists(params$paths$api_raw)) {
      s3_award_count <- nrow(read_csv(params$paths$api_raw, show_col_types = FALSE))
    }
    
    s4_award_count <- "PENDING"
    if(nrow(s4_entry) > 0 && !is.na(s4_entry$local_params)) {
      # Safe parse: prevents the "missing value where TRUE/FALSE needed" error
      s4_award_count <- jsonlite::fromJSON(s4_entry$local_params)$pruned_count
    }
    
    cat("\n==========================================")
    cat("\n  AOTH AWARD MANAGEMENT: PIPELINE v2")
    cat("\n==========================================")
    cat(paste("\nProject DNA: ", params$target_start, " to ", params$target_end))
    cat(paste("\n[Seq 3] API Pull:  ", s3_award_count, " awards (from ", s3_acc_count, " accounts)"))
    cat(paste("\n[Seq 4] Master:    ", s4_award_count, " awards (pruned for window)"))
    cat("\n------------------------------------------")
    cat("\n1. EXECUTE FULL SEQUENCE (Fetch & Prune)")
    cat("\n2. EXIT")
    
    choice <- readline("\nSelect an option (1-2): ")
    
    if (choice == "1") {
      ingest_awards()
      prune_awards()
      message("\nPipeline Complete: Transfer packet saved and Master list curated.")
    } 
    else if (choice == "2") {
      clean_up()
      break
    }
  }
}


# -------------------------------
# --- 5. STANDALONE EXECUTION ---
# -------------------------------

if (interactive()) run_award_manager()