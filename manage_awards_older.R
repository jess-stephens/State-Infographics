# =========================================================================
# Project: AOTH State Infographics v2
# Module:  manage_awards.R
# Purpose: Orchestrates multi-account API pulls with integrated pruning 
#          to generate both Raw and Master award lists.
# =========================================================================

library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)
library(lubridate)
library(fs)
library(rlang)

# --- 1. Initialization ---
source("scripts/data_utils.R")

path_raw_out    <- "data/raw/award_list_from_api.csv"
path_master_out <- "data/reference/award_list_master.csv"
fs::dir_create("data/raw/")

# --- 2. Parameters ---
# Analysis Window: FY24-FY25
analysis_start_date <- as.Date("2023-10-01") 
analysis_end_date   <- as.Date("2025-09-30")

discovery_start_date <- analysis_start_date - years(1)
current_date         <- Sys.Date()
fy_end_year          <- if (month(current_date) >= 10) year(current_date) + 1 else year(current_date)
discovery_end_date   <- as.Date(paste0(fy_end_year, "-09-30"))

params <- list(
  discovery_start = as.character(discovery_start_date),
  discovery_end   = as.character(discovery_end_date)
)

award_groups <- list(
  contracts = c("A", "B", "C", "D"),
  grants    = c("02", "03", "04", "05")
)

# --- 3. Load Target Accounts ---
target_accounts <- read_csv("data/reference/account_list_master.csv", show_col_types = FALSE) %>%
  filter(analysis_status == "INCLUDE") %>%
  select(tas_code, agency_code, account_code)

# --- 4. Execution: Extraction Loop ---
message(paste("Starting full extraction for", nrow(target_accounts), "accounts..."))

raw_award_collection <- purrr::map_df(target_accounts$tas_code, function(current_tas) {
  
  acc_info <- target_accounts %>% filter(tas_code == current_tas)
  
  purrr::map_dfr(names(award_groups), function(group_name) {
    
    message(paste("Processing TAS:", current_tas, "| Group:", group_name))
    
    query_body <- list(
      filters = list(
        time_period = list(list(start_date = params$discovery_start, 
                                end_date = params$discovery_end)),
        treasury_account_components = list(list(aid = acc_info$agency_code, 
                                                main = acc_info$account_code)),
        recipient_scope = "domestic",
        award_type_codes = award_groups[[group_name]]
      ),
      fields = c("Award ID", "Recipient Name", "Recipient UEI", 
                 "Award Amount", "Start Date", "End Date"),
      limit = 100
    )
    
    batch <- call_usaspending_api(
      endpoint = "/api/v2/search/spending_by_award/",
      query_body = query_body
    )
    
    # Logic: Only process if results exist
    if (nrow(batch) > 0) {
      batch <- batch %>%
        mutate(
          tas_reference = current_tas,
          api_group = group_name,
          across(contains("date"), as.Date),
          award_amount = as.numeric(award_amount)
        )
      
      # UI Feedback for Pagination monitoring
      if (nrow(batch) > 100) {
        message(paste("   [Pagination Success]: Retrieved", nrow(batch), "records."))
      }
    }
    
    return(batch)
  })
})

# --- 5. Phase I: Save Raw Ground-Truth ---
final_raw_extract <- raw_award_collection %>%
  distinct(award_id, .keep_all = TRUE)

write_csv(final_raw_extract, path_raw_out)
update_registry(2, "award_list_from_api", "pull_timestamp", max(final_raw_extract$pull_timestamp))

# --- 6. Phase II: Pruning & Curation ---
# Logic: Focus strictly on performance period overlap with the FY24-FY25 window.
# We defer financial filtering to a later analytical stage.
message("\nStarting Performance-Based Pruning...")

award_list_master <- final_raw_extract %>%
  filter(
    # Gate 1: Work was still ongoing after the analysis window opened
    end_date >= analysis_start_date,
    
    # Gate 2: Work began before the analysis window closed
    # (Excludes future awards that haven't entered the performance period yet)
    start_date <= analysis_end_date
  ) %>%
  mutate(
    recipient_name = str_to_upper(recipient_name),
    is_active = if_else(end_date >= Sys.Date(), TRUE, FALSE)
  ) %>%
  arrange(desc(award_amount))

# --- 7. Phase III: Final Persistence & Sync ---
write_csv(award_list_master, path_master_out)

# Update registry for Step 3 (Curation)
update_registry(
  seq_num = 3, 
  csv_name = "award_list_master", 
  col_name = "curation_timestamp", 
  ts = Sys.time()
)

message(paste("Success: Master list curated with", nrow(award_list_master), "records."))
message(paste("Master saved to:", path_master_out))