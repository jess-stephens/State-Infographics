# =========================================================================
# Project: AOTH State Infographics v2
# Script:  config_registry.R
# Path:    /script/config_registry.R
# Version: 2026-05-11-v01
# Purpose: Initialize/Orchestrate the Data Registry Manifest
# =========================================================================


library(tidyverse)
library(fs)


# ===========================================================================
# DOCUMENTATION: config_registry.R
# ===========================================================================
#' REGISTRY SCHEMA DICTIONARY:
#' @field pipeline_sequence [int] Order of operations (0 = Metadata, 1-N = Pipeline).
#' @field dataset_name [chr] Unique key for the dataset (e.g., "account_list_master").
#' @field file_path [chr] Relative path to the physical CSV file.
#' @field responsible_module [chr] The .R script that governs this dataset.
#' @field global_params [json] Project-wide context (dates/agencies) at time of run.
#' @field input_params [json] Input metadata for that specific sequence step.
#' @field output_params [json] Output metadata for that specific sequence step.
#' @field timestamp [dttm] Completion time for that specific sequence step.
#' 
#' WORKFLOW:
#' @workflow Initialization: Sets up the project structure and primary manifest.
#' @workflow Orchestration: Merges schema updates with existing run-metadata.
#' @workflow Auditing: Creates timestamped backups if archiving is selected.
# ===========================================================================


# ----------------------------------------
# --- 1. FUNCTION: initialize_registry ---
# ----------------------------------------

initialize_registry <- function() {
  
  # 1.1. Structural paths
  registry_path <- "data/reference/data_registry.csv"
  archive_dir   <- "data/archive"
  
  # 1.2. Overall schema
  registry_schema <- tribble(
    ~pipeline_sequence, ~dataset_name,               ~file_path,                                     ~responsible_module,
    0,                  "project_metadata",          NA_character_,                                  "data_utils.R",
    1,                  "account_list_from_api",     "data/raw/account_list_from_api.csv",           "manage_accounts.R",
    2,                  "account_list_master",       "data/reference/account_list_master.csv",       "manage_accounts.R",
    3,                  "award_list_from_api",       "data/raw/award_list_from_api.csv",             "manage_awards.R",
    4,                  "award_list_master",         "data/reference/award_list_master.csv",         "manage_awards.R",
    5,                  "transaction_list_from_api", "data/reference/transaction_list_from_api.csv", "manage_transactions.R"
  ) %>%
    mutate(
      global_params = NA_character_, # JSON blob container
      input_params  = NA_character_, # JSON blob container
      output_params  = NA_character_, # JSON blob container
      timestamp     = as.POSIXct(NA)
    )
  
  # 1.3. Define key(s) to join with existing file
  join_keys <- c("pipeline_sequence", "dataset_name")
  
  # 1.4. No Existing file: Build from scratch
  if (!file_exists(registry_path)) {
    dir_create(path_dir(registry_path))
    write_csv(registry_schema, registry_path)
    return(message(">>> No existing file found: Initial registry created from schema."))
  }
  
  # 1.5. Existing file: Archive before sync 
  ts_string <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archive_path <- path(archive_dir, paste0("data_registry_", ts_string, ".csv"))
  dir_create(archive_dir)
  file_copy(registry_path, archive_path)
  message(">>> Existing registry archived to: ", archive_path)
  
  # 1.6. Existing file: Sync with schema
  registry_csv <- read_csv(
    registry_path, 
    col_types = cols(
      pipeline_sequence  = col_integer(),
      dataset_name       = col_character(),
      file_path          = col_character(),
      responsible_module = col_character(),
      global_params      = col_character(), 
      input_params       = col_character(), 
      output_params      = col_character(), 
      timestamp          = col_datetime()   
    )
  )

  schema_sfx <- ".schema"
  csv_sfx    <- ".csv"
  
  # Create the joined dataset for comparison
  joined_data <- registry_schema %>%
    left_join(registry_csv, by = join_keys, suffix = c(schema_sfx, csv_sfx))
  
  # PRE-FLIGHT: Identify which schema columns actually have a CSV history
  schema_cols <- setdiff(names(registry_schema), join_keys)
  matching_cols <- intersect(schema_cols, names(registry_csv))
  
  registry_sync <- joined_data %>%
    # STAGE A: Port existing data for matched columns
    mutate(across(
      .cols = all_of(paste0(matching_cols, schema_sfx)), 
      .fns  = function(x) {
        root_name    <- str_remove(cur_column(), paste0("\\", schema_sfx, "$"))
        csv_col_name <- paste0(root_name, csv_sfx)
        csv_val      <- joined_data[[csv_col_name]]
        coalesce(x, csv_val)
      }, 
      .names = "{str_remove(.col, paste0('\\\\', schema_sfx, '$'))}"
    )) %>%
    # STAGE B: Initialize brand new columns from the schema
    mutate(across(
      .cols = all_of(setdiff(schema_cols, matching_cols)),
      .fns  = ~ .x,
      .names = "{.col}"
    )) %>%
    # Final Cleanup: Order and Select
    select(all_of(names(registry_schema))) %>%
    arrange(across(all_of(join_keys)))
  
  write_csv(registry_sync, registry_path)
  message(">>> New registry synchronized with schema. Previous version safely archived.")
}
  


# ---------------------------------------
# --- 2. EXECUTE: initialize_registry ---
# ---------------------------------------

if (interactive()) initialize_registry()