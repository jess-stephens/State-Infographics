# =========================================================================
# Project: AOTH State Infographics v2
# Script:  data_utils.R
# Path:    /script/data_utils.R
# Version: 2026-05-06-v04
# Purpose: Modular functions for data pipeline orchestration
# =========================================================================


library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)
library(rlang)


# ===========================================================================
# DOCUMENTATION: data_utils.R
# ===========================================================================
#' MODULE INVENTORY:
#' 1. get_registry_config: The 'Reader'—Extracts manifest rows.
#' 2. set_parameters: The 'DNA-Maker'—Calculates 1:1 comparative windows.
#' 3. call_usaspending_api: The 'Ingester'—Handles paginated API retrieval.
#' 4. update_registry: The 'Writer'—Commits metadata to the central ledger.
#' 5. verify_pipeline_sync: The 'Validator'—Checks for analytical drift.
#' 6. clean_up: Removes all objects from global environment unless excluded.
#' 
#' DESIGN PRINCIPLE:
#' This module enforces a "Shared Truth" model. No data is ingested without 
#' being anchored to a specific Project DNA (time window/parameters).
# ===========================================================================


# ----------------------------------------
# --- UTILITY #01: GET_REGISTRY_CONFIG ---
# ----------------------------------------

#' Retrieve Configuration from the Data Registry
#'
#' @description
#' Accesses the central data registry to retrieve metadata, file paths, and 
#' parameter blobs for a specific dataset or the global project state.
#'
#' @param d_name String. The unique name of the dataset. Defaults to 
#' "project_metadata" (Sequence 0).
#'
#' @return A named list containing the registry row attributes (file_path, 
#' responsible_module, global_params, etc.).
#' @export
get_registry_config <- function(d_name = "project_metadata") {
  reg_path <- "data/reference/data_registry.csv"
  if (!file.exists(reg_path)) stop("Registry manifest not found. Run config_registry.R first.")
  
  conf <- readr::read_csv(reg_path, show_col_types = FALSE) %>%
    dplyr::filter(dataset_name == d_name)
  
  if (nrow(conf) == 0) stop(paste("Dataset", d_name, "not found in manifest."))
  
  return(as.list(conf))
}


# -----------------------------------
# --- UTILITY #02: SET_PARAMETERS ---
# -----------------------------------

#' Initialize or Retrieve Global Project Parameters
#'
#' @description
#' Manages the "Project DNA" stored in the registry's project_metadata row. 
#' It calculates the 1:1 comparative windows required for the AOTH v2 
#' framework or resumes from the existing registry state.
#'
#' @param force_new Logical. If TRUE, bypasses existing registry data and 
#' triggers a fresh parameter calculation. Defaults to FALSE.
#'
#' @details
#' Implements the AOTH comparative math: Target Window (Post-Jan 20, 2025) 
#' and a 1:1 duration baseline (Comparison Window).
#' This logic ensures that 'Current Administration' spending is always 
#' measured against an equivalent number of days from the 'Prior 
#' Administration'.
#'
#' @return A named list (params) containing target and comparison date ranges.
#' @export

set_parameters <- function(force_new = FALSE) {
  
  reg_conf <- get_registry_config()
  existing_dna <- reg_conf$global_params
  has_dna <- !is.na(existing_dna) && existing_dna != "NA"
  
  if (has_dna && !force_new) {
    message("Status: Existing project metadata found in Registry.")
    params <- jsonlite::fromJSON(existing_dna)
    
    confirm <- utils::askYesNo(paste0(
      "Current Window: ", params$target_start, " to ", params$target_end, 
      "\nKeep these parameters?"
    ))
    
    if (isTRUE(confirm)) return(params)
  }
  
  message("Action: Defining new analytical parameters...")
  t_start <- as.Date("2025-01-20") 
  t_end   <- Sys.Date()
  
  duration   <- as.numeric(t_end - t_start)
  comp_start <- t_start - duration
  comp_end   <- t_start - 1
  
  params <- list(
    target_start     = t_start,
    target_end       = t_end,
    comparison_start = comp_start,
    comparison_end   = comp_end,
    analysis_start   = comp_start,
    analysis_end     = t_end
  )
  
  update_registry(
    d_name   = "project_metadata",
    mod_name = "data_utils.R",
    ts       = Sys.time(),
    global_p = params
  )
  
  message("Success: Project DNA committed to Registry (Sequence 0).")
  return(params)
}


# -----------------------------------------
# --- UTILITY #03: CALL_USASPENDING_API ---
# -----------------------------------------

#' Standardized Wrapper for USAspending API Calls
#'
#' @description
#' Executes paginated API requests with a 5-retry safety standard and exponential backoff.
#' Includes a "Salvage" mechanism to return partial data if the API hangs or fails.
#'
#' @param endpoint String. The API endpoint.
#' @param query_body List. The JSON-ready list for POST payloads.
#' @param method String. HTTP method ("POST" or "GET").
#' @param base_url String. The API root URL.
#'
#' @return A consolidated tibble containing results from all retrieved pages (or partial results on failure).
#' @export

call_usaspending_api <- function(endpoint, 
                                 query_body = NULL, 
                                 method = "POST", 
                                 base_url = "https://api.usaspending.gov") {
  
  target_url <- paste0(base_url, endpoint)
  all_batches <- list()
  current_page <- 1
  has_more <- TRUE
  
  while (has_more) {
    if (toupper(method) == "POST" && !is.null(query_body)) {
      query_body$page <- current_page
    }
    
    attempt <- 1
    response <- NULL
    
    while (attempt <= 5) {
      response <- tryCatch({
        if (toupper(method) == "POST") {
          httr::POST(url = target_url, 
                     body = jsonlite::toJSON(query_body, auto_unbox = TRUE),
                     httr::add_headers(`Content-Type` = "application/json"), 
                     httr::timeout(60)) # Reduced to 60s for "fail-fast" salvage
        } else {
          httr::GET(url = target_url, httr::timeout(60))
        }
      }, error = function(e) { NULL })
      
      # SUCCESS: Break the retry loop
      if (!is.null(response) && httr::status_code(response) == 200) break
      
      # FAILURE: Exponential Backoff
      wait_time <- 2 ^ attempt
      message(paste("    !!! API warning: Attempt", attempt, "failed on Page", current_page, ". Retrying in", wait_time, "s..."))
      
      attempt <- attempt + 1
      Sys.sleep(wait_time)
    }
    
    # --- SALVAGE LOGIC: REPLACES ORIGINAL stop() ---
    if (is.null(response) || httr::status_code(response) != 200) {
      message(paste("    !!! SALVAGE OP: API failed at Page", current_page, "after 5 attempts."))
      message("    !!! Returning collected batches to maintain pipeline momentum.")
      
      # Break the 'while(has_more)' loop entirely to return what we have
      break 
    }
    # -----------------------------------------------
    
    raw_content <- httr::content(response, as = "text", encoding = "UTF-8")
    page_json <- jsonlite::fromJSON(raw_content)
    
    batch_data <- as_tibble(page_json$results) %>%
      janitor::clean_names() %>%
      dplyr::mutate(pull_timestamp = Sys.time())
    
    all_batches[[current_page]] <- batch_data
    
    # Check for more pages
    meta <- page_json$page_metadata
    results_count <- nrow(batch_data)
    limit_val <- query_body$limit %||% 100 
    
    has_more <- (
      (!is.null(meta$hasNext) && isTRUE(meta$hasNext)) || 
        (!is.null(meta$has_next_page) && isTRUE(meta$has_next_page)) ||
        (results_count == limit_val)
    )
    
    if (toupper(method) == "POST" && has_more) {
      current_page <- current_page + 1
      message(paste("    ... [Sync]: Advancing to Page", current_page))
      Sys.sleep(1) # Breath between successful calls
    } else {
      has_more <- FALSE
    }
  } 
  
  # Bind and return whatever was successfully stored in the list
  return(bind_rows(all_batches))
}


# ------------------------------------
# --- UTILITY #04: UPDATE_REGISTRY ---
# ------------------------------------

#' Commit Metadata to the Data Registry
#'
#' @description
#' Updates the central ledger for a specific dataset/module pair. Stores 
#' project-wide and step-specific parameters as JSON blobs to ensure 
#' pipeline auditability[cite: 1].
#'
#' @param d_name String. The dataset_name defined in the registry tribble[cite: 1].
#' @param mod_name String. The filename of the responsible module (e.g., "manage_accounts.R")[cite: 1].
#' @param ts POSIXct. The execution timestamp.
#' @param global_p List. The global project parameters (DNA).
#' @param local_p List (Optional). Metadata unique to this specific run.
#'
#' @return Invisibly returns NULL. Updates data/reference/data_registry.csv.
#' @export

update_registry <- function(d_name, mod_name, ts, global_p, local_p = NULL) {
  registry_path <- "data/reference/data_registry.csv"
  
  if (!file.exists(registry_path)) {
    stop("Registry manifest not found. Please run config_registry.R first.")
  }
  
  # Explicitly cast to character to strip the <json> class attribute #
  g_json <- as.character(jsonlite::toJSON(global_p, auto_unbox = TRUE))
  l_json <- if (!is.null(local_p)) {
    as.character(jsonlite::toJSON(local_p, auto_unbox = TRUE)) 
  } else {
    NA_character_
  }
  
  registry <- readr::read_csv(
    registry_path, 
    show_col_types = FALSE,
    col_types = cols(
      global_params = col_character(), # Force character regardless of NA content
      local_params = col_character(),
      timestamp = col_datetime()       # Ensure timestamp is treated as POSIXct
    )
  )
  
  registry <- registry %>%
    mutate(
      # Both sides of the if_else are now guaranteed to be <character> #
      global_params = if_else(dataset_name == d_name & responsible_module == mod_name, g_json, global_params),
      local_params  = if_else(dataset_name == d_name & responsible_module == mod_name, l_json, local_params),
      timestamp     = if_else(dataset_name == d_name & responsible_module == mod_name, ts, timestamp)
    )
  
  readr::write_csv(registry, registry_path)
}


# -----------------------------------------
# --- UTILITY #05: VERIFY_PIPELINE_SYNC ---
# -----------------------------------------

#' Validate Pipeline Synchronization
#'
#' @description
#' Performs a "Manifest Handshake." Compares the Project DNA (Sequence 0) 
#' against the metadata stored for the current module to identify 
#' analytical drift.
#'
#' @param d_name String. The dataset_name to verify.
#'
#' @return Boolean. TRUE if synced; FALSE (with a warning) if out of date[cite: 1].
#' @export
#' @details
#' If the Project DNA has changed (e.g., today's date has advanced), 
#' downstream modules must be re-run to ensure the comparative analysis 
#' remains valid across all datasets.

verify_pipeline_sync <- function(d_name) {
  message(paste0("Check: Verifying synchronization for ", d_name, "..."))
  
  project_conf <- get_registry_config()
  module_conf <- get_registry_config(d_name)
  
  if (is.na(module_conf$global_params)) {
    message("Status: New dataset detected. Synchronization is pending first run.")
    return(TRUE)
  }
  
  is_synced <- identical(project_conf$global_params, module_conf$global_params)
  
  if (is_synced) {
    message("Success: Module is synchronized with Project DNA.")
    return(TRUE)
  } else {
    warning(paste0(
      "\n!!! SYNC ERROR: ", d_name, " is out of date !!!",
      "\nProject Window: ", project_conf$global_params,
      "\nModule Window:  ", module_conf$global_params,
      "\nAction: Re-run the responsible module: ", module_conf$responsible_module
    ))
    return(FALSE)
  }
}


# -----------------------------
# --- UTILITY #06: CLEAN_UP ---
# -----------------------------

#' Defensive Environment Purge
#'
#' @description
#' Executes a "Clean Sweep" of the Global Environment by default. 
#' Allows for specific objects to be preserved via the keep argument.
#' To be executed on exit from a module.
#'
#' @param keep Character vector (Optional). Names of objects to exclude from the purge.
#'
#' @return Null. Clears .GlobalEnv.
#' @export

clean_up <- function(keep = NULL) {

  # Identify all current objects
  all_objects <- ls(envir = .GlobalEnv)
  
  # Determine targets (Everything minus the Keep list)
  to_remove <- setdiff(all_objects, keep)
  
  # Execute Purge
  if (length(to_remove) > 0) {
    rm(list = to_remove, envir = .GlobalEnv)
  }
  
  # Visual reset
  cat("\014") 
  
  message("Restored to pristine environment.")
}