# =========================================================================
# Project: AOTH State Infographics v2
# Script:  manage_awards.R
# Path:    /manage_awards.R
# Version: 2026-05-13-v01
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


# ---------------------------------------------------------------------------
# --- 1. CONFIGURATION & PARAMETERS -----------------------------------------
# ---------------------------------------------------------------------------

## 1.1. Extract Global Parameters
params_from_registry <- set_parameters()

## 1.2. Unified Parameter Object
params <- list(
  
  # 1.2.1. Time periods for easy reference in module
  target_start     = as.Date(params_from_registry$target_start),
  target_end       = as.Date(params_from_registry$target_end),
  comparison_start = as.Date(params_from_registry$comparison_start),
  comparison_end   = as.Date(params_from_registry$comparison_end),
  analysis_start   = as.Date(params_from_registry$analysis_start),
  analysis_end     = as.Date(params_from_registry$analysis_end),
  discovery_start  = as.Date(params_from_registry$discovery_start),
  discovery_end    = as.Date(params_from_registry$discovery_end)
)
  
  # 1.2.2. Derived filter for API ingestion and registry logging
  params$api_template <- list(
    time_period = list(list(
      start_date = as.character(params$discovery_start),
      end_date   = as.character(params$discovery_end)
    )),
    recipient_scope = "domestic",
    fields = c("Award ID", "Recipient Name", "Recipient UEI", "recipient_id",
               "Award Amount", "Start Date", "End Date"),
    limit = 100
  )
  
  # 1.2.3. Segregated groups of award types required for individual API runs
  params$award_groups <- list(
    contracts = c("A", "B", "C", "D"),
    grants    = c("02", "03", "04", "05")
  )
  
  # 1.2.4. Structural paths
  params$paths <- list(
    api_raw  = "data/raw/award_list_from_api.csv",
    master   = "data/reference/award_list_master.csv",
    accounts = "data/reference/account_list_master.csv"
  )
  
  # 1.2.5. Registry keys for sequence updates
  params$datasets <- list(
    api_raw = "award_list_from_api",
    master  = "award_list_master"
  )
  params$mod_names <- list(
    api_raw = "manage_awards.R",
    master  = "manage_awards.R"
  )
  
  # 1.2.6. Global parameter reference for registry consistency
  params$params_from_registry <- params_from_registry



# ---------------------------------------------------------------------------
# --- 2. FUNCTION_01: [ ingest_awards ] -------------------------------------
# ------ API run for individual context/query parameters -------------------
# ---------------------------------------------------------------------------

  ingest_awards <- function(tas_code, group_name, target_accounts, params) {
    
    # 2.1. Filter details for specific TAS
    account_row <- target_accounts %>% filter(tas_code == !!tas_code)
    
    # 2.2. Structure API query for this run
    api_query <- list(
      filters = list(
        time_period = params$api_template$time_period,
        treasury_account_components = list(list(
          aid = account_row$agency_code, 
          main = account_row$account_code
        )),
        recipient_scope = params$api_template$recipient_scope,
        award_type_codes = params$award_groups[[group_name]]
      ),
      fields = params$api_template$fields,
      limit  = params$api_template$limit, 
      sort   = "Award ID", 
      order  = "asc"
    )
    
    # 2.3 Capture context for this run
    api_context = list(
      tas_code   = tas_code,
      group_name = group_name
    )
    
    # 2.4. Execute API run and capture context in results
    return(try_api(
      api_function = fedspender,
      endpoint     = "/api/v2/search/spending_by_award/",
      params       = api_query,
      method       = "POST",
      context      = api_context,
      timeout      = 60,
      page_pause   = 1
    ))
  }
  
  
# ---------------------------------------------------------------------------
# --- 3. FUNCTION_02: [ grid_ingest ] ---------------------------------------
# ------ API -> award_list_master.csv ---------------------------------------
# ---------------------------------------------------------------------------
  
  grid_ingest <- function() {
  
  # 3.1. Verify pipeline dependencies
  message("Action: Verifying Account Master synchronization...")
  if(!file_exists(params$paths$accounts)) {
    stop("Account Master not found. Run manage_accounts.R first.")
    }
  target_accounts <- read_csv(params$paths$accounts, show_col_types = FALSE) %>%
    filter(analysis_status == "INCLUDE")
  if(nrow(target_accounts) == 0) {
    stop("No accounts assigned as 'INCLUDE'. Run manage_accounts.R to assign.")
    }
  
  # 3.2. Generate TAS hash for the registry
  target_tas_list <- sort(target_accounts$tas_code)
  params$tas_hash <- rlang::hash(target_tas_list)
  
  # 3.3  Prepare the grid for iteration
  iteration_grid <- tidyr::expand_grid(
    tas   = target_tas_list,
    group = names(params$award_groups)
  )
  
  # 3.4. Iterate API extraction by TAS and award type:
  message(sprintf("Executing API extraction: %d runs across %d accounts...", 
                  nrow(iteration_grid), length(target_tas_list)))
  api_collection <- purrr::map2(
    .x = iteration_grid$tas, 
    .y = iteration_grid$group, 
    .f = ~ ingest_awards(
      tas_code = .x, 
      group_name = .y, 
      target_accounts = target_accounts, 
      params = params)
  )
  
  # 3.5. Unbundle & deduplicate data
  api_data <- unbundle_data(api_bundle = api_collection) %>% 
    clean_names() %>% 
    mutate(
      across(contains("date"), ~ lubridate::as_date(.x)), 
      across(contains("amount"), ~ as.numeric(.x))
    ) %>% 
    distinct(award_id, .keep_all = TRUE)
  
  # 3.6. Unbundle & process metadata

  # 3.6.1. Initialize output_params list (with defaults) within params object
  params$output_params <- list(
    data_status   = "partial",
    data_counts   = list(output_count = nrow(api_data)),
    exit_context  = list(),
    last_page     = NA_character_,
    error_message = NA_character_
  )
  
  # 3.6.2. Unbundle & morph metadata
  # Promoting 'none' to 'complete' if no error was reported (a "clean zero")
  api_metadata <- unbundle_metadata(api_bundle = api_collection) %>%
    mutate(
      row_id = row_number(),
      adj_data_status = if_else(
        data_status == "none" & (is.na(error_message) | error_message == ""), 
        "complete", 
        data_status
      )
    )
  
  # 3.6.3. Assign overall data_status
  # If the run resulted in only one unique status (all complete or all none), 
  # use that; otherwise, label as partial progress.
  unique_data_status <- unique(api_metadata$adj_data_status)
  params$output_params$data_status <- if (length(unique_data_status) == 1) {
    unique_data_status 
  } else { 
    "partial" 
  }
  
  # 3.6.4. Locate row with the first failure (for error_message)
  # Defaults to the last row if the entire run was a success
  fail_idx <- c(which(api_metadata$adj_data_status != "complete"), nrow(api_metadata))
  break_row       <- slice(api_metadata, min(fail_idx))
  
  # 3.6.5. Locate row the last success (for exit_criteria & last_page)
  # Defaults to the first row if the entire run was a failure
  success_idx     <- c(1, which(api_metadata$adj_data_status != "none" & 
                                  row_number() <= min(fail_idx)))
  exit_row        <- slice(api_metadata, max(success_idx))
  
  # 3.6.6. Final output_param assignments
  params$output_params$exit_context  <- exit_row$iteration_context[[1]]
  params$output_params$last_page     <- exit_row$last_page
  params$output_params$error_message <- break_row$error_message
  
  # 3.7.  Write to file
  write_csv(api_data, params$paths$api_raw)
  
  # 3.8. Update registry 
  update_registry(
    d_name   = params$datasets$api_raw,
    mod_name = params$mod_names$api_raw,
    ts       = Sys.time(),
    global_p = params$params_from_registry,
    input_p  = list(
      static_query = params$api_template,
      dynamic_hash = params$tas_hash
      ),
    output_p = params$output_params
  )
  message(paste("Success: API transfer packet saved with", nrow(api_data), "records."))
}


# ---------------------------------------------------------------------------
# --- 4. FUNCTION_02: [ prune_awards ] --------------------------------------
# ------ award_list_from_api.csv -> award_list_master.csv -------------------
# ---------------------------------------------------------------------------

prune_awards <- function() {
  if(!file_exists(params$paths$api_raw)) {
    return(message("Error: Run Ingest (3) first."))
    }
  
  message("Action: Pruning awards based on performance overlap...")
  raw_data <- read_csv(params$paths$api_raw, show_col_types = FALSE)
  
  master_awards <- raw_data %>%
    filter(
      end_date >= params$analysis_start,
      start_date <= params$analysis_end
    ) %>%
    mutate(
      recipient_name = str_to_upper(recipient_name)
    ) %>%
    arrange(desc(award_amount))
  
  write_csv(master_awards, params$paths$master)
  
  update_registry(
    d_name   = params$datasets$master,
    mod_name = params$mod_names$master,
    ts       = Sys.time(),
    global_p = params$params_from_registry,
    input_p  = list(
      input_dataset = params$datasets$api_raw,
      input_count   = nrow(raw_data)
    ),
    output_p = list(
      data_status = "complete",
      data_counts = list(
        output_count  = nrow(master_awards),
        dropped_count = nrow(raw_data) - nrow(master_awards)
      )
    )
  )
  message(paste("Success: Master Award list curated at:", params$paths$master))
}


# ---------------------------------------------------------------------------
# --- 5. FUNCTION_03: [ run_award_manager ] ---------------------------------
# ------ Dashboard controller -----------------------------------------------
# ---------------------------------------------------------------------------

run_award_manager <- function() {
  verify_pipeline_sync(params$datasets$api_raw)
  
  while (TRUE) {
    # Extract counts from registry if they exist
    s3_award_count <- pluck_registry_json(
      params$datasets$api_raw, "output_params", c("data_counts", "output_count")
    ) %||% "0"
    
    s4_award_count <- pluck_registry_json(
      params$datasets$master, "output_params", c("data_counts", "output_count")
    ) %||% "0"
    
    cat("\n==========================================")
    cat("\n  AOTH AWARD MANAGEMENT: PIPELINE v2")
    cat("\n==========================================")
    cat(paste("\nAnalytical Window: ", params$analysis_start, " to ", params$analysis_end))
    cat(paste("\n[Seq 3] API Pull:  ", s3_award_count, " awards"))
    cat(paste("\n[Seq 4] Master:    ", s4_award_count, " awards (pruned for window)"))
    cat("\n------------------------------------------")
    cat("\n1. EXECUTE FULL SEQUENCE (Fetch & Prune)")
    cat("\n2. EXIT")
    
    choice <- readline("\nSelect an option (1-2): ")
    
    if (choice == "1") {
      grid_ingest()
      prune_awards()
      message("\nPipeline Complete: Transfer packet saved and Master list curated.")
    } 
    else if (choice == "2") {
      clean_up()
      break
    }
  }
}


# ---------------------------------------------------------------------------
# --- 6. STANDALONE EXECUTION -----------------------------------------------
# ---------------------------------------------------------------------------

  if (interactive() && !isTRUE(getOption("aoth.batch"))) run_award_manager()