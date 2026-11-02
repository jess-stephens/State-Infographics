# =========================================================================
# Project: AOTH State Infographics v2
# Script:  data_utils.R
# Path:    /script/data_utils.R
# Version: 2026-05-14-v01
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
#' Section A - Registry Primitives
#' 1. get_registry_config: The 'Reader'—Extracts manifest rows. Read one row 
#' from the registry.
#' 2. pluck_registry_json: dig into a JSON column within that row.
#' 3. update_registry: The 'Writer'—Commits metadata to the central ledger.
#' writes results back to the registry.
#' 4. archive_cvs: back up a file before overwriting it. 
#' Section B - Project Parameters
#' 5. set_parameters: The 'DNA-Maker'—Calculates 1:1 comparative windows. 
#' Calculate or retrieve the project's date windows.
#' Section C - Pipeline Validation
#' 6. verify_pipeline_sync: The 'Validator'—Checks for analytical drift.
#' Check dates/order/completeness across the registry.
#' Section D - API Transport Layer
#' 7. try_api: Wraps provider-specific API calls in error-handling logic.
#' Retry wrapper for any API call.
#' 8. fedspender: Handles calls to any usaspending API endpoint.
#' The actual USAspending request-builder.
#' Section E - Response Processing
#' 9. unbundle_data: Extracts data payload from API transport packet.
#' Extract the data table from an API response. 
#' 10. unbundle_metadata: Extract the status/audit info from an API response.
#' 11. clean_up: Removes all objects from global environment unless excluded. 
#' Reset the environment at the end of a script. 
#' 
#' DESIGN PRINCIPLE:
#' This module enforces a "Shared Truth" model. No data is ingested without 
#' being anchored to a specific Project DNA (time window/parameters).
# ===========================================================================

# ---------------------------------------------------------------------------
# --- Section A — Registry Primitives -----------------------------------
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# --- UTILITY_01: [ get_registry_config ] -----------------------------------
# ---------------------------------------------------------------------------

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
  
  return(conf)
}


# ---------------------------------------------------------------------------
# --- UTILITY_02: [ pluck_registry_json ] -----------------------------------
# ---------------------------------------------------------------------------

#' Pluck Nested Values from Registry JSON Columns
#'
#' @description
#' A surgical extraction utility that retrieves a specific record from the 
#' data registry and "tunnels" into a JSON-formatted parameter column to 
#' return a nested value.
#'
#' @param d_name String. The unique dataset name used to filter the registry.
#' @param json_col String. The column containing the JSON blob (e.g., "input_params").
#' @param json_nest Character Vector. The ordered path of keys to traverse 
#'   (e.g., c("data_counts", "output_count")). Defaults to NULL to return full blob.
#'
#' @return The extracted value from the nested path. Returns \code{NULL} if the 
#'   dataset is not found, the JSON is empty, or the path is invalid.
#'
#' @details 
#' This function leverages \code{get_registry_config()} for row retrieval and 
#' \code{purrr::pluck()} for safe path traversal. It enforces a strict 
#' integrity check, ensuring only one source record is processed.
#'
#' @examples
#' \dontrun{
#'   pluck_registry_json("api_raw", "output_params", c("data_counts", "output_count"))
#' }
#' 
#' @export

pluck_registry_json <- function(d_name, json_col, json_nest = NULL) {
  
  # 1. Fetch the data (as tibble)
  registry_row <- get_registry_config(d_name)
  
  # 2. Ensure we are plucking from exactly ONE source record.
  if (nrow(registry_row) != 1) return(NULL)
  
  # 3. Pull the JSON blob (1 column in 1 row)
  json_blob <- registry_row %>% dplyr::pull(!!rlang::sym(json_col))
  
  # 4. Defensive check: Exit early if the JSON blob is empty
  if (is.na(json_blob) || json_blob == "") return(NULL)
  
  # 5. Parse and Pluck
  # Note: If json_nest is NULL, pluck returns the full parsed JSON blob
  json_blob %>%
    jsonlite::fromJSON() %>%
    purrr::pluck(!!!json_nest, .default = NULL)
}


# ---------------------------------------------------------------------------
# --- UTILITY_03: [ update_registry ] ---------------------------------------
# ---------------------------------------------------------------------------

#' Commit Metadata to the Data Registry
#'
#' @description
#' Updates the central ledger for a specific dataset/module pair. Stores 
#' project-wide and step-specific parameters as JSON blobs to ensure 
#' pipeline auditability and state-awareness.
#'
#' @param d_name String. The unique \code{dataset_name} defined in the registry.
#' @param mod_name String. The filename of the responsible module (e.g., "data_utils.R").
#' @param ts POSIXct. The execution timestamp for the registry entry.
#' @param global_p List. The global project parameters (Project DNA).
#' @param input_p List (Optional). Input metadata or filters for this specific run.
#' @param output_p List (Optional). Output metadata or counts from this specific run.
#'
#' @return Logical. \code{TRUE} if the file exists after the write operation; 
#'    \code{FALSE} otherwise.
#' @export

update_registry <- function(d_name, mod_name, ts, global_p, input_p = NULL, output_p = NULL) {
  registry_path <- "data/reference/data_registry.csv"
  
  if (!file.exists(registry_path)) {
    stop("Registry manifest not found. Please run config_registry.R first.")
  }
  
  # Explicitly cast to character to strip the <json> class attribute #
  global_json <- as.character(jsonlite::toJSON(global_p, auto_unbox = TRUE))
  input_json <- if (!is.null(input_p)) {
    as.character(jsonlite::toJSON(input_p, auto_unbox = TRUE)) 
  } else {
    NA_character_
  }
  output_json <- if (!is.null(output_p)) {
    as.character(jsonlite::toJSON(output_p, auto_unbox = TRUE)) 
  } else {
    NA_character_
  }
  
  registry <- readr::read_csv(
    registry_path, 
    show_col_types = FALSE,
    col_types = cols(
      global_params = col_character(), # Force character regardless of NA content
      input_params = col_character(),
      output_params = col_character(),
      timestamp = col_datetime()       # Ensure timestamp is treated as POSIXct
    )
  )
  
  registry <- registry %>%
    mutate(
      # Both sides of the if_else are now guaranteed to be <character> #
      global_params = if_else(dataset_name == d_name & responsible_module == mod_name, global_json, global_params),
      input_params  = if_else(dataset_name == d_name & responsible_module == mod_name, input_json, input_params),
      output_params  = if_else(dataset_name == d_name & responsible_module == mod_name, output_json, output_params),
      timestamp     = if_else(dataset_name == d_name & responsible_module == mod_name, ts, timestamp)
    )
  
  readr::write_csv(registry, registry_path)
  return(file.exists(registry_path))
}


# ---------------------------------------------------------------------------
# --- UTILITY_04: [ archive_csv ] ---------------------------------------
# ---------------------------------------------------------------------------

#' Archive Dataset Prior to Overwrite
#'
#' @description
#' Moves a prior version of a file to a flat archive directory. Designed as 
#' a temporary safe haven to allow for validation before final commit.
#'
#' @param file_path String. The path to the file to be archived.
#'    Aligns with file_path in data_registry.csv.
#' @param archive_dir String. The target directory for archived files. 
#'    Defaults to "data/archive".
#'
#' @return String. The path to the archived file on success; NULL if the 
#'    file doesn't exist or archiving fails.
#' @export

archive_csv <- function(file_path, archive_dir = "data/archive") {
  
  # 1. Defensive check for NULL or missing file_path
  if (is.null(file_path) || file_path == "" || !fs::file_exists(file_path)) {
    message(">>> [Archive]: No valid file found to archive. Skipping.")
    return(NULL)
  }
  
  # 2. Defensive check for malformed archive_dir
  if (is.null(archive_dir) || archive_dir == "") {
    warning(">>> [Archive]: archive_dir was NULL or empty. Reverting to 'data/archive'.")
    archive_dir <- "data/archive"
  }
  
  # 3. Ensure the Archive Safe Haven exists
  if (!fs::dir_exists(archive_dir)) fs::dir_create(archive_dir)
  
  # 4. Generate Timestamped Filename
  file_name <- fs::path_file(file_path)
  file_ext  <- fs::path_ext(file_path)
  file_base <- fs::path_ext_remove(file_name)
  timestamp  <- format(fs::file_info(file_path)$modification_time, "%Y%m%d_%H%M%S")
  
  archive_name <- paste0(file_base, "_", timestamp, ".", file_ext)
  archive_path <- fs::path(archive_dir, archive_name)
  
  # 5. Execute Copy
  archive_result <- tryCatch({
    fs::file_copy(file_path, archive_path, overwrite = TRUE)
    message(paste(">>> [Archive]: File archived to", archive_path))
    return(as.character(archive_path)) 
  }, error = function(e) {
    warning(paste(">>> [Archive Error]:", e$message))
    return(NULL)
  })
  
  return(archive_result)
}

# ---------------------------------------------------------------------------
# --- Section B - Project Parameters -----------------------------------
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# --- UTILITY_05: [ set_parameters ] ----------------------------------------
# ---------------------------------------------------------------------------

#' Initialize or Retrieve Global Project Parameters
#'
#' @description
#' Manages the "Project DNA" stored in the registry's project_metadata row. 
#' It calculates the 1:1 comparative windows required for the AOTH v2 
#' framework or resumes from the existing registry state.
#' 
#' @details 
#' This function leverages \code{pluck_registry_json()} for state-awareness, 
#' \code{archive_csv()} for registry protection during fresh initialization, 
#' and \code{update_registry()} for persistence.
#'
#' @param force_new Logical. If TRUE, bypasses existing registry data  
#'    and triggers a fresh parameter calculation and registry archive. 
#'    Defaults to FALSE.
#'
#' @return A named list (params) containing target and comparison date ranges.
#' @export

set_parameters <- function(force_new = FALSE,
                           target_end = Sys.Date(),
                           ask = interactive() && !isTRUE(getOption("aoth.batch"))) {
  
  # 1. Check for existing global_params
  # NULL or NA values in the registry will result in is.list() being FALSE
  
  existing_params <- pluck_registry_json(d_name = "project_metadata", 
                                      json_col  = "global_params", 
                                      json_nest = NULL)
  
  if (is.list(existing_params) && !force_new) {
    message(">>> [Status]: Existing project metadata found in Registry.")
    confirm <- if (ask) {
      utils::askYesNo(paste0(
        "Current Window: ", existing_params$target_start, " to ", existing_params$target_end, 
        "\nKeep these parameters?"
      ))
    } else TRUE
    if (isTRUE(confirm)) return(existing_params)
  }
  
  # 2. Archive registry before overwriting global_params
  # This triggers if we are forcing a reset or if it's the very first run
  
  if (force_new || !is.list(existing_params)) {
    archive_csv("data/reference/data_registry.csv")
  }
  
  # 3. Define new global_params
  
  message("[Action]: Defining new analytical parameters...")
  
  # 3.1. Define the target period for funding cuts
  t_start    <- as.Date("2025-01-20") 
  t_end      <- as.Date(target_end)
  t_duration <- as.numeric(t_end - t_start)
  
  # 3.2. Define the comparison period before funding cuts
  comp_start <- t_start - t_duration
  comp_end   <- t_start - 1
  
  # 3.3. Define the full analytical window
  anal_start <- comp_start
  anal_end   <- t_end
  anal_window     <- as.numeric(anal_end - anal_start)
  
  # 3.4. Define the broader discovery window for award search
  disc_start <- anal_start - anal_window
  disc_end   <- t_end
  
  # 3.5. Assemble full global_params schema
  params <- list(
    target_start     = as.character(t_start),
    target_end       = as.character(t_end),
    comparison_start = as.character(comp_start),
    comparison_end   = as.character(comp_end),
    analysis_start   = as.character(anal_start),
    analysis_end     = as.character(anal_end),
    discovery_start  = as.character(disc_start),
    discovery_end    = as.character(disc_end)
  )
  
  # 4. Commit to registry
  
  update_success <- update_registry(
    d_name   = "project_metadata",
    mod_name = "data_utils.R",
    ts       = Sys.time(),
    global_p = params
  )
  
  if (update_success == TRUE) {
  message("[Success]: Project parameters committed to registry (Sequence 0).")
  return(params)
  } else {
    stop("[CRITICAL ERROR]: Failed to commit project parameters to registry. Parameters not set.")
  }
}

# ---------------------------------------------------------------------------
# --- Section C — Pipeline Validation -----------------------------------
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# --- UTILITY_06: [ verify_pipeline_sync ] ----------------------------------
# ---------------------------------------------------------------------------

#' Validate Metadata, Temporal Lineage, and Upstream Integrity
#'
#' @description
#' Performs a multi-path validation of the data registry to prevent analytical 
#' drift. It ensures the current execution is anchored to the correct 
#' "Project DNA" and follows a linear, successful chronological sequence.
#'
#' @details 
#' The function executes three primary checks:
#' 1. **Metadata Sync**: Ensures `global_params` match Sequence 0.
#' 2. **Temporal Sync**: Ensures execution timestamps are strictly ascending.
#' 3. **Integrity Sync**: Confirms all upstream sequences report a "complete" status.
#'
#' @param d_name String. The unique \code{dataset_name} to verify.
#'
#' @return Logical. \code{TRUE} if all synchronization checks pass; \code{FALSE} 
#'    (with descriptive warnings) if drift or breaks are detected.
#' @export

#' Validate metadata and temporal sync
#'
#' @description
#' Performs a dual-path validation:
#' 1. Metadata: Ensures the current step's global_params match Sequence 0.
#' 2. Temporal: Ensures the execution timestamps follow a strict, 
#'    ascending order aligned with the pipeline_sequence.
#'
#' @param d_name String. The dataset_name to verify.
#'
#' @return Boolean. TRUE if strictly synchronized; FALSE (with warning) if not.
#' @export

verify_pipeline_sync <- function(d_name) {
  
  # Import registry
  registry_path <- "data/reference/data_registry.csv"
  registry <- readr::read_csv(registry_path, show_col_types = FALSE) %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp))
  
  # Verify current sequence >> stop or continue
  current_sequence <- registry %>% 
    dplyr::filter(dataset_name == d_name)
  if (nrow(current_sequence) == 0) stop(paste("Dataset", d_name, "not found."))
  
  # Filter for preceding pipeline from 0 up to and including current sequence
  pipeline_inclusive <- registry %>%
    dplyr::filter(pipeline_sequence <= current_sequence$pipeline_sequence[1]) %>%
    dplyr::arrange(pipeline_sequence)
  
  # Filter for preceding pipeline from 1 up to but excluding current sequence
  pipeline_exclusive <- registry %>%
    dplyr::filter(pipeline_sequence > 0 & pipeline_sequence < current_sequence$pipeline_sequence[1]) %>%
    dplyr::arrange(pipeline_sequence)
  
  # Verify metadata >> exit or continue
  metadata_count <- unique(pipeline_inclusive$global_params) %>% 
    length()
  metadata_sync <- metadata_count == 1
  
  if (metadata_sync == FALSE) {
    warning(paste0(
      "\n!!! GLOBAL METADATA MISMATCH: Analytical drift detected !!!",
      "\nMultiple parameter sets found across the pipeline history.",
      "\nAction: Re-run the pipeline to re-align with the Project Mandate."
    ))
    return(FALSE)
  }
  
  # Verify temporal sync >> exit or continue
  pipeline_inclusive <- pipeline_inclusive %>%  
    dplyr::mutate(
      prev_timestamp = dplyr::lag(timestamp),
      temporal_sync = timestamp >= prev_timestamp
    )
  
  if (any(pipeline_inclusive$temporal_sync == FALSE, na.rm = TRUE)) {
    # Isolate the first break point for the warning message
    sync_break <- pipeline_inclusive %>% 
      dplyr::filter(temporal_sync == FALSE) %>% 
      dplyr::slice(1)
    
    warning(paste0(
      "\n!!! TOTAL ORDER BREAK: Sequence is out of sync !!!",
      "\nBreak detected at: ", sync_break$dataset_name, " (Seq ", sync_break$pipeline_sequence, ")",
      "\nReason: Timeline is non-linear. Preceding items must have earlier timestamps.",
      "\nAction: Re-run from Sequence ", sync_break$pipeline_sequence, " to restore order."
    ))
    return(FALSE)
  }
  
  # Define function to extract and evaluate data_status from output_params
  check_status <- function(json_str) {
    if (is.na(json_str) || json_str == "NA") return("missing")
    json_list <- jsonlite::fromJSON(json_str)
    return(json_list$data_status %||% "incomplete")
  }
  
  # Verify dataset integrity >> exit or continue
  pipeline_exclusive <- pipeline_exclusive %>% 
    dplyr::mutate(
      data_integrity = purrr::map_chr(output_params, check_status)
    )
  integrity_sync <- all(pipeline_exclusive$data_integrity == "complete")
  
  if (integrity_sync == FALSE && nrow(pipeline_exclusive) > 0) {
    integrity_break <- pipeline_exclusive %>% 
      dplyr::filter(data_integrity != "complete") %>% 
      dplyr::slice(1)
    
    warning(paste0(
      "\n!!! INTEGRITY BREAK: Upstream data is incomplete !!!",
      "\nSource: ", integrity_break$dataset_name, " (Seq ", integrity_break$pipeline_sequence, ")",
      "\nStatus: ", integrity_break$data_integrity,
      "\nAction: Ensure upstream modules finish successfully before proceeding."
    ))
    return(FALSE)
  }
  
  # Return success if all sync checks are passed
  message("Success: DNA, Lineage, and Integrity verified.")
  return(TRUE)
}

# ---------------------------------------------------------------------------
# --- Section D — API Transport Layer -----------------------------------
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# --- UTILITY_07: [ try_api ] -----------------------------------------------
# ---------------------------------------------------------------------------

#' Execute API Calls with Exponential Backoff and State Awareness
#'
#' @param api_function The provider-specific function (adapter) to execute.
#' @param ... Arguments passed directly to the api_function.
#' @param retries Integer. Maximum number of attempts. Default is 3.
#' @param retry_pause Numeric. The base for exponential backoff (pause^attempt). Default is 2.
#' @param context List. Arbitrary metadata for state tracking (e.g., TAS, Group).
#'
#' @return A list containing:
#'   $data: The primary payload (dataframe or list).
#'   $data_status: "full", "partial", or "none".
#'   $error_msg: Character string of the final error encountered.
#'   $attempts: Integer count of attempts made.
#'   $context: The identity metadata passed in.
#' @export

try_api <- function(api_function, ..., retries = 3, retry_pause = 2, context = list()) {
  
  attempt <- 1
  
  while (attempt <= retries) {
    response <- tryCatch(
      
      {
        # Execute the API function
        api_results <- api_function(...)
        
        # Package response with API results
        list(
          data        = api_results$data, 
          data_status = api_results$data_status, 
          error_msg   = api_results$error_msg, 
          attempts    = attempt,
          context     = context
        )
      },
      
      # Package response on API hang/error
      error = function(e) {
        list(
          data        = NULL, 
          data_status = "none", 
          error_msg   = as.character(e$message), 
          attempts    = attempt,
          context     = context
        )
      }
      
    )
    
    # Exit early on full success
    if (response$data_status == "full") return(response)
    
    # If partial or none, log and wait before retry
    message(sprintf(">>> [Transport] Attempt %d: Status is '%s'.", attempt, response$data_status))
    if (attempt < retries) Sys.sleep(retry_pause^attempt)
    
    attempt <- attempt + 1
  }
  
  return(response)
}


# ---------------------------------------------------------------------------
# --- UTILITY_08: [ fedspender ]  -------------------------------------------
# ---------------------------------------------------------------------------

#' Robust utility for calls to any USAspending API endpoint
#'
#' @description
#' Executes paginated POST searches or single-shot GET requests using a 
#' "Smart Template" system. This adapter allows developers to copy-paste 
#' endpoint paths directly from official API contracts while maintaining 
#' automated state awareness and pagination accumulation.
#'
#' @param endpoint   String. The API path template (e.g., "/api/v2/recipient/{recipient_id}/{?year}").
#' @param params     List. Universal container for path, query, or body parameters.
#' @param method     String. HTTP method ("POST" or "GET"). Defaults to "POST".
#' @param base_url   String. The API root URL. Defaults to "https://api.usaspending.gov".
#' @param timeout    Integer. Seconds to wait before timing out. Defaults to 60.
#' @param page_pause Numeric. Seconds to sleep between pagination pages. Defaults to 1.
#'
#' @return A standardized list (Packet) for transport via \code{try_api()}:
#' \itemize{
#'   \item \strong{data}: A consolidated tibble (POST) or list (GET).
#'   \item \strong{data_status}: String; "full", "partial", or "none".
#'   \item \strong{error_msg}: Character; contains the error trace if the call fails.
#' }
#'
#' @details
#' \strong{Smart Template Logic:}
#' \itemize{
#'   \item \code{{variable}}: Path placeholders are interpolated using matching keys in \code{params}.
#'   \item \code{{?variable}}: Documentation-style query placeholders are stripped and delegated to \code{httr}.
#' }
#' \strong{Note:} Parameters used in the URL path are automatically filtered out of 
#' the POST body to ensure JSON payload hygiene.
#'
#' @export
#' @family API Utilities
#' @examples
#' \dontrun{
#' # GET Example
#' fedspender("/api/v2/recipient/{recipient_id}/", list(recipient_id = "123"), method = "GET")
#' 
#' # POST Example
#' fedspender("/api/v2/search/spending_by_award/", list(filters = list(...)))
#' }

fedspender <- function(endpoint, 
                       params     = list(), 
                       method     = "POST", 
                       base_url   = "https://api.usaspending.gov",
                       timeout    = 60,
                       page_pause = 1) {
  
  # 1. Process placeholders in endpoint: { }, {? } ---
  
  # 1.1. Keep track of used params to keep the POST body/GET query clean
  used_params <- c()
  
  # 1.2. Interpolate path placeholders: {variable}
  for (p_name in names(params)) {
    path_placeholder <- paste0("\\{", p_name, "\\}")
    if (grepl(path_placeholder, endpoint)) {
      endpoint <- gsub(path_placeholder, params[[p_name]], endpoint)
      used_params <- c(used_params, p_name)
    }
  }
  
  # 1.3. Strip query placeholders: {?variable}
  endpoint <- gsub("\\{\\?[a-zA-Z0-9_]+\\}", "", endpoint)
  
  # 1.4. Assemble final URL for use in API call
  target_url <- paste0(base_url, endpoint)
  target_url <- gsub("(?<!:)/{2,}", "/", target_url, perl = TRUE) 
  
  # 1.5 Prepare for API call(s) that will accumulate paginated responses
  unused_params <- params[!(names(params) %in% used_params)]
  all_batches  <- list()
  current_page <- 1
  status       <- "full"
  error_msg    <- NA_character_
  
  # 2. GET method with single response
  if (toupper(method) == "GET") {
    res <- tryCatch({
      resp <- httr::GET(url = target_url, query = unused_params, httr::timeout(timeout))
      if (httr::status_code(resp) != 200) stop(paste("HTTP Error:", httr::status_code(resp)))
      
      data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
      list(data = data, data_status = "full", error_msg = NA_character_)
    }, error = function(e) {
      list(data = NULL, data_status = "none", error_msg = as.character(e$message))
    })
    
    return(list(data = res$data, data_status = res$data_status, error_msg = res$error_msg))
  }
  
  # 3. POST method with paginated response
  repeat {
    unused_params$page <- current_page
    
    message(sprintf(">>> [Fedspender]: Requesting page %d for endpoint %s...", current_page, endpoint))
    
    page_res <- tryCatch({
      resp <- httr::POST(
        url = target_url, 
        body = jsonlite::toJSON(unused_params, auto_unbox = TRUE),
        httr::add_headers(`Content-Type` = "application/json"),
        httr::timeout(timeout)
      )
      
      if (httr::status_code(resp) != 200) stop(paste("HTTP Error:", httr::status_code(resp)))
      
      raw_content <- httr::content(resp, as = "text", encoding = "UTF-8")
      page_json   <- jsonlite::fromJSON(raw_content)
      
      batch_data <- tibble::as_tibble(page_json$results %||% list()) %>%
        janitor::clean_names() %>%
        dplyr::mutate(pull_timestamp = Sys.time())
      
      message(sprintf(">>> [Fedspender]: Success. Retrieved %d records from page %d.", nrow(batch_data), current_page))
      
      list(data = batch_data, json = page_json, success = TRUE)
      
    }, error = function(e) {
      list(data = NULL, success = FALSE, msg = as.character(e$message))
    })
    
    if (page_res$success) {
      all_batches[[current_page]] <- page_res$data
      
      # 3.1 Pagination check
      meta      <- page_res$json$page_metadata
      limit_val <- unused_params$limit %||% 100
      
      has_more <- (
        (!is.null(meta$hasNext) && isTRUE(meta$hasNext)) || 
          (!is.null(meta$has_next_page) && isTRUE(meta$has_next_page)) ||
          (nrow(page_res$data) == limit_val)
      )
      
      if (has_more) {
        current_page <- current_page + 1
        Sys.sleep(page_pause) 
      } else { break }
    } else {
      status <- "partial"; error_msg <- page_res$msg; break
    }
  }
  
  return(list(
    data        = dplyr::bind_rows(all_batches),
    data_status = if(length(all_batches) == 0) "none" else status,
    error_msg   = error_msg
  ))
}

# ---------------------------------------------------------------------------
# --- Section E — Response Processing -----------------------------------
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# --- UTILITY_09: [ unbundle_data ] -----------------------------------------
# ---------------------------------------------------------------------------

#' Extract Data Payload from API Transport Bundle
#'
#' @description
#' A universal utility that extracts the $data element from either a single 
#' API transport packet or a collection (list) of packets. It normalizes the 
#' nesting depth to ensure downstream pipes receive a valid, bound tibble 
#' regardless of the input shape.
#'
#' @param api_bundle List. A single packet from try_api() or a collection 
#' of packets from iterative runs (e.g., via purrr::map).
#'
#' @return A tibble. If input contains singletons (nested lists), they are 
#' hoisted into a single-row tibble. If input is NULL or empty, returns an 
#' empty tibble.
#' 
#' @export
#' @details
#' This function adheres to the "Ground-truth" principle by preserving 
#' deep nesting in list-columns rather than dropping complex payloads. 
#' It utilizes a "Normalization Gate" to automatically detect if the 
#' input is a single list (packet) or a list of lists (collection).

unbundle_data <- function(api_bundle) {
  
  # 1. Defensive check for null content
  if (is.null(api_bundle)) {
    warning(">>> [Unbundle]: Received NULL input. Returning empty tibble.")
    return(tibble::tibble())
  }
  
  # 2. Normalize to ensure a single "packet" is structured as a "collection"
  # uses top-level data_status as proxy for determining packet vs. collection
  if (!is.null(api_bundle$data_status)) {
    api_collection <- list(api_bundle)
  } else {
    api_collection <- api_bundle
  }
  
  # 3. Map and pluck from individual packets in collection
  purrr::map_dfr(api_collection, function(api_packet) {
    
    # Extract payload
    data_payload <- api_packet$data
    
    # Defensive check for null content
    if (is.null(data_payload)) return(tibble::tibble())
    
    # Handle singleton vs. table
    if (is.data.frame(data_payload)) {
      return(tibble::as_tibble(data_payload))
    } else if (is.list(data_payload)) {
      # This handles GET singletons: converts list to 1-row tibble
      # We wrap in list() to ensure as_tibble treats it as a single row
      return(tibble::as_tibble(list(data_payload)))
    } else {
      # Fallback for unexpected types
      return(tibble::tibble())
    }
  })
}



# ---------------------------------------------------------------------------
# --- UTILITY_10: [ unbundle_metadata ] -------------------------------------
# ---------------------------------------------------------------------------

#' Unbundle Metadata from API Transport Bundle
#' 
#' @description 
#' Standardizes the "Audit Trail" by plucking status and iteration context 
#' from one or more API transport packets. This is a mechanical necessity 
#' for identifying the "First Break" in a contiguous success chain.
#' 
#' @param api_bundle List. A single packet from try_api() or a collection 
#' of packets from iterative runs.
#' @param iteration_ids Character vector. The keys (e.g., "tas_code", "group") 
#' to extract from the packet's context to maintain traceability. 
#' Defaults to NULL.
#' 
#' @return A tibble with columns: \code{data_status}, \code{last_page}, 
#' \code{has_data}, and \code{iteration_context} (a list-column).
#' 
#' @export

unbundle_metadata <- function(api_bundle, iteration_ids = NULL) {
  
  # 1. Defensive check for null content
  if (is.null(api_bundle)) return(tibble::tibble())
  
  # 2. Normalize to ensure a single "packet" is structured as a "collection"
  # uses top-level data_status as proxy for determining packet vs. collection  
  if (!is.null(api_bundle$data_status)) {
    api_collection <- list(api_bundle)
  } else {
    api_collection <- api_bundle
  }
  
  # 3. Map and pluck individual packets in collection 
  purrr::map_df(api_collection, function(api_packet) {
    
    id_list <- if (is.null(iteration_ids)) {
      list() 
    } else {
      api_packet$context[iteration_ids]
    }
    
    tibble::tibble(
      data_status       = api_packet$data_status,
      last_page         = as.character(api_packet$last_page),
      has_data          = nrow(api_packet$data) > 0,
      iteration_context = list(id_list),
      error_message     = if (is.null(api_packet$error_msg)) {
        NA_character_ 
      } else { 
        as.character(api_packet$error_msg) 
      }
    )
  })
}


# ---------------------------------------------------------------------------
# --- Section F — Housekeeping -----------------------------------
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# --- UTILITY_1: [ clean_up ] ----------------------------------------------
# ---------------------------------------------------------------------------

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

