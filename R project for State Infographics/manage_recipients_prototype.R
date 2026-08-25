# ==============================================================================
# Project: AOTH State Infographics v2
# Module:  manage_recipients.R
# Purpose: Extract, Hydrate, and Assemble Recipient Geodata
# ==============================================================================

library(tidyverse)
library(tidygeocoder)
source("scripts/data_utils.R") # For call_usaspending_api utility

# -----------------------------------------------
# --- FUNCTION 01: INGEST_RECIPIENTS ---
# -----------------------------------------------
ingest_recipients <- function(recipient_hash) {
  
  endpoint <- paste0("/api/v2/recipient/", recipient_hash, "/")
  resp <- call_usaspending_api(endpoint = endpoint, method = "GET")
  
  if (is.null(resp) || length(resp) == 0) {
    message(paste("    !!! No data returned for hash:", recipient_hash))
    return(NULL)
  }
  
  # Data Preservation: Use map to force any NULL/empty elements to NA_character_ 
  if (!is.null(resp$location)) {
    location_data <- resp$location %>%
      purrr::map(~ .x %||% NA_character_) %>% 
      as_tibble() %>% 
      janitor::clean_names() %>%
      rename_with(~paste0("loc_", .x))
  } else {
    message(paste("    --- Warning: No location object for hash:", recipient_hash))
    location_data <- tibble(loc_address_line_1 = NA_character_)
  }
  
  base_data <- tibble(
    recipient_hash = recipient_hash,
    recipient_name = resp$name %||% NA_character_,
    recipient_uei  = resp$uei %||% NA_character_
  )
  
  return(bind_cols(base_data, location_data))
}

# -----------------------------------------------
# --- FUNCTION 02: ASSEMBLE_GEOCODING_STRINGS ---
# -----------------------------------------------
assemble_geocoding_strings <- function(data) {
  
  message(">>> Action: Assembling addresses for ", nrow(data), " records...")
  
  result <- data %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(
      # ZIP+4 Standard: 5-digit zip followed by hyphen and 4-digit extension
      clean_zip = if_else(!is.na(loc_zip4) && loc_zip4 != "", 
                          paste0(loc_zip, "-", loc_zip4), 
                          as.character(loc_zip)),
      
      # Single-String Assembly for ArcGIS: Specific -> General
      full_address = list(
        loc_address_line1,
        loc_address_line2,
        loc_address_line3,
        loc_city_name,
        loc_state_code,
        clean_zip,
        "USA"
      ) %>% 
        purrr::keep(~ !is.na(.x) && .x != "") %>% 
        paste(collapse = ", ") %>%
        stringr::str_squish()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-clean_zip)
  
  message(">>> Success: Address strings assembled.")
  return(result)
}


# -----------------------------------------------
# --- FUNCTION 03: GEOCODE_RECIPIENTS ---
# -----------------------------------------------

#' Execute Geocoding via ArcGIS
#'
#' @description
#' Takes the assembled address strings and retrieves coordinates. 
#' Defaults to ArcGIS as it provides high-resolution domestic results 
#' without requiring an API key for low-volume requests.
#'
#' @param data A tibble containing the 'full_address' column.
#' @return A tibble with added 'lat' and 'long' columns.
#' @export

geocode_recipients <- function(data) {
  
  message(">>> Action: Initiating geocoding for ", nrow(data), " records...")
  
  # Standardized call to tidygeocoder
  # We use the 'arcgis' method for its superior handling of US street addresses
  result <- data %>%
    tidygeocoder::geocode(
      address = full_address,
      method  = 'arcgis',
      lat     = lat,
      long    = lon
    )
  
  # Success check: How many coordinates were actually found?
  match_count <- sum(!is.na(result$lat))
  message(paste0(">>> Status: Successfully geocoded ", match_count, " of ", nrow(data), " records."))
  
  return(result)
}


# -----------------------------------------------
# --- BITE-SIZE TEST RUN ---
# -----------------------------------------------

# --- INTEGRATED PIPELINE TEST ---

# 1. Select Target
target_id <- "71710641-1c0a-d4c7-a593-1c039ee88fa2-C"

# 2. Execute Chain
final_backbone_row <- target_id %>%
  ingest_recipients() %>%
  assemble_geocoding_strings() %>%
  geocode_recipients()

# 3. Final Review
glimpse(final_backbone_row)