# =========================================================================
# Project: AOTH State Infographics v2
# Script:  test_handshake_iterative.R
# Purpose: Iterative group-based API calls with automated discovery dates
# =========================================================================

library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)
library(lubridate)

# --- 1. Dynamic Parameter Setup (Intent to Execution) ---
analysis_start_date  <- as.Date("2023-10-01") 
analysis_end_date    <- as.Date("2025-09-30")

# Discovery start is 1 year prior; end is current FY end
discovery_start_date <- analysis_start_date - years(1)
current_date         <- Sys.Date()
fy_end_year          <- if (month(current_date) >= 10) year(current_date) + 1 else year(current_date)
discovery_end_date   <- as.Date(paste0(fy_end_year, "-09-30"))

params <- list(
  discovery_start = as.character(discovery_start_date),
  discovery_end   = as.character(discovery_end_date),
  # Grouped types to satisfy the 422 group constraint
  award_groups    = list(
    contracts = c("A", "B", "C", "D"),
    grants    = c("02", "03", "04", "05")
  )
)

# --- 2. Iterative Group Pull Logic ---
target_url <- "https://api.usaspending.gov/api/v2/search/spending_by_award/"

message(paste0("Discovery Window: ", params$discovery_start, " to ", params$discovery_end))

all_test_results <- purrr::map_dfr(names(params$award_groups), function(group_name) {
  
  message(paste("Initiating handshake for group:", group_name))
  
  query_body <- list(
    filters = list(
      time_period = list(
        list(start_date = params$discovery_start, end_date = params$discovery_end)
      ),
      treasury_account_components = list(
        list(aid = "072", main = "1037") # USAID Primary
      ),
      recipient_scope = "domestic",
      award_type_codes = params$award_groups[[group_name]]
    ),
    fields = c("Award ID", "Recipient Name", "Recipient UEI", "Award Amount", "Start Date", "End Date"),
    limit = 10,
    page = 1
  )
  
  response <- httr::POST(
    url = target_url, 
    body = jsonlite::toJSON(query_body, auto_unbox = TRUE),
    httr::add_headers(`Content-Type` = "application/json"), 
    httr::timeout(60)
  )
  
  if (httr::status_code(response) == 200) {
    res_content <- httr::content(response, as = "text", encoding = "UTF-8")
    batch <- as_tibble(jsonlite::fromJSON(res_content)$results) %>%
      janitor::clean_names() %>%
      mutate(api_group = group_name)
    return(batch)
  } else {
    warning(paste("Group", group_name, "failed with status", httr::status_code(response)))
    return(NULL)
  }
})

# --- 3. Verify Combined Results ---
if (nrow(all_test_results) > 0) {
  message("\nSuccess! Iterative handshakes complete.")
  print(all_test_results %>% count(api_group))
  print(head(all_test_results))
} else {
  stop("All API handshakes failed.")
}