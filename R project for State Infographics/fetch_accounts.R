library(tidyverse)
library(janitor)

source("scripts/data_utils.R")

# --- 1. Configuration & Parameters ---
# Centralizing all logic for high standards and easy modification
params <- list(
  # (1) Date Range for the FY23-FY26 Analysis
  start_date = "2022-10-01",
  end_date   = "2026-09-30",
  
  # (1) Agency Parameters: Longform for API, Shortform for Metadata record
  agencies = list(
    list(api_name = "Agency for International Development", short_name = "USAID"),
    list(api_name = "Department of State", short_name = "State")
  ),
  
  # (2) Output Filename Parameterization
  output_filename = "data/reference/account_list_from_api.csv"
)

# --- 2. Helper Logic for API/Metadata Consistency ---
# Construct the agency list for the API body
api_agency_filter <- map(params$agencies, ~list(
  type = "awarding", 
  tier = "toptier", 
  name = .x$api_name
))

# Construct the metadata string for the CSV record
metadata_label <- paste0(
  "Agencies: ", paste(map_chr(params$agencies, "short_name"), collapse = "|"), 
  "; Range: ", params$start_date, " to ", params$end_date
)

# --- 3. The "Robust" Engine ---
# --- 3. The Extraction Engine ---
# Now significantly leaner; logic is strictly analytical unnesting
get_all_accounts <- function(agency_filter, meta_label, start, end) {
  
  # Modular handshake now handles the entire while() loop internally
  full_data <- call_usaspending_api(
    endpoint = "/api/v2/search/spending_by_category/federal_account/",
    query_body = list(
      filters = list(
        time_period = list(list(start_date = start, end_date = end)),
        agencies = agency_filter
      ),
      limit = 100
    )
  )
  
  # Analytical reshaping stays in the script
  full_data %>%
    select(
      tas_code = code,
      account_name = name,
      total_obligations = amount,
      pull_timestamp
    ) %>%
    separate(tas_code, into = c("agency_code", "account_code"), sep = "-", remove = FALSE) %>%
    mutate(query_filter = meta_label)
}

# --- 4. Execution & File Generation ---
master_account_ledger <- get_all_accounts(
  api_agency_filter, 
  metadata_label, 
  params$start_date, 
  params$end_date
)

write_csv(master_account_ledger, params$output_filename)
message(paste("Success. File saved to:", params$output_filename))