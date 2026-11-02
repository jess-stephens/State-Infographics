library(tidyverse)

# --- 1. Configuration & Parameters ---
# Centralizing paths to maintain high standards for the mega-repository
params <- list(
  # Raw data pulled from the API (Source of Truth with Metadata)
  raw_file = "data/reference/account_list_from_api.csv",
  
  # Persistent Decision Log (Ground-Truth Assignment)
  master_file = "data/reference/account_list_master.csv"
)

# --- 2. Synchronize and Identify "Discovery" Rows ---
# Load the latest API pull
raw_data <- read_csv(params$raw_file)

if (file.exists(params$master_file)) {
  master_data <- read_csv(params$master_file)
  
  # Identify "New" accounts not yet in the master record (The Delta-Check)
  # We join ONLY on tas_code to prevent schema mismatches from timestamps
  new_accounts <- raw_data %>%
    anti_join(master_data, by = "tas_code")
  
  if (nrow(new_accounts) > 0) {
    message(paste("Detected", nrow(new_accounts), "new accounts from the latest API pull."))
    # Append new accounts to the existing ledger
    df <- bind_rows(master_data, new_accounts)
  } else {
    # If no new accounts, we refresh the metadata (timestamp/filter) for existing rows
    # This ensures your Decision Log reflects the most recent "Ground-Truth" pull
    df <- master_data %>%
      select(-query_filter, -pull_timestamp) %>%
      left_join(raw_data %>% select(tas_code, query_filter, pull_timestamp), by = "tas_code")
  }
} else {
  message("No master file found. Initializing from raw pull.")
  df <- raw_data
}

# --- 3. Clean and Enforce Automatic Inclusion ---
if (!"analysis_status" %in% names(df)) {
  df$analysis_status <- NA_character_
}

df <- df %>%
  mutate(
    # Force 'INCLUDE' for your primary technical anchor: USAID (072)
    analysis_status = case_when(
      agency_code == "072" ~ "INCLUDE",
      is.na(analysis_status) | analysis_status == "" ~ "PENDING",
      TRUE ~ analysis_status
    )
  )

# --- 4. The Interactive Loop (Summary & Iteration) ---
repeat {
  # Summarize current progress for logical oversight
  summary_stats <- df %>% count(analysis_status)
  print(summary_stats)
  
  pending_rows <- which(df$analysis_status == "PENDING")
  
  if (length(pending_rows) == 0) {
    message("All accounts assigned. Sequential ingestion complete.")
    break
  }
  
  # Prompt for manual review of PENDING non-072 accounts
  proceed <- askYesNo(paste("Review", length(pending_rows), "PENDING accounts?"))
  if (!isTRUE(proceed)) break
  
  for (idx in pending_rows) {
    row <- df[idx, ]
    cat(sprintf("\n[%s] %s: %s (Total: %s)", 
                row$agency_code, row$tas_code, row$account_name, 
                format(row$total_obligations, big.mark=",")))
    
    choice <- readline("Action? [i]nclude, [e]xclude, [s]kip: ")
    
    df$analysis_status[idx] <- case_when(
      tolower(choice) == "i" ~ "INCLUDE",
      tolower(choice) == "e" ~ "EXCLUDE",
      TRUE ~ "PENDING"
    )
    
    # Save frequently to preserve your "Ground-Truth" decisions
    write_csv(df, params$master_file)
  }
}

# Final save to confirm the Master Decision Record
write_csv(df, params$master_file)
message(paste("Master Assignment updated at:", params$master_file))