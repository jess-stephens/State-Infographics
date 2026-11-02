# =========================================================================
# Project: AOTH State Infographics v2
# Script:  05_runner_script.R
# Purpose: Reproducible, version-controlled run of the  data refresh
# Usage:   Open the .Rproj, open this file, and Source it (or run it
#          section by section). Do not type pipeline commands into the
#          console by hand; change the settings below and commit instead.
# =========================================================================


# ---------------------------------------------------------------------------
# --- 0. RUN SETTINGS (the only section you should need to edit) -----------
# ---------------------------------------------------------------------------

# Pinned end date for the analysis window. Using a fixed date (instead of
# "today") means re-running this script later asks the API for the same window.
PULL_DATE <- as.Date("2026-11-01")


# ---------------------------------------------------------------------------
# --- 1. SETUP ---------------------------------------------------------------
# ---------------------------------------------------------------------------

# 1.1. Confirm we are in the project root (scripts use relative paths)
stopifnot(
  "Open the .Rproj file so the working directory is the project root." =
    file.exists("R project for State Infographics.Rproj")
)

# 1.2. Load shared utilities
source("Scripts/data_utils.R")

# 1.3. Batch mode: no startup dashboards, no "keep parameters?" prompts
options(aoth.batch = TRUE)

# 1.4. Record the R/package versions used for this run
dir.create("logs", showWarnings = FALSE)
writeLines(
  capture.output(sessionInfo()),
  file.path("logs", paste0("sessioninfo_", PULL_DATE, ".txt"))
)


# ---------------------------------------------------------------------------
# --- 2. PROJECT PARAMETERS (Sequence 0) ------------------------------------
# ---------------------------------------------------------------------------

# force_new = TRUE archives the current registry, then recalculates all
# windows from the pinned PULL_DATE.
run_params <- set_parameters(force_new = TRUE, target_end = PULL_DATE)
print(run_params)


# ---------------------------------------------------------------------------
# --- 3. ACCOUNTS (Sequences 1-2) -------------------------------------------
# ---------------------------------------------------------------------------

source("manage_accounts.R")

# 3.1. Pull accounts from the API
ingest_accounts()

# 3.2. Assign new accounts. USAID (072) is auto-included. You will be
#      prompted for any NEW non-USAID account. Those decisions are saved in
#      data/reference/account_list_master.csv, so commit that file.
assign_accounts()


# ---------------------------------------------------------------------------
# --- 4. AWARDS (Sequences 3-4) ---------------------------------------------
# ---------------------------------------------------------------------------

# Note: this overwrites the `params` object created by manage_accounts.R,
# which is fine because the accounts step is finished.
source("manage_awards.R")

grid_ingest()
prune_awards()


# ---------------------------------------------------------------------------
# --- 5. VERIFY --------------------------------------------------------------
# ---------------------------------------------------------------------------

# Stops with an error if dates, run order, or upstream status are out of sync
stopifnot(verify_pipeline_sync("award_list_master"))

message("Run complete for PULL_DATE = ", PULL_DATE,
        ". Review `git status`, then commit the updated data and logs.")