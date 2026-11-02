# =========================================================================
# Project: AOTH Extraction Map Engine
# Script: visualizer.R (The Controller)
# Standard: Tidyverse; In Situ logic; Sequential Ingestion
# =========================================================================

library(tidyverse)
library(sf)
library(tigris)

# Load modular library components
source("viz_utils.R")
source("viz_styles.R")

# --- STEP 1: LOAD DATA ---
# Logic: Anchored in ground-truth dummy data for prototyping
dummy_data <- tribble(
  ~state_iso, ~cluster_id,   ~cd_id, ~unit_count, ~anchor_lon, ~anchor_lat,
  "PA",       "Philadelphia", "03",          85,         -75.165,     39.952,
  "PA",       "Pittsburgh",   "12",          55,         -79.995,     40.440,
  "PA",       "Allentown",    "07",          25,         -75.471,     40.608,
  "PA",       "Harrisburg",   "10",          35,         -76.886,     40.263,
  "PA",       "Lancaster",    "11",          30,         -76.306,     40.037,
  "AZ",       "Phoenix",      "04",         161,        -112.074,     33.448,
  "AZ",       "Phoenix",      "07",          26,        -112.074,     33.448,
  "AZ",       "Tucson",       "06",          45,        -110.974,     32.222,
  "AZ",       "Flagstaff",    "02",          20,        -111.651,     35.198,
  "WA",       "Seattle",      "07",         120,        -122.332,     47.606,
  "WA",       "Spokane",      "05",          35,        -117.426,     47.658,
  "WA",       "Yakima",       "04",          15,        -120.506,     46.602
)

# --- STEP 2: SELECT STATE & FILTER DATA ---
selected_state <- utils::select.list(choices = sort(unique(dummy_data$state_iso)), title = "Select AOTH State Context")
state_data <- dummy_data %>% dplyr::filter(state_iso == selected_state)

# --- STEP 3: FETCH MAP ---
# Logic: Generates the 'map_stack' with the foundational 1% base_unit
map_stack <- fetch_map(selected_state, base_percent = 0.01)

# --- STEP 4: PROJECT ANCHORS ---
cluster_anchors <- project_anchors(state_data, map_stack$crs)

# --- STEP 5: ALLOCATE HEX UNITS ---
# Logic: Uses in situ multipliers (10x inner / 13x outer) relative to geography
hex_units <- allocate_hex(
  state_df           = state_data,
  map_stack          = map_stack,
  anchors_sf         = cluster_anchors,
  radius_multiplier  = 10,
  annulus_multiplier = 13
)

# --- STEP 6: GENERATE CLUSTER HULLS ---
# Logic: Employs a 0.5x buffer for a snug, professional enclosure
hex_boxes <- melt_hex(hex_units, map_stack = map_stack, buffer_multiplier = 0.5)

# --- STEP 7: RENDER FINAL VISUALIZATION ---
# Logic: Supports modular placement to avoid visual clutter (e.g., "bottom right")
state_map <- render_map(
  map_stack  = map_stack, 
  hex_units  = hex_units, 
  hex_boxes  = hex_boxes,
  legend_pos = "bottom left" 
)

print(state_map)