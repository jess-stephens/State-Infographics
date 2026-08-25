library(tidyverse)
library(tigris)
library(sf)
library(stringr)
library(pals)
library(ggrepel)

# 1. Get the Shapefile
# cb = TRUE keeps the file size manageable for "Handout Aesthetics"
az_districts <- congressional_districts(state = "AZ", cb = TRUE, year = 2024) %>%
  st_transform(crs = 26912) %>%
  mutate(district_id_clean = str_pad(as.character(as.numeric(CD119FP)), 2, pad = "0"))

# 2. Set the Map Frame
state_bbox <- st_bbox(az_districts)

# 3. Generate the District Centroids
# This is what the loop uses to "pull" hexes into neighborhoods
cd_centroids <- az_districts %>%
  st_centroid()

# 4. Define our three anchors (Moving this here for clarity)
anchors <- list(
  "Phoenix"   = st_sfc(st_point(c(-112.074, 33.448)), crs = 4326),
  "Tucson"    = st_sfc(st_point(c(-110.974, 32.222)), crs = 4326),
  "Flagstaff" = st_sfc(st_point(c(-111.651, 35.198)), crs = 4326)
) %>% 
  map(~st_transform(.x, crs = 26912))

# 1. UPDATED DATA
az_stats <- tibble(
  district = c("04", "07", "03", "01", "02", "06", "07", "02"),
  total_units = c(161, 26, 40, 30, 15, 45, 15, 20),
  cluster = c("Phoenix", "Phoenix", "Phoenix", "Phoenix", "Phoenix", "Tucson", "Tucson", "Flagstaff")
)

all_cluster_hexes <- list()

for (cl_name in unique(az_stats$cluster)) {
  
  # A. Filter data for THIS cluster
  cl_data <- az_stats %>% filter(cluster == cl_name) %>% arrange(total_units)
  cl_anchor <- anchors[[cl_name]]
  
  # B. Generate the Footprint for THIS cluster
  cl_lattice <- st_make_grid(
    st_buffer(cl_anchor, 50000), 
    cellsize = 5000, 
    square = FALSE
  ) %>% 
    st_sf() %>%
    mutate(dist = as.numeric(st_distance(st_centroid(.), cl_anchor))) %>%
    slice_min(dist, n = sum(cl_data$total_units)) %>%
    mutate(temp_id = row_number())
  
  # C. Run Greedy Allocation with "Annulus Intersection" Pull
  pool <- cl_lattice
  assigned_list <- list()
  
  # Create the "Extraction Ring" once per cluster
  inner_ring <- st_buffer(cl_anchor, 50000)
  outer_ring <- st_buffer(cl_anchor, 65000) # Slightly wider for stronger pull
  extraction_ring <- st_difference(outer_ring, inner_ring)
  
  for(i in 1:nrow(cl_data)) {
    curr_cd <- cl_data$district[i]
    curr_n  <- cl_data$total_units[i]
    
    # 1. Find the intersection of the District and the Ring
    ring_intersection <- st_intersection(
      az_districts %>% filter(district_id_clean == curr_cd),
      extraction_ring
    )
    
    # 2. Determine the Pull Point
    # Check if intersection exists AND has geometry
    if(nrow(ring_intersection) > 0 && !any(st_is_empty(ring_intersection))) {
      this_center <- st_centroid(st_union(ring_intersection)) # Union handles multi-part shapes
    } else {
      # Fallback: Pull toward global centroid if no intersection with the ring
      this_center <- cd_centroids %>% filter(district_id_clean == curr_cd)
    }
    
    # 3. Allocation Logic
    if(i < nrow(cl_data)) {
      closest <- pool %>%
        mutate(d = as.numeric(st_distance(., this_center))) %>%
        arrange(d) %>% 
        slice(1:curr_n)
      
      pool <- pool %>% filter(!temp_id %in% closest$temp_id)
    } else {
      closest <- pool
    }
    assigned_list[[i]] <- closest %>% mutate(district_id = curr_cd, cluster_id = cl_name)
  }
  
  # --- CRITICAL FIX: Save the cluster results to the main list ---
  all_cluster_hexes[[cl_name]] <- bind_rows(assigned_list)
}

# Combine everything
az_hexes_final <- bind_rows(all_cluster_hexes)

# Create separate 'containers' for each metro area
az_metro_boxes <- az_hexes_final %>%
  group_by(cluster_id) %>%
  summarize() %>%
  st_buffer(1000)

# Define the active districts for the filter
active_cds <- unique(az_hexes_final$district_id)

# Calculate Label Positions based on the 'Jagged Box' containers
cluster_labels <- az_metro_boxes %>%
  # We iterate through each box to get its specific geometry bounds
  rowwise() %>%
  mutate(
    # Get the bounding box of the dissolved container
    bbox = list(st_bbox(geometry)),
    # Use the max X (East) and the midpoint of Y (Vertical Center)
    x_max = bbox["xmax"],
    y_mid = (bbox["ymin"] + bbox["ymax"]) / 2
  ) %>%
  ungroup() %>%
  mutate(
    # Nudge 15km right of the box edge for a clean "Callout" look
    label_x = x_max + 000, 
    label_y = y_mid
  )

aoth_colors <- c(
  "01" = "#2a4f83", # PEPFAR Navy
  "02" = "#ba0c2f", # USAID Red
  "03" = "#6b4c9a", # Royal Amethyst
  "04" = "#4a5d8a", # Indigo
  "06" = "#8b3a4d", # Muted Burgundy
  "07" = "#7b6a8d", # Dusk Purple
  "08" = "#5c4d7d"  # Deep Violet
)

# 2. Updated ggplot Block
ggplot() +
  # 1. THE BASE: All districts in uniform gray
  geom_sf(data = az_districts, fill = "#f6f6f6", color = "#f6f6f6", linewidth = 0) +
  
  # 2. THE HIGHLIGHT: Only active districts
  geom_sf(data = az_districts %>% filter(district_id_clean %in% active_cds),
          aes(fill = district_id_clean), 
          color = "gray65", linewidth = 0, alpha = 0.15, show.legend = FALSE) +
  
  geom_sf(data = az_districts, fill = "#f6f6f6", alpha = 0, color = "gray35", linewidth = 0.1) +
  geom_sf(data = st_union(az_districts), fill = "#f6f6f6", alpha = 0, color = "gray65", linewidth = 0.5) +
  
  # 3. THE ENCLOSURES: Metro "Jagged Boxes"
  geom_sf(data = az_metro_boxes, fill = "#f6f6f6", color = "#505050", 
          linewidth = 0.2, alpha = 0.85) +
  
  # 4. THE HEXES: Shared fill aesthetic
  geom_sf(data = az_hexes_final, aes(fill = district_id), 
          color = "#f6f6f6", linewidth = 0.2, alpha = 0.65) +
  
  # 5. DYNAMIC LABELS: Repelled from the right edge of metro containers
  geom_text_repel(
    data = cluster_labels,
    aes(x = label_x, y = label_y, label = toupper(cluster_id)),
    size = 3.5,                   # Standard legible size for federal-style reporting
    fontface = "plain",
    family = "Source Sans Pro",
    color = "#333333",
    alpha = 0.65,
    hjust = 0,                  # Anchors the start of the text to our calculated X
    nudge_x = 1000,            # Extra nudge to clear the 1km container buffer
    direction = "y",            # Keeps labels from stacking vertically if clusters shift
    segment.color = "gray70",   # Subtle line connecting label to the box
    segment.size = 0.3,
    min.segment.length = 0,     # Always draw a line for a clear visual link
    box.padding = 0.5,          # Prevents text from crowding the box
    point.padding = 0.5
  ) +
  
  coord_sf(xlim = c(state_bbox["xmin"], state_bbox["xmax"]), 
           ylim = c(state_bbox["ymin"], state_bbox["ymax"]), datum = NA) +
  
  scale_fill_manual(values = aoth_colors) +
  
  theme_void() +
  theme(
    legend.position = "none", 
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 18, family = "Source Sans Pro", hjust = 0.5),
    plot.subtitle = element_text(size = 10, family = "Source Sans Pro", hjust = 0.5, color = "gray30")
  ) +
  labs(title = "ARIZONA: Multi-Hub Funding Extraction",
       subtitle = "Extraction Clusters: Phoenix, Tucson, and Flagstaff")