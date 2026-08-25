# =========================================================================
# Project: AOTH Extraction Map Engine
# Script: viz_utils.R (The Functional Buckets)
# =========================================================================


# --- UTILITY #01: FETCH_MAP ---
# Retrieves data and establishes the fundamental spatial anchor.

fetch_map <- function(state_iso, base_percent = 0.01) {
  
  # 1. Cache Escape
  if (exists("map_stack") && isTRUE(map_stack$state_iso == state_iso)) {
    return(map_stack)
  }
  
  # 2. Acquisition
  raw_sf <- tigris::congressional_districts(state = state_iso, cb = TRUE, year = 2024)
  
  # 3. Projection (UTM Centroid Logic)
  lon_center <- sf::st_coordinates(sf::st_centroid(sf::st_union(raw_sf)))[1]
  utm_zone   <- floor((lon_center + 180) / 6) + 1
  target_crs <- 32600 + utm_zone 
  
  projected_sf <- raw_sf %>% 
    sf::st_transform(crs = target_crs) %>%
    dplyr::mutate(cd_id = stringr::str_pad(as.character(as.numeric(CD119FP)), 2, pad = "0"))
  sf::st_agr(projected_sf) <- "constant"    #Silence warnings
  
  # 4. Fundamental Spatial Anchor
  bbox      <- sf::st_bbox(projected_sf)
  base_unit <- min(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin) * base_percent
  
  # 5. Assemble Minimal map_stack (Implicit Return)
  list(
    state_iso = state_iso,
    crs       = target_crs,
    sf        = projected_sf,
    base_unit = base_unit
  )
}


# --- UTILITY #02: PROJECT_ANCHORS ---

project_anchors <- function(state_df, target_crs) {
  
  state_df %>%
    dplyr::distinct(cluster_id, anchor_lon, anchor_lat) %>%
    sf::st_as_sf(coords = c("anchor_lon", "anchor_lat"), crs = 4326) %>%
    sf::st_transform(crs = target_crs) %>% 
    `st_agr<-`("constant")    #Silence warnings
  
}


# --- UTILITY #03: MAKE_LATTICE ---
# Generates a hexagonal grid where the capture radius is clearly 10x the base unit.

make_lattice <- function(anchor_sf, n_required, map_stack, radius_multiplier = 10) {
  
  # Multiplier is calculated in situ for transparency
  inner_radius <- map_stack$base_unit * radius_multiplier
  
  sf::st_make_grid(
    sf::st_buffer(anchor_sf, inner_radius), 
    cellsize = map_stack$base_unit, 
    square = FALSE
  ) %>% 
    sf::st_sf() %>% 
    `st_agr<-`("constant") %>%    #Silence warnings
    dplyr::mutate(
      dist = as.numeric(sf::st_distance(sf::st_centroid(.), anchor_sf)),
      temp_id = dplyr::row_number()
    ) %>%
    dplyr::slice_min(dist, n = n_required)
}


# --- UTILITY #04: FIND_MAGNETS ---
# Identifies centroid "magnets" within an extraction ring (10x to 13x base_unit).

find_magnets <- function(cd_ids, map_stack, anchor_sf, radius_multiplier = 10, annulus_multiplier = 13) {
  
  # 1. Define Extraction Ring (Annulus) logic in situ
  inner_radius <- map_stack$base_unit * radius_multiplier
  outer_radius <- map_stack$base_unit * annulus_multiplier
  
  inner_ring <- sf::st_buffer(anchor_sf, dist = inner_radius)
  outer_ring <- sf::st_buffer(anchor_sf, dist = outer_radius)
  annulus <- sf::st_difference(outer_ring, inner_ring) %>% 
    `st_agr<-`("constant")    #Silence warnings
  
  # 2. Batch calculate Magnets for each District
  cd_ids %>%
    purrr::set_names() %>%
    purrr::map(function(curr_cd) {
      target_cd <- map_stack$sf %>% dplyr::filter(cd_id == !!curr_cd)
      ring_intersection <- sf::st_intersection(target_cd, annulus) %>% 
        `st_agr<-`("constant")    #Silence warnings
      
      # Determine magnet point based on intersection outcome
      if(nrow(ring_intersection) > 0 && !any(sf::st_is_empty(ring_intersection))) {
        sf::st_centroid(sf::st_union(ring_intersection))
      } else {
        sf::st_centroid(target_cd)
      }
    })
}


# --- UTILITY #05: ALLOCATE_HEX ---
# Orchestrates the assignment of hex units to districts based on cluster gravity.

allocate_hex <- function(state_df, map_stack, anchors_sf, 
                         radius_multiplier = 10, annulus_multiplier = 13) {
  
  all_cluster_hexes <- list()
  
  # Step 1: Group data for unit counts per district
  cluster_stats <- state_df %>%
    dplyr::group_by(cluster_id, cd_id) %>%
    dplyr::summarize(total_units = sum(unit_count), .groups = "drop")
  
  # --- PRIMARY LOOP: Iterate through each CLUSTER (Anchor Level) ---
  for (this_cluster in unique(cluster_stats$cluster_id)) {
    
    this_cluster_stats <- cluster_stats %>% 
      dplyr::filter(cluster_id == this_cluster) %>% 
      dplyr::arrange(total_units)
    
    # ANCHOR: The static projected point center for this cluster
    this_anchor <- anchors_sf %>% dplyr::filter(cluster_id == this_cluster)
    
    # Generate the hexagonal footprint lattice around the Anchor
    hex_lattice <- make_lattice(
      anchor_sf = this_anchor, 
      n_required = sum(this_cluster_stats$total_units),
      map_stack = map_stack,
      radius_multiplier = radius_multiplier
    )
    
    # MAGNET LOOKUP: Centralized batch operation for the cluster
    magnet_lookup <- find_magnets(
      cd_ids = this_cluster_stats$cd_id, 
      map_stack = map_stack, 
      anchor_sf = this_anchor,
      radius_multiplier = radius_multiplier,
      annulus_multiplier = annulus_multiplier
    )
    
    pool <- hex_lattice
    assigned_list <- list()
    
    # --- SECONDARY LOOP: Iterate through each DISTRICT (Assignment Level) ---
    for(i in 1:nrow(this_cluster_stats)) {
      curr_cd <- this_cluster_stats$cd_id[i]
      curr_n  <- this_cluster_stats$total_units[i]
      
      this_magnet <- magnet_lookup[[curr_cd]]
      
      if(i < nrow(this_cluster_stats)) {
        closest <- pool %>%
          dplyr::mutate(d = as.numeric(sf::st_distance(., this_magnet))) %>%
          dplyr::arrange(d) %>% 
          dplyr::slice(1:curr_n)
        
        pool <- pool %>% dplyr::filter(!temp_id %in% closest$temp_id)
      } else {
        closest <- pool
      }
      
      assigned_list[[i]] <- closest %>% 
        dplyr::mutate(cd_id = curr_cd, cluster_id = this_cluster)
    }
    
    all_cluster_hexes[[this_cluster]] <- dplyr::bind_rows(assigned_list)
  }
  
  final_hex_set <- dplyr::bind_rows(all_cluster_hexes)
  sf::st_agr(final_hex_set) <- "constant"   #Silence warnings
  final_hex_set
}


# --- UTILITY #06: MELT_HEX ---
# Union all hex units by cluster and buffer to create a cohesive boundary.

melt_hex <- function(hex_sf, map_stack, buffer_multiplier = 0.5) {
  
  # Step 1: Define the enclosure buffer (Default: half of a base_unit)
  hull_buffer <- map_stack$base_unit * buffer_multiplier
  
  # Step 2: Dissolve and expand
  hex_sf %>%
    dplyr::group_by(cluster_id) %>%
    dplyr::summarize(geometry = sf::st_union(geometry), .groups = "drop") %>%
    sf::st_buffer(dist = hull_buffer) %>%
    sf::st_make_valid() %>%
    `st_agr<-`("constant")    #Silence warnings
  
}


# --- UTILITY #07: MAKE_HEX_LEGEND ---
# Modular placement based on directional strings. 

make_hex_legend <- function(map_stack, 
                            scaling_label = "$1 Million", 
                            placement = "bottom left") {
  
  bbox <- sf::st_bbox(map_stack$sf)
  
  # 1. Determine X-Coordinate (Modular Left/Right)
  # Uses the 10x offset for padding and accounts for the 15x runway on the right
  x_coord <- if (grepl("left", placement)) {
    bbox$xmin + (map_stack$base_unit * 10)
  } else {
    bbox$xmax - (map_stack$base_unit * 25)
  }
  
  # 2. Determine Y-Coordinate (Modular Top/Bottom)
  # Uses 5x padding for bottom and 10x for top to avoid header overlap
  y_coord <- if (grepl("bottom", placement)) {
    bbox$ymin + (map_stack$base_unit * 5)
  } else {
    bbox$ymax - (map_stack$base_unit * 10)
  }
  
  # 3. Position the Anchor Point and Generate Hex
  ref_point <- sf::st_sfc(
    sf::st_point(c(x_coord, y_coord)), 
    crs = map_stack$crs
  )
  
  sf::st_make_grid(ref_point, cellsize = map_stack$base_unit, square = FALSE)[1] %>%
    sf::st_sf() %>%
    dplyr::mutate(
      label = scaling_label,
      runway_end = sf::st_bbox(.)$xmax + (map_stack$base_unit * 15)
    )
}


# --- UTILITY #08: RENDER_MAP ---
# A modular, content-focused plotting engine utilizing the AOTH style guide.

render_map <- function(map_stack, hex_units, hex_boxes, style = aoth_style, legend_pos = "bottom left") {
  
  # 1. Generate the modular reference hex
  reference_hex <- make_hex_legend(map_stack, placement = legend_pos)
  
  ggplot2::ggplot() +
    # Layer 1: Basemap (Congressional Districts)
    ggplot2::geom_sf(
      data  = map_stack$sf, 
      fill  = style$color_base_fill, 
      color = style$color_base_line, 
      size  = style$size_base_line
    ) +
    
    # Layer 2: Cluster Hulls (The "Melted" Enclosures)
    ggplot2::geom_sf(
      data     = hex_boxes, 
      fill     = NA, 
      color    = style$color_hull_line, 
      linetype = style$hull_linetype, 
      size     = style$size_hull_line
    ) +
    
    # Layer 3: Allocated Hex Units
    ggplot2::geom_sf(
      data  = hex_units, 
      aes(fill = cd_id), 
      color = style$color_hex_line, 
      size  = style$size_hex_line,
      alpha = style$alpha_hex
    ) +
    
    # Layer 4: Physical Scale Legend (Reference Hex)
    ggplot2::geom_sf(
      data  = reference_hex, 
      fill  = style$color_legend_hex, 
      color = style$color_hex_line
    ) +
    
    # Layer 5: Reference Text Label (Using logical runway)
    # Replaces hardcoded nudge with map_stack proportional logic
    ggplot2::geom_text(
      data = reference_hex,
      aes(x = runway_end, y = sf::st_coordinates(sf::st_centroid(geometry))[,2], label = label),
      hjust = 1, # Aligns text to the end of the 15x runway
      size  = 3,
      color = "grey40"
    ) +
    
    # Aesthetics & Metadata
    ggplot2::scale_fill_viridis_d(option = style$palette_primary) +
    ggplot2::coord_sf(datum = NA) +
    theme_aoth(style) +
    ggplot2::labs(
      title    = paste("AOTH Extraction Map:", map_stack$state_iso),
      subtitle = "Systematic Allocation by Congressional District",
      fill     = "District",
      caption  = generate_scaling_caption(style)
    )
}