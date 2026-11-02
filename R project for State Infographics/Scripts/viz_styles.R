# --- AOTH ANALYTICAL PLAYBOOK: VIZ_STYLES.R ---
# Purpose: Centralize aesthetic constants and style logic for map engines.
# Guidelines: Tidyverse naming conventions; logical, systematic organization.

# 1. THE STYLE LIST (Single Source of Truth)
aoth_style <- list(
  
  # Colors
  color_base_fill      = "#f6f6f6",    # State background
  color_base_line      = "grey80",    # Congressional District boundaries
  color_hull_line      = "grey40",    # Cluster "melt" enclosures
  color_hex_line       = "#f6f6f6",     # Individual hex unit borders
  color_legend_hex     = "grey80",    # Color for the reference hex
  palette_primary      = "mako",      # Main Viridis scale for districts
  
  # Sizes & Alpha: Precise line weights
  size_base_line       = 0.2,
  size_hull_line       = 0.5,         # Thicker for enclosure visibility
  size_hex_line        = 0.1,
  alpha_hex            = 0.8,
  
  # Line Styles
  hull_linetype        = "dashed",
  
  # The Quantum: Scaling Logic & Labels
  scaling_factor       = 1e6,         # $1 Million divisor
  scaling_label        = "$1 Million",
  unit_floor           = 1            # Every non-zero cluster gets 1 hex
)

# 2. CAPTION GENERATOR
# Logic: Creates a standardized string for map footers to ensure transparency.
generate_scaling_caption <- function(style = aoth_style) {
  paste0("Scaling Factor: One hex unit (⬢) = ", style$scaling_label)
}


# 4. CUSTOM THEME: AOTH MINIMAL
# Logic: Strips graticules and background noise for a "content-first" view.
theme_aoth <- function(style = aoth_style) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid   = ggplot2::element_blank(),
      axis.text    = ggplot2::element_blank(),
      axis.title   = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, color = "grey40", size = 9),
      legend.position = "none"
    )
}