#
# Configuration for SES/income index and green space.
#
# Used by:
#   build_ses_index.R  - ses$labels, ses$sd_multipliers (income status categories in output)
#   process_green.R    - green$thresholds (Limited/Good/Great counts), green$icon_* (markers)
#   create_bivariate_map.R - uses its own bivariate palette; does not use this config
#
# Edit this file to change index categories, colours, icons, or green-space thresholds.
#

map_config <- list(

  # --- SES (socioeconomic status) ---
  # Order: [1] = Lowest ... [5] = Highest. Same order for colours, labels, and emoji (for popups).
  # Colours: Lowest=red, Low=yellow, Average=green, High=blue, Highest=purple (matches pop-up emojis).
  ses = list(
    labels = c("Lowest", "Low", "Average", "High", "Highest"),
    colours = c("#D72638", "#FFCC00", "#3DBB61", "#0078D7", "#6F008C"),
    # Emoji shown beside category in popup (National Comparison): 🔴 Lowest, 🟡 Low, 🟢 Average, 🔵 High, 🟣 Highest
    emoji = c("🔴", "🟡", "🟢", "🔵", "🟣"),
    # SD multipliers for break boundaries: mean + sd_multipliers * sd (4 values => 5 categories)
    sd_multipliers = c(-2, -1, 1, 2)
  ),

  # --- Green space ---
  green = list(
    # Count thresholds for green status (parks + playgrounds + nature in study area)
    # Above great => "Great", above good => "Good", above limited => "Limited", else "None"
    thresholds = c(great = 20L, good = 5L, limited = 1L),
    # Marker icon URLs
    icon_url = list(
      parks           = "https://cdn-icons-png.flaticon.com/512/284/284648.png",
      playgrounds     = "https://cdn-icons-png.flaticon.com/512/619/619032.png",
      nature_reserves = "https://cdn-icons-png.flaticon.com/512/3073/3073484.png"
    ),
    icon_width  = 40L,
    icon_height = 40L
  )
)
