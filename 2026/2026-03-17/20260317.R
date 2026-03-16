# Packages ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(showtext)

# Fonts ------------------------------------------------------------------------
font_add_google("Fira Sans")
font_add_google("Open Sans")

title_font <- "Fira Sans"
body_font <- "Open Sans"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# Theme ------------------------------------------------------------------------
theme_clean_map <- function() {
  theme_void(base_family = body_font, base_size = 10) +
    theme(
      plot.title = ggtext::element_textbox_simple(
        size = rel(1.4),
        face = "bold",
        family = title_font,
        margin = margin(t = 5, r = 0, b = 5, l = 0, unit = "pt")
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(0.9),
        lineheight = 1.2,
        margin = margin(t = 5, r = 0, b = 5, l = 0, unit = "pt")
      ),
      plot.caption = ggtext::element_markdown(
        size = rel(0.7),
        hjust = 0,
        lineheight = 1.25,
        colour = "#999999"
      )
    )
}

# Tidytuesday data -------------------------------------------------------------
tt_data <- tidytuesdayR::tt_load(x = 2026, week = 11)

monthly_losses_data <- tt_data$monthly_losses_data
monthly_mortality_data <- tt_data$monthly_mortality_data

# Get spatial data on Norway ---------------------------------------------------
norway_spatial_data <- giscoR::gisco_get_nuts(
  country = "Norway",
  nuts_level = 3,
  resolution = "10"
) |>
  select(NUTS_NAME) |>
  mutate(NUTS_NAME = stringr::str_remove(NUTS_NAME, "/.*"))

# Data processing --------------------------------------------------------------
## Total levels of deaths, discards, escapes, and others by region
losses <- monthly_losses_data |>
  filter(geo_group == "county", species == "salmon") |>
  select(region, dead:other) |>
  tidyr::pivot_longer(
    cols = dead:other,
    names_to = "loss_type",
    values_to = "count"
  ) |>
  mutate(
    loss_type = factor(
      loss_type,
      levels = c("dead", "discarded", "escaped", "other"),
      labels = c("Dead Fish", "Discarded fish", "Escaped fish", "Other losses")
    )
  ) |>
  group_by(region, loss_type) |>
  summarise(total_loss = sum(count), .groups = "drop")

# Combine with map data --------------------------------------------------------
map_data <- left_join(
  losses,
  norway_spatial_data,
  by = c("region" = "NUTS_NAME")
) |>
  sf::st_as_sf() |>
  filter_out(region == "Akershus")

# Convert the county polygons into single points
point_data <- map_data |> sf::st_point_on_surface()

# Extract coordinates from points
coords <- point_data |> sf::st_coordinates()

# Offset the points slightly, so they aren't all overlapping
point_data_final <- point_data |>
  bind_cols(coords) |>
  rename(x = X, y = Y) |>
  mutate(
    # Assign each loss type a position around the centroid using angles.
    # These angles correspond to positions on a circle:
    # 0       = right
    # pi/2    = above
    # pi      = left
    # 3*pi/2  = below
    # This ensures the four points are placed symmetrically around the county
    # centroid instead of overlapping at the same location.
    angle = case_when(
      loss_type == "Dead Fish" ~ 0,
      loss_type == "Discarded fish" ~ pi / 2,
      loss_type == "Escaped fish" ~ pi,
      loss_type == "Other losses" ~ 3 * pi / 2
    ),
    # Distance that each point will be moved away from the centroid.
    radius = 0.18,
    # Convert polar coordinates (angle + radius) into cartesian offsets.
    x_new = x + radius * cos(angle),
    y_new = y + radius * sin(angle)
  )

# Setting up title, subtitle, and caption --------------------------------------
title <- "What happens to lost farmed salmon in Norway's aquaculture industry?"

subtitle <- "Farmed salmon and rainbow trout are lost each year through <span style='color:#E63946;'>**mortality**</span>, <span style='color:#E9C46A;'>**discarding**</span>, <span style='color:#2A9D8F;'>**escapes**</span>, and <span style='color:#8A817C;'>**other causes**</span> across Norwegian aquaculture regions."

caption <- cmBrand::create_caption(
  social = cmBrand::social_brand(),
  tt_text = cmBrand::tt_text(
    year = 2026,
    week = 11,
    source = "Norwegian Veterinary Institute"
  )
)

# Plotting ---------------------------------------------------------------------
ggplot() +
  # Base map of Norway
  geom_sf(
    data = map_data,
    fill = "grey95",
    colour = "grey70",
    linewidth = 0.25
  ) +
  # Adding centroid points
  geom_point(
    data = point_data_final,
    aes(x = x_new, y = y_new, size = total_loss, fill = loss_type),
    shape = 21,
    colour = "white",
    stroke = 0.3,
    alpha = 0.9
  ) +
  ggrepel::geom_label_repel(
    data = point_data_final |> distinct(region, x, y),
    aes(x = x, y = y, label = region),
    family = body_font,
    size = 2.5,
    label.size = 0,
    fill = "white",
    alpha = 0.9,
    box.padding = 4.35,
    point.padding = 0.5,
    segment.color = "grey70",
    segment.size = 0.3,
    force = 2,
    max.overlaps = Inf,
    seed = 123
  ) +
  # Customisation
  scale_size_continuous(
    trans = "sqrt",
    range = c(2, 10),
    breaks = c(20000000, 40000000, 80000000),
    labels = scales::label_number(scale = 1e-6, suffix = "M"),
    name = "Absolute loss \ncount",
    guide = legendry::guide_circles(
      text_position = "right",
      override.aes = list(
        size = c(3, 7, 12),
        fill = "grey50",
        alpha = 0.9
      )
    )
  ) +
  scale_fill_manual(
    values = c(
      "Dead Fish" = "#E63946", # Red
      "Discarded fish" = "#E9C46A", # Yellow
      "Escaped fish" = "#2A9D8F", # Teal
      "Other losses" = "#8A817C" # Grey
    ),
    name = "Loss Type"
  ) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
  ) +
  guides(fill = "none") +
  coord_sf(datum = NA, clip = "off", expand = FALSE) +
  theme_clean_map() +
  theme(
    legendry.legend.key.margin = margin(t = 5, r = 5, b = 0, l = 0, "pt"),
    legend.ticks = element_line(colour = "black", linetype = "22"),
    legend.position = "inside",
    legend.position.inside = c(0.88, 0.45),
    legend.text = element_text(size = rel(0.6)),
    legend.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = rel(0.8),
      margin = margin(t = 3, b = 5)
    )
  ) +
  ggview::canvas(width = 5, height = 6) -> fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-03-17/20260317.png"))
