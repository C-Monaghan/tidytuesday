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

caption <- cmBrand::create_caption(
  social = cmBrand::social_brand(),
  tt_text = cmBrand::tt_text(
    year = 2026,
    week = 26,
    source = "Wreck Inventory of Ireland Database"
  )
)

# Theme ------------------------------------------------------------------------
theme_clean <- function() {
  theme_void(base_family = body_font, base_size = 10) +
    theme(
      plot.title = ggtext::element_textbox_simple(
        colour = "white",
        size = rel(1.4),
        face = "bold",
        family = title_font,
        margin = margin(t = 10, r = 0, b = 5, l = 2)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        colour = "white",
        size = rel(1.1),
        margin = margin(t = 5, r = 0, b = 5, l = 0)
      ),
      plot.caption = ggtext::element_markdown(
        size = rel(0.8),
        hjust = 0,
        lineheight = 1.25,
        colour = "#999999",
        margin = margin(t = 5, r = 0, b = 0.5, l = 2, unit = "pt")
      )
    )
}

# Tidytuesday data -------------------------------------------------------------
tt_data <- tidytuesdayR::tt_load(x = 2026, week = 26)

# Data processing --------------------------------------------------------------
wreck_locations <- tt_data |>
  purrr::pluck("wreck_inventory") |>
  select(year, latitude, longitude) |>
  tidyr::drop_na() |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Getting map data of Ireland --------------------------------------------------
ireland <- rnaturalearth::ne_countries(
  country = "Ireland",
  scale = "medium",
  returnclass = "sf"
)

# Plotting ---------------------------------------------------------------------
ggplot() +
  geom_sf(data = ireland, fill = "grey15", colour = "grey40") +
  geom_sf(
    data = wreck_locations,
    colour = "white",
    alpha = 0.2,
    size = 0.3
  ) +
  labs(
    title = "Known shipwrecks in Irish waters (1550 - 2017)",
    x = NULL,
    y = NULL,
    caption = caption
  ) +
  theme_clean() +
  theme(
    plot.background = element_rect(fill = "black", colour = NA),
    panel.background = element_rect(fill = "black", colour = NA),
  ) +
  ggview::canvas(width = 5, height = 5) -> fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-06-30/20260630.png"))
