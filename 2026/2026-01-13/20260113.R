# Packages ---------------------------------------------------------------------
library(dplyr)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(ggtext)

# Theme ------------------------------------------------------------------------
theme_map <- function() {
  theme_void(base_size = 10) +
    theme(
      plot.title = element_markdown(
        size = 18,
        face = "bold",
        margin = margin(b = 15)
      ),
      plot.caption = element_markdown(
        hjust = 0,
        size = 9,
        color = "gray40",
        margin = margin(t = 15)
      ),
      strip.text = element_markdown(
        hjust = 0,
        face = "bold",
        padding = margin(8, 0, 8, 10),
        lineheight = 1.3
      ),
      strip.background = element_rect(fill = "gray95", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20),
      panel.spacing = unit(15, "points")
    )
}

# Load Tidy Tuesday dataset ----------------------------------------------------
tt_data <- tidytuesdayR::tt_load(2026, week = 2)$africa

# Processing -------------------------------------------------------------------
# Getting polygon data for Africa
africa <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent == "Africa") |>
  st_transform(
    crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  )

# Top 5 languages spoken in Africa
top_5_languages <- tt_data |>
  distinct(language, native_speakers, .keep_all = TRUE) |>
  arrange(desc(native_speakers)) |>
  slice_head(n = 5) |>
  mutate(
    language = factor(language, levels = language),
    label = glue::glue(
      "<span style='font-size:12pt; font-weight:bold;'>{language}</span><br>
      <span style='font-size:9pt; color:gray50;'>{round(native_speakers / 1000000, 1)} million native speakers</span>"
    )
  )

# Joining with polygon data
top_5_languages_map <- tt_data |>
  filter(language %in% top_5_languages$language) |>
  left_join(africa |> select(name_en), by = c("country" = "name_en")) |>
  st_as_sf() |>
  mutate(language = factor(language, levels = top_5_languages$language))

# Plotting ---------------------------------------------------------------------
fig <- ggplot() +
  # Base map of Africa
  geom_sf(
    data = africa,
    fill = "#99999980",
    colour = "white",
    linewidth = 0.3
  ) +
  # Top 5 spoken languages
  geom_sf(
    data = top_5_languages_map,
    fill = ggokabeito::palette_okabe_ito(1),
    colour = "white",
  ) +
  # Customisations
  labs(
    title = "Top 5 Most Spoken Languages in Africa",
    caption = glue::glue(
      "#TidyTuesday: 2026 Week 2 &bull; Source: The Languages of Africa"
    ),
  ) +
  facet_wrap(
    ~language,
    labeller = labeller(
      language = function(x) {
        ifelse(x %in% top_5_languages$language, top_5_languages$label, "")
      }
    )
  ) +
  theme_map()

# Exporting --------------------------------------------------------------------
cowplot::save_plot(
  filename = here::here("2026", "2026-01-13", "20260113.png"),
  plot = fig,
  base_height = 7,
  base_width = 7
)
