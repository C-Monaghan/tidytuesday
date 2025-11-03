rm(list = ls())

# Packages ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)

# Theme ------------------------------------------------------------------------
theme_map <- function() {
  theme_void() +
    theme(
      plot.background = element_rect(fill = "white"),
      plot.title = ggtext::element_markdown(
        face = "bold",
        hjust = 0,
        size = rel(1.4)
      ),
      plot.subtitle = ggtext::element_markdown(
        hjust = 0,
        size = rel(1.1),
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = ggtext::element_markdown(
        hjust = 0,
        size = rel(0.8),
        color = "grey50",
        margin = margin(t = 5, b = 5)
      )
    )
}

# Datasets ---------------------------------------------------------------------
# Tidy Tuesday data
tb_data <- tidytuesdayR::tt_load(2025, week = 46)$who_tb_data

# Europe polygon data
europe_data <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent == "Europe")

# Rates of death from TB in Europe per year
europe_deaths <- tb_data |>
  filter(g_whoregion == "Europe") |>
  select(country, year, e_mort_100k) |>
  # Standardizing country names (e.g., Russia Federation -> Russia)
  mutate(
    country_std = countrycode::countrycode(
      country,
      "country.name",
      "country.name.en"
    )
  ) |>
  mutate(country_std = ifelse(is.na(country_std), country, country_std))

# Joining mortality data with polygon data
europe_deaths_map <- europe_deaths |>
  left_join(europe_data, by = c("country_std" = "name")) |>
  st_as_sf()

# Consistent scale for facets
vmin <- min(europe_deaths_map$e_mort_100k, na.rm = TRUE)
vmax <- max(europe_deaths_map$e_mort_100k, na.rm = TRUE)

# Plotting ---------------------------------------------------------------------
tb_death_map <- europe_deaths_map |>
  ggplot() +
  geom_sf(fill = "grey90", colour = "white", linewidth = 0.1) +
  geom_sf(aes(fill = e_mort_100k), linewidth = 0.2) +
  scale_fill_gradientn(
    colours = rev(MetBrewer::met.brewer("Hiroshige")),
    name = "TB deaths (per 100k)",
    limits = c(vmin, vmax),
    na.value = "grey90"
  ) +
  coord_sf(xlim = c(-25, 55), ylim = c(35, 73), expand = FALSE) +
  labs(
    title = "Tuberculosis Mortality in Europe",
    subtitle = "Average TB deaths per 100k population by year",
    caption = stringr::str_glue(
      "#TidyTuesday: { 2025 } Week { 46 } &bull; Source: WHO TB Burden Data<br>"
    )
  ) +
  facet_wrap(~year) +
  theme_map() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.95, 0.05),
    legend.direction = "horizontal",
    legend.text = element_text(size = rel(0.55)),
    legend.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = rel(0.85),
      margin = margin(t = 3)
    ),
    legend.title.position = "bottom",
    legend.key.width = unit(1.3, "lines"),
    legend.key.height = unit(1, "lines"),
    strip.text = element_text(face = "bold", size = rel(0.9))
  )

# Exporting --------------------------------------------------------------------
cowplot::save_plot(
  filename = file.path(this.path::this.dir(), "2025-11-18.png"),
  plot = tb_death_map,
  base_width = 10,
  base_height = 7
)
