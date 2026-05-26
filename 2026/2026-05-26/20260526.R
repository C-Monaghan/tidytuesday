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
    week = 22,
    source = "Sustainable Energy for all (SE4ALL)"
  )
)

# Theme ------------------------------------------------------------------------
theme_clean <- function() {
  theme_minimal(base_family = body_font, base_size = 10) +
    theme(
      plot.title = ggtext::element_textbox_simple(
        size = rel(1.4),
        face = "bold",
        family = title_font,
        margin = margin(t = 5, r = 0, b = 5, l = 0)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(1.1),
        margin = margin(t = 5, r = 0, b = 0, l = 0)
      ),
      plot.caption = ggtext::element_markdown(
        size = rel(0.8),
        hjust = 0,
        lineheight = 1.25,
        colour = "#999999",
        margin = margin(t = 5, r = 0, b = 0.5, l = 0, unit = "pt")
      )
    )
}

# Tidytuesday data -------------------------------------------------------------
tt_data <- tidytuesdayR::tt_load(x = 2026, week = 21)

# Data processing --------------------------------------------------------------
irish_energy <- tt_data |>
  purrr::pluck("energy_cleaned") |>
  filter(country_name == "Ireland") |>
  select(
    year = yr,
    wind_energy_consumption_tfec_pct,
    hydro_energy_consumption_tfec_pct,
    solar_energy_consumption_tfec_pct,
    modern_biomass_energy_consumption_tfec_pct,
    liquid_biofuels_energy_consumption_tfec_pct,
    waste_energy_consumption_tfec_pct
  ) |>
  tidyr::pivot_longer(
    cols = -year,
    names_to = "source",
    values_to = "value"
  ) |>
  mutate(
    source = recode(
      source,
      wind_energy_consumption_tfec_pct = "Wind",
      hydro_energy_consumption_tfec_pct = "Hydro",
      solar_energy_consumption_tfec_pct = "Solar",
      modern_biomass_energy_consumption_tfec_pct = "Biomass",
      liquid_biofuels_energy_consumption_tfec_pct = "Biofuels",
      waste_energy_consumption_tfec_pct = "Waste"
    )
  ) |>
  mutate(value = tidyr::replace_na(value, 0)) |>
  group_by(year) |>
  mutate(
    renewable_share = value / sum(value, na.rm = TRUE)
  ) |>
  ungroup()

# Where to place the labels on the graph
label_data <- irish_energy |>
  group_by(source) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(
    year = case_when(
      source == "Wind" ~ 2002,
      source == "Hydro" ~ 1993,
      source == "Solar" ~ 2010,
      source == "Biomass" ~ 1990,
      source == "Biofuels" ~ 2010,
      source == "Waste" ~ 2010
    )
  ) |>
  mutate(
    renewable_share = case_when(
      source == "Wind" ~ 0.15,
      source == "Hydro" ~ 0.20,
      source == "Solar" ~ 0.41,
      source == "Biomass" ~ 0.75,
      source == "Biofuels" ~ 0.90,
      source == "Waste" ~ 0.39
    )
  ) |>
  mutate(
    use_repel = ifelse(source %in% c("Biofuels", "Waste", "Solar"), TRUE, FALSE)
  )

# Plotting ---------------------------------------------------------------------
irish_energy |>
  ggplot(aes(x = year, y = renewable_share, fill = source)) +
  geom_area(colour = "white", linewidth = rel(0.15)) +
  geom_text(
    data = label_data |> filter(use_repel == FALSE),
    aes(
      x = year,
      y = renewable_share,
      label = source
    ),
    nudge_x = 3,
    hjust = 0,
    size = rel(3),
    fontface = "bold",
  ) +
  ggrepel::geom_text_repel(
    data = label_data |> filter(use_repel == TRUE),
    aes(
      x = year,
      y = renewable_share,
      label = source
    ),
    nudge_x = 2,
    direction = "y",
    hjust = 0,
    size = rel(3),
    fontface = "bold",
    arrow = arrow(length = unit(0.01, "npc")),
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Wind" = "#0F766E",
      "Hydro" = "#3B82F6",
      "Solar" = "#F59E0B",
      "Biomass" = "#8B5E3C",
      "Biofuels" = "#DC2626",
      "Waste" = "#6B7280"
    )
  ) +
  labs(
    title = "Ireland: From biomass to wind",
    subtitle = "Ireland’s renewable mix shifted from biomass-heavy in the early 1990s to wind-dominated today.",
    x = NULL,
    y = "Share of renewable energy mix",
    fill = NULL,
    caption = caption
  ) +
  theme_clean() +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  ggview::canvas(width = 5, height = 6) -> fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-05-26/20260526.png"))
