# Packages ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(showtext)

# Fonts ------------------------------------------------------------------------
font_add_google("Oswald")
font_add_google("Noto Sans")

title_font <- "Oswald"
body_font <- "Noto Sans"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

caption <- cmBrand::create_caption(
  social = cmBrand::social_brand(),
  tt_text = cmBrand::tt_text(
    year = 2026,
    week = 13,
    source = "Centre for Marine Applied Research's Coastal Monitoring Program"
  )
)

# Theme ------------------------------------------------------------------------
theme_clean <- function() {
  theme_minimal(base_family = body_font, base_size = 10) +
    theme(
      plot.title = element_text(
        size = rel(1.4),
        face = "bold",
        family = title_font
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(1),
        margin = margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")
      ),
      plot.caption = ggtext::element_markdown(
        size = rel(0.6),
        hjust = 0,
        lineheight = 1.25,
        colour = "#999999",
        margin = margin(t = 0, r = 0, b = 0.5, l = -30, unit = "pt")
      )
    )
}

# Tidytuesday data -------------------------------------------------------------
tt_data <- tidytuesdayR::tt_load(x = 2026, week = 13)

ocean_temperature <- tt_data$ocean_temperature
ocean_temperature_deployments <- tt_data$ocean_temperature_deployments

# Creating a season dataset ----------------------------------------------------
season_temps <- ocean_temperature |>
  rename(
    depth = sensor_depth_at_low_tide_m,
    mean_temp = mean_temperature_degree_c,
    sd_temp = sd_temperature_degree_c
  ) |>
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date),
    month_label = lubridate::month(date, label = TRUE),
    doy = lubridate::yday(date),

    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    ),

    # To make the lines connect nicely for winter (December -> January)
    # I wanna make sure ggplot knows December belongs in the next year
    season_year = if_else(month == 12, year + 1, year)
  )

# Plotting ---------------------------------------------------------------------
season_temps |>
  ggplot(aes(x = doy, y = mean_temp, colour = depth)) +
  geom_line(
    aes(group = interaction(season_year, depth)),
    alpha = 0.06,
    linewidth = 0.5
  ) +
  # Vertical lines to distinguish seasons
  geom_vline(
    xintercept = c(80, 172, 264, 355),
    colour = "grey70",
    linewidth = 0.3,
    linetype = "dashed",
    alpha = 0.5
  ) +
  # Mean lines for each depth
  stat_summary(
    aes(group = depth),
    fun = mean,
    geom = "line"
  ) +
  # Adding annotations
  annotate(
    "text",
    x = 90,
    y = 17,
    label = "Surface warms \nfaster",
    size = rel(3),
    hjust = 0,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 275,
    y = 2.5,
    label = "Deep stays \ncooler longer",
    size = rel(3),
    hjust = 0,
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = seq(15, 345, by = 60),
    labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")
  ) +
  scale_y_continuous(labels = scales::label_number(suffix = "°C")) +
  MetBrewer::scale_colour_met_c(name = "Homer1", direction = -1) +
  labs(
    title = "Seasonal ocean temperature cycles across depths",
    subtitle = "Thin lines show individual years <br> <b>Bold lines show the seasonal average</b>",
    x = NULL,
    y = "Temperature (°C)",
    colour = "Depth (meters)",
    caption = caption
  ) +
  theme_clean() +
  theme(
    axis.text = element_text(size = rel(0.7)),
    axis.title = element_text(size = rel(0.9)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, size = rel(0.8)),
    legend.key.width = unit(1.25, "cm")
  ) +
  ggview::canvas(width = 5, height = 4) -> fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-03-31/20260331.png"))
