# Packages ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(showtext)

# Fonts ------------------------------------------------------------------------
font_add_google("Inter")
font_add_google("IBM Plex Sans")

title_font <- "IBM Plex Sans"
body_font <- "Inter"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

caption <- cmBrand::create_caption(
  social = cmBrand::social_brand(),
  tt_text = cmBrand::tt_text(
    year = 2026,
    week = 14,
    source = "Repair Monitor"
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
      plot.caption = ggtext::element_markdown(
        size = rel(0.8),
        hjust = 0,
        lineheight = 1.25,
        colour = "#999999",
        margin = margin(t = 5, r = 0, b = 0.5, l = -80, unit = "pt")
      )
    )
}

# Tidytuesday data -------------------------------------------------------------
tt_data <- tidytuesdayR::tt_load(x = 2026, week = 14)

repairs <- tt_data$repairs
repairs_text <- tt_data$repairs_text

# Data processing --------------------------------------------------------------
# How often was something repaired and how successful was it?
repair_success <- repairs |>
  filter(!is.na(category), !is.na(repaired)) |>
  mutate(
    success = case_when(
      repaired == "yes" ~ 1,
      repaired == "half" ~ 0.5,
      repaired == "no" ~ 0
    )
  ) |>
  group_by(category) |>
  summarise(
    attempts = n(),
    success_rate = mean(success, na.rm = TRUE),
    .groups = "drop"
  )

# I could probably do with this some fancy regex but I'm not that smart 😅
repair_success_final <- repair_success |>
  mutate(
    category = dplyr::case_when(
      stringr::str_detect(category, "non-electric") ~
        stringr::str_replace(category, "non-electric", "\n(non-electric)"),

      stringr::str_detect(category, "electric") ~
        stringr::str_replace(category, "electric", "\n(electric)"),

      stringr::str_detect(category, "alarm") ~
        stringr::str_replace(category, "/ alarm", "and \nalarm"),

      stringr::str_detect(category, "phones") ~
        stringr::str_replace(category, "/ phones", "\nand phones"),

      stringr::str_detect(category, "sound") ~
        stringr::str_replace(category, "sound", "\nsound"),

      TRUE ~ category
    )
  )

# Plotting ---------------------------------------------------------------------
title <- stringr::str_glue(
  "Most items brought to repair cafés can be fixed\n({scales::percent(mean(repair_success_final$success_rate), accuracy = 1)})"
)

repair_success_final |>
  mutate(
    highlight = case_when(
      success_rate >= mean(repair_success_final$success_rate) ~ "above average",
      success_rate < mean(repair_success_final$success_rate) ~ "below average",
    )
  ) |>
  mutate(
    category = forcats::fct_reorder(category, success_rate),
    category = forcats::fct_rev(category)
  ) |>
  ggplot(aes(x = success_rate, y = category)) +
  geom_vline(
    xintercept = mean(repair_success_final$success_rate),
    colour = "grey70",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  geom_segment(
    aes(x = 0.5, xend = success_rate, yend = category),
    colour = "grey85",
    linewidth = 0.6
  ) +
  geom_point(
    aes(colour = highlight, size = attempts),
    alpha = 0.9
  ) +
  annotate(
    geom = "text",
    x = 0.9,
    y = 11.5,
    label = "Electronics are harder \nto repair",
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = seq(0.5, 1, by = 0.1),
    limits = c(0.5, 1),
    labels = scales::percent_format()
  ) +
  scale_size_continuous(
    trans = "sqrt",
    range = c(2, 8),
    labels = scales::comma_format(),
    name = "Repair attempts",
    guide = legendry::guide_circles(
      text_position = "right",
      override.aes = list(
        size = c(3, 7, 12),
        fill = "grey50",
        alpha = 0.5
      )
    )
  ) +
  MetBrewer::scale_colour_met_d(name = "Egypt", direction = -1) +
  labs(
    title = title,
    x = "Repair success rate",
    y = NULL,
    caption = caption
  ) +
  guides(colour = "none") +
  theme_clean() +
  theme(
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0.5),
    axis.title.x = element_text(size = rel(0.9), face = "bold"),
    legendry.legend.key.margin = margin(t = 5, r = 5, b = 0, l = 0, "pt"),
    legend.ticks = element_line(colour = "black", linetype = "22"),
    legend.position = "inside",
    legend.position.inside = c(0.88, 0.55),
    legend.text = element_text(size = rel(0.6)),
    legend.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = rel(0.9),
      margin = margin(t = 3, r = 5, b = 5, l = 0)
    )
  ) +
  ggview::canvas(width = 5, height = 6) -> fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-04-07/20260407.png"))
