# Packages ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(showtext)

# Fonts ------------------------------------------------------------------------
font_add_google("Oswald")
font_add_google("Inter")

title_font <- "Oswald"
body_font <- "Inter"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# Tidytuesday data -------------------------------------------------------------
dataset <- tidytuesdayR::tt_load(x = 2026, week = 7) |>
  purrr::pluck("dataset")

# Total number of animal each year ---------------------------------------------
total_animal_year <- dataset |>
  filter(stringr::str_detect(
    value_label,
    "goats|sheep|cattle|deer|chickens|poultry|pigs|horses"
  )) |>
  group_by(year_ended_june, value_label) |>
  summarise(total_animal_year = sum(value), .groups = "drop")

# Total number of animal overall -----------------------------------------------
total_animal_overall <- total_animal_year |>
  group_by(value_label) |>
  summarise(total_animal = sum(total_animal_year)) |>
  mutate(
    label = stringr::str_glue(
      "<span style='font-family:{body_font}; font-weight:bold; font-size:8pt;'>{value_label}</span><br>
        <span style='font-family:{body_font}; font-size:7pt; color:#666666;'>Total: {scales::comma(total_animal)}</span>"
    )
  )

# What year was the highest animal proportion ----------------------------------
highest_year <- total_animal_year |>
  split(~value_label) |>
  purrr::map(function(animal) {
    animal |>
      slice_max(total_animal_year) |>
      rename(highest_animal = total_animal_year)
  }) |>
  bind_rows() |>
  mutate(
    highest_year_amount = stringr::str_glue(
      "{round(highest_animal / 1e6, 2)} million<br>{stringr::str_remove(value_label, 'Number of ')}"
    )
  ) |>
  mutate(
    x_points = c(1976, 2015, 1990, 2000, 1995, 1990, 2020, 1960),
    y_points = highest_animal / 1e6 + c(0, -20, 0, 0, 0.1, 0, -4, 0)
  )

# Join everything together -----------------------------------------------------
plot_data <- total_animal_year |>
  left_join(total_animal_overall, by = "value_label") |>
  left_join(highest_year, by = c("year_ended_june", "value_label"))

# Where do the highest animals appear
peak_data <- plot_data |>
  na.omit() |>
  mutate(label = forcats::fct_rev(label))

# Annotations ------------------------------------------------------------------
tag <- stringr::str_glue(
  "<span style='font-family:{title_font}; font-size:10pt;'>",
  "While the New Zealand sheep population has <br>been on a steady decline since 1983, ",
  "<br>other animal populations have also declined <br>(goats, deer, horses, etc.). ",
  "In contrast, <br>the chicken population has been steadily <br>rising.",
  "</span>"
)

social <- cmBrand::social_brand()
tt_text <- cmBrand::tt_text(
  year = 2026,
  week = 7,
  source = "New Zealand agricultural production statistics"
)

caption <- stringr::str_glue(
  "{tt_text} {social} &bull; #rstats #ggplot2"
)

# Plot -------------------------------------------------------------------------
plot_data |>
  mutate(
    label = forcats::fct_rev(label)
  ) |>
  ggplot(aes(x = year_ended_june, y = total_animal_year / 1e6)) +
  geom_line(aes(group = value_label), colour = "#E69F00") +
  geom_point(data = peak_data, aes(y = highest_animal / 1e6)) +
  ggtext::geom_richtext(
    data = peak_data,
    aes(
      x = x_points,
      y = y_points,
      label = highest_year_amount
    ),
    size = rel(2.25)
  ) +
  scale_y_continuous(
    labels = scales::comma_format(suffix = " M"),
    expand = expansion(0.25, 0.05)
  ) +
  labs(
    x = NULL,
    y = "Total (millions)",
    tag = tag,
    caption = caption
  ) +
  facet_wrap(
    ~label,
    scales = "free"
  ) +
  guides(colour = "none") +
  theme_minimal(base_size = 10, base_family = plot_font) +
  theme(
    plot.tag = ggtext::element_textbox_simple(hjust = 0.5, halign = 0.5),
    plot.tag.position = c(0.85, 0.2),
    plot.caption = ggtext::element_markdown(
      size = rel(0.6),
      hjust = 0,
      colour = "#999999",
      margin = margin(5, 0, 0, 0)
    ),
    strip.text = ggtext::element_textbox_simple(lineheight = 1.25),
    axis.text.x = element_text(size = rel(0.8)),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10)
  ) +
  ggview::canvas(width = 7, height = 6) -> fig

# Export -----------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-02-17/20260217.png"))
