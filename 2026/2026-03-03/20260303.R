# Packages ---------------------------------------------------------------------
pacman::p_load(
  dplyr,
  stringr,
  ggplot2,
  patchwork,
  showtext
)

# Fonts ------------------------------------------------------------------------
font_add_google("Oswald")
font_add_google("Noto Sans", "Noto Sans", regular.wt = 400, bold.wt = 700)

title_font <- "Oswald"
body_font <- "Noto Sans"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# Theme ------------------------------------------------------------------------
theme_tortoise <- function() {
  theme_minimal(base_size = 10, base_family = body_font) +
    theme(
      plot.title = ggtext::element_textbox_simple(
        size = rel(2.25),
        margin = margin(t = 5, r = 0, b = 10, l = 0, unit = "pt"),
        family = title_font,
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        lineheight = 2.5,
        margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")
      ),
      axis.text = element_text(size = rel(1.4)),
      axis.title = element_text(size = rel(1.6)),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 10, unit = "pt"),
      plot.background = element_rect(fill = "#FEFAE0")
    )
}

# Data -------------------------------------------------------------------------
tt_data <- tidytuesdayR::tt_load(2026, week = 9)

# Clutch size data
clutch_size <- tt_data$clutch_size_cleaned |>
  mutate(
    year = lubridate::year(date),
    locality = factor(locality, levels = c("Konjsko", "Beach", "Plateau")),
    location = ifelse(locality == "Konjsko", "Mainland", "Island"),
    location = factor(location, levels = c("Mainland", "Island"))
  )

# Body condition data
tortoise_body_condition <- tt_data$tortoise_body_condition |>
  mutate(
    sex = factor(sex, levels = c("f", "m"), labels = c("Females", "Males")),
    locality = factor(locality, levels = c("Konjsko", "Beach", "Plateau")),
    location = ifelse(locality == "Konjsko", "Mainland", "Island"),
    location = factor(location, levels = c("Mainland", "Island"))
  )

# Data processing --------------------------------------------------------------
## Male / female ratio over time for each location
sex_ratio <- tortoise_body_condition |>
  count(year, location, sex) |>
  group_by(year, location) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

## Summary statistics on female tortoises
body_condition_stats <- tortoise_body_condition |>
  filter(sex == "Females") |>
  group_by(location) |>
  summarise(
    mean_condition = mean(body_condition_index, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) |>
  transmute(
    location = location,
    y = c(10, 8),
    label = sprintf(
      "<span style='color:grey30;'>**Mean: %.2f <br> n = %.f**</span>",
      mean_condition,
      n
    )
  )

## Summary statistics on clutch sizes
clutch_stats <- clutch_size |>
  group_by(location) |>
  summarise(
    mean_eggs = mean(eggs, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) |>
  transmute(
    location = location,
    y = c(9, 6),
    label = sprintf(
      "<span style='color:grey20;'>**Mean: %.2f <br> n = %.f**</span>",
      mean_eggs,
      n
    )
  )

# Running t-tests --------------------------------------------------------------
body_condition_test <- tortoise_body_condition |>
  filter(sex == "Females") |>
  select(location, body_condition_index) |>
  rstatix::t_test(formula = body_condition_index ~ location) |>
  transmute(
    label = sprintf(
      "<span style='color:grey30; font-size:16pt;'><span style='color:#3A5F0B;'>**Mainland females**</span> are healthier<br>t(%.f) = %.2f; p < 0.001 </span>",
      df,
      statistic
    )
  )

clutch_size_test <- clutch_size |>
  select(location, eggs) |>
  rstatix::t_test(formula = eggs ~ location) |>
  transmute(
    label = sprintf(
      "<span style='color:grey30; font-size:16pt;'><span style='color:#3A5F0B;'>**Mainland females**</span> have bigger clutch sizes<br>t(%.f) = %.2f; p < 0.001 </span>",
      df,
      statistic
    )
  )

# Plotting ---------------------------------------------------------------------
# Plot 1
fig_1 <- sex_ratio |>
  filter(sex == "Males") |>
  ggplot(aes(x = year, y = prop, colour = location, size = location)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_colour_manual(
    values = c("Mainland" = "#3A5F0B", "Island" = "#7A1E1E")
  ) +
  scale_size_manual(
    values = c("Mainland" = rel(0.75), "Island" = rel(1.5)),
    guide = "none"
  ) +
  labs(
    title = "Island population shows increasing male bias",
    x = NULL,
    y = "Proportion of males",
    colour = NULL
  ) +
  guides(colour = "none") +
  theme_tortoise()

# Plot 2
fig_2 <- tortoise_body_condition |>
  filter(sex == "Females") |>
  ggplot(aes(
    x = location,
    y = body_condition_index,
    colour = location,
    fill = location,
    group = location
  )) +
  gghalves::geom_half_point(
    transformation = ggbeeswarm::position_quasirandom(),
    size = rel(0.7),
    alpha = 0.6,
    side = "l"
  ) +
  gghalves::geom_half_violin(colour = "black", side = "r") +
  stat_summary(fun = mean, geom = "point", colour = "black", size = rel(2.5)) +
  # Add statistical annotations
  ggtext::geom_richtext(
    data = body_condition_stats,
    aes(
      x = location,
      y = y,
      label = label
    ),
    colour = NA,
    fill = NA,
    size = rel(5),
    nudge_x = 0.1,
    hjust = 0,
    label.margin = margin(0, 0.5, 0, 0.5, "lines"),
    inherit.aes = FALSE,
  ) +
  scale_colour_manual(
    values = c("Mainland" = "#3A5F0B", "Island" = "#7A1E1E")
  ) +
  scale_fill_manual(values = c("Mainland" = "#3A5F0B", "Island" = "#7A1E1E")) +
  labs(
    title = "Female body condition suffers on the island",
    subtitle = body_condition_test$label,
    x = NULL,
    y = "Body condition index",
    colour = NULL,
    fill = NULL
  ) +
  guides(fill = "none", colour = "none") +
  theme_tortoise()

# Plot 3
fig_3 <- clutch_size |>
  ggplot(aes(x = location, y = eggs, colour = location, fill = location)) +
  gghalves::geom_half_point(
    transformation = ggbeeswarm::position_quasirandom(),
    alpha = 0.6,
    side = "l"
  ) +
  gghalves::geom_half_violin(colour = "black", side = "r") +
  ggtext::geom_richtext(
    data = clutch_stats,
    aes(x = location, y = y, label = label),
    colour = NA,
    fill = NA,
    size = rel(5),
    nudge_x = 0.1,
    hjust = 0,
    label.margin = margin(0, 0.5, 0, 0.5, "lines"),
    inherit.aes = FALSE
  ) +
  stat_summary(fun = mean, geom = "point", colour = "black", size = rel(2)) +
  scale_colour_manual(
    values = c("Mainland" = "#3A5F0B", "Island" = "#7A1E1E")
  ) +
  scale_fill_manual(values = c("Mainland" = "#3A5F0B", "Island" = "#7A1E1E")) +
  labs(
    title = "Island females produce fewer eggs",
    subtitle = clutch_size_test$label,
    x = NULL,
    y = "Clutch size (number of eggs)",
    colour = NULL,
    fill = NULL
  ) +
  guides(fill = "none", colour = "none") +
  theme_tortoise()


# Arranging in a grid ----------------------------------------------------------
## Annotations
tt_text <- cmBrand::tt_text(
  year = 2026,
  week = 9,
  source = "Arsovski et al., (2026)"
)
social <- cmBrand::social_brand()

title = "How sex ratio imbalance drives population collapse"
subtitle = "<span style='color:#7A1E1E;'>**Island tortoises**</span> show worsening health and reproduction compared <br>to the <span style='color:#3A5F0B;'>**mainland population**</span>"
caption <- stringr::str_glue("{tt_text} {social} &bull; #rstats #ggplot2")

fig_1 /
  (fig_2 + fig_3) +
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption,
    theme = theme(
      plot.title = ggtext::element_textbox_simple(
        size = rel(3),
        family = title_font,
        margin = margin(t = 5, r = 0, b = 10, l = 60, unit = "pt")
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(2),
        colour = "grey30",
        family = body_font,
        margin = margin(t = 0, r = 0, b = 10, l = 60, unit = "pt")
      ),
      plot.caption = ggtext::element_textbox_simple(
        size = rel(1.5),
        color = "gray50",
        family = body_font,
        hjust = 0,
        lineheight = 1.25,
        margin = margin(t = 5, r = 0, b = 5, l = 60, unit = "pt")
      ),
      plot.background = element_rect(fill = "#FEFAE0")
    )
  ) +
  ggview::canvas(width = 12, height = 16) -> final_fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(final_fig, here::here("2026/2026-03-03/20260303.png"))
