# Packages ---------------------------------------------------------------------
pacman::p_load(
  dplyr,
  ggplot2,
  ggwaffle,
  showtext
)

# Fonts ------------------------------------------------------------------------
font_add_google("Oswald")
font_add_google("Noto Sans")

title_font <- "Oswald"
body_font <- "Noto Sans"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

tt_text <- cmBrand::tt_text(
  year = 2026,
  week = 6,
  source = "Olympic event schedule (Milano–Cortina 2026)"
)

socials <- cmBrand::social_brand()

# Theme ------------------------------------------------------------------------

# Data -------------------------------------------------------------------------
schedule <- tidytuesdayR::tt_load(x = 2026, week = 6) |>
  purrr::pluck("schedule")

# Processing -------------------------------------------------------------------
# How many hours are each event shown for
total_event_time <- schedule |>
  select(start_datetime_local, end_datetime_local, discipline_name) |>
  # Some events are happenning at the same time in the same room
  # I would assume these are different teams playing against eachother
  # simultaneouly. Let's look at distinct sporting events at distinct times.
  distinct(start_datetime_local, discipline_name, .keep_all = TRUE) |>
  mutate(
    duration = difftime(
      end_datetime_local,
      start_datetime_local,
      units = "hours"
    )
  ) |>
  group_by(discipline_name) |>
  summarise(total_time = sum(duration), .groups = "drop") |>
  mutate(
    total_time = round(total_time) |> as.numeric(),
    discipline_name = forcats::fct_reorder(discipline_name, total_time)
  )

# Plot -------------------------------------------------------------------------
title <- "Who Really Owns the Olympic Schedule?"
subtitle <- "**Ice Hokey** and **Curling** are two most scheduled events in the Winter Olympics."
caption <- stringr::str_glue("{tt_text} {socials} &bull; #rstats #ggplot2")

total_event_time |>
  ggplot(aes(x = total_time, y = discipline_name)) +
  geom_col(fill = "#E69F00", colour = "black", width = 0.7) +
  geom_text(
    aes(label = paste0(total_time, " hrs")),
    hjust = -0.2,
    color = "grey20",
    size = 3
  ) +
  scale_x_continuous(
    limits = c(0, max(total_event_time$total_time) * 1.15),
    labels = scales::label_number(suffix = " hrs")
  ) +
  labs(
    title = title,
    subtitle = subtitle,
    x = "Total number of hours",
    y = NULL,
    caption = caption
  ) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    plot.title = element_text(
      size = rel(1.4),
      face = "bold",
      family = title_font,
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = rel(1),
      margin = margin(0, 0, 5, 0),
    ),
    plot.caption = ggtext::element_markdown(
      size = rel(0.8),
      colour = "#999999",
      hjust = 1,
      halign = 0,
      lineheight = 1.25
    ),
    axis.text = element_text(colour = "grey20"),
    axis.title = element_text(colour = "grey20"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#FCF9DA"),
    plot.margin = margin(0.5, 0.5, 0.25, 0.25, "cm")
  ) +
  ggview::canvas(width = 5, height = 6) -> fig

# Export -----------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-02-10/20260210.png"))
