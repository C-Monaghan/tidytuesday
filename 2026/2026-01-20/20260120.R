# Packages ---------------------------------------------------------------------
library(dplyr)
library(lubridate)

library(ggplot2)
library(ggtext)

# Theme ------------------------------------------------------------------------
# Nasa colours
nasa_palette <- c(
  "image" = "#0B3D91", # NASA Torea Bay
  "video" = "#FC3D21" # NASA Red Orange
)

theme_nasa <- function() {
  theme_minimal() +
    theme(
      plot.title = element_markdown(
        size = 18,
        face = "bold",
        margin = margin(b = 15),
      ),
      plot.caption = element_markdown(
        hjust = 0,
        size = 9,
        color = "gray40",
        margin = margin(t = 15)
      ),
      axis.title = element_text(size = 12, color = "gray30"),
      axis.text = element_text(size = 10, color = "gray40"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.4),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.margin = margin(b = 10),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20),
      axis.line.x = element_line(color = "gray80", linewidth = 0.5),
      axis.ticks.x = element_line(color = "gray80")
    )
}

# Load Tidy Tuesday dataset ----------------------------------------------------
tt_data <- tidytuesdayR::tt_load(2026, week = 3)$apod

# Processing -------------------------------------------------------------------
# Count of images / videos per year + month (2007 - 2026)
apod_clean <- tt_data |>
  mutate(
    # Extracting tidy date / year / month columns
    date = ymd(date),
    year = year(date),
    month = month(date, label = TRUE, abbr = FALSE),
    year_month = floor_date(date, "month"),
    media_type = factor(media_type),
    # Can group by decades
    decade = case_when(
      year < 2010 ~ "2007-2009",
      year < 2020 ~ "2010-2019",
      TRUE ~ "2020-2025"
    )
  ) |>
  group_by(year_month, year, month, decade, media_type) |>
  summarise(count = n(), .groups = "drop") |>
  # Fill in missing months with zeros
  tidyr::complete(year_month, media_type, fill = list(count = 0)) |>
  # There are a couple of "other" formats - we want to remove them
  filter(media_type %in% c("image", "video")) |>
  group_by(year_month) |>
  mutate(
    total = sum(count),
    proportion = ifelse(total > 0, count / total, 0)
  ) |>
  ungroup()

# Annotations
labels <- tibble::tribble(
  ~year_month           , ~proportion , ~size , ~label                                                                            ,
  as.Date("2008-11-20") , 0.15        , 3.5   , "**A police cruiser <br>captured video of a <br> small meteor over <br> Canada**" ,
  as.Date("2008-11-01") , 0.75        , 3.5   , "**Image-dominated <br>era**"                                                     ,
  as.Date("2013-01-01") , 0.20        , 3.5   , "**The gentle rise of videos**"                                                   ,
)

# Plotting ---------------------------------------------------------------------
fig <- apod_clean |>
  # Base plot
  ggplot(aes(x = year_month, y = proportion, fill = media_type)) +
  geom_area(
    alpha = 0.85,
    position = "stack",
    color = NA
  ) +
  geom_line(
    aes(color = media_type, group = media_type),
    position = "stack",
    linewidth = 0.8,
    alpha = 0.3
  ) +
  geom_vline(
    data = data.frame(x = as.Date("2011-01-01")),
    aes(xintercept = x),
    linetype = "dashed",
    color = "#999999",
    linewidth = 0.4,
  ) +
  # Annotations
  ggtext::geom_richtext(
    data = labels,
    aes(label = label, fill = NULL),
    colour = "#000000",
    fill = "#99999966",
    size = labels$size,
    label.colour = "white",
    show.legend = FALSE
  ) +
  # Customisation
  scale_fill_manual(
    values = nasa_palette,
    labels = c("Image", "Video")
  ) +
  scale_color_manual(values = nasa_palette, guide = "none") +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.02)),
    limits = c(min(apod_clean$year_month), max(apod_clean$year_month))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.02)),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    title = "Evolution of Media in NASA's Astronomy Picture of the Day",
    x = NULL,
    y = "Monthly proportion",
    caption = glue::glue(
      "#TidyTuesday: 2026 Week 3 &bull; Source: Astronomy Picture of the Day Archive"
    )
  ) +
  theme_nasa() +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(0.6, "cm"),
      keywidth = unit(2, "cm"),
      label.position = "bottom",
      nrow = 1,
      override.aes = list(alpha = 1)
    )
  )

# Exporting --------------------------------------------------------------------
cowplot::save_plot(
  filename = here::here("2026", "2026-01-20", "20260120.png"),
  plot = fig,
  base_height = 8,
  base_width = 12
)
