# Packages ---------------------------------------------------------------------
library(dplyr)
library(stringr)
library(ggplot2)
library(showtext)

# Functions --------------------------------------------------------------------
# R code for creating a cool sqiggly y axis
# Author: Wadea Abu Dahoud
# see https://github.com/wade31985-art/ggsquigglelog/blob/main/R/squiggle_log_axis.R
source(here::here("2026/2026-06-09/R/make_squiggled_log_axis.R"))

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
    week = 23,
    source = "List of films based on video games (Wikipedia)"
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
game_films <- tidytuesdayR::tt_load(x = 2026, week = 23) |>
  purrr::pluck("game_films")

# Data processing
franchise_summary <- game_films |>
  filter_out(str_detect(title, "Untitled") | is.na(worldwide_box_office)) |>
  mutate(
    franchise = str_extract(title, "^[^:]+"),
    franchise = str_remove(franchise, "\\d"),
    franchise = str_trim(franchise),

    # Some manual edits
    franchise = case_when(
      str_detect(franchise, "Pokémon") ~ "Pokémon",
      str_detect(franchise, "Persona") ~ "Persona",
      str_detect(franchise, "Mario") ~ "Super Mario",
      str_detect(franchise, "Kombat") ~ "Mortal Kombat",
      str_detect(franchise, "Fighter") ~ "Street Fighter",
      str_detect(franchise, "Silent") ~ "Silent Hill",
      TRUE ~ franchise
    )
  ) |>
  group_by(franchise) |>
  summarise(
    film_count = n(),
    total_box_office = sum(worldwide_box_office),
    avg_rotten_tomatoes = mean(rotten_tomatoes, na.rm = TRUE),
    top_film = title[which.max(worldwide_box_office)]
  ) |>
  filter_out(is.na(avg_rotten_tomatoes)) |>
  mutate(revenue_per_film = total_box_office / film_count) |>
  arrange(desc(revenue_per_film))

# Creating some labels
franchise_summary <- franchise_summary |>
  mutate(
    category = case_when(
      film_count >= 10 ~ "Blockbusters (10+ films)",
      film_count >= 3 ~ "Major Franchise (3-9 films)",
      TRUE ~ "Single/Sequel (1-2 films)"
    ),
    label = revenue_per_film > quantile(revenue_per_film, 0.8, na.rm = TRUE) |
      avg_rotten_tomatoes > 70 |
      franchise %in% c("Resident Evil", "Pokémon", "Super Mario")
  )

# Plotting ---------------------------------------------------------------------
franchise_summary |>
  ggplot(aes(
    x = avg_rotten_tomatoes,
    y = revenue_per_film,
    colour = category
  )) +
  geom_point(aes(size = film_count, alpha = film_count)) +
  # Cool sqiggly axis
  annotation_squiggled_log_y_axis(
    ymin = min(franchise_summary$revenue_per_film),
    ymax = max(franchise_summary$revenue_per_film),
    x0 = 0,
    amplitude = 1.5,
    linewidth = 0.35,
    base = 10,
    waves = 20,
    n = 4000,
    amplitude_power = 1.2,
    wave_power = 0.5
  ) +
  # Adding some labels to successful films
  ggrepel::geom_text_repel(
    data = franchise_summary |> filter(label),
    aes(label = franchise),
    size = 3,
    fontface = "bold",
    min.segment.length = 0,
    point.padding = 0.3,
    box.padding = 1.5,
    seed = 1234,
    max.overlaps = 15,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.2)),
  ) +
  scale_y_log10(
    breaks = c(1e5, 1e6, 1e7, 1e8, 1e9),
    labels = scales::dollar_format()
  ) +
  scale_size_continuous(breaks = c(1, 3, 5, 11, 20)) +
  scale_alpha_continuous(range = c(0.4, 0.9), guide = "none") +
  labs(
    title = "Do higher rated video game films earn more per release?",
    x = "Average Rotten Tomatoes score",
    y = "Average Revenue per Film (log scale)",
    caption = str_glue(
      "{caption}<br> *Note*: A **bigger circle size** represents a **greater number of films** in the franchise"
    ),
    colour = NULL
  ) +
  guides(size = "none") +
  theme_clean() +
  theme(legend.position = "bottom") +
  ggview::canvas(width = 6.5, height = 6) -> fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-06-09/20260609.png"))
