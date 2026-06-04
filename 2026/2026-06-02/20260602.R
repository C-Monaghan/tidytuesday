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
    week = 23,
    source = "European Parenting Leave Policies"
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
tt_data <- tidytuesdayR::tt_load(x = 2026, week = 22)

# Processing -------------------------------------------------------------------
co_parent_leave <- tt_data |>
  purrr::pluck("eplp") |>
  select(year, country, co_ld) |>
  filter(co_ld > 0) |>
  group_by(country) |>
  summarise(min_year = min(year)) |>
  arrange(min_year) |>
  mutate(
    country = countrycode::countrycode(
      country,
      origin = "iso2c",
      destination = "country.name",
      custom_match = c("UK" = "United Kingdom")
    )
  )

# Plotting ---------------------------------------------------------------------
fig <- co_parent_leave |>
  ggplot(aes(x = min_year, y = reorder(country, min_year))) +
  geom_segment(
    aes(
      x = min(min_year),
      xend = min_year,
      yend = country
    ),
    colour = "grey90"
  ) +
  geom_point(size = rel(3)) +
  labs(
    title = "When European countries introduced co-parent leave",
    x = NULL,
    y = NULL,
    caption = caption
  ) +
  theme_clean() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  ggview::canvas(width = 5, height = 5)

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-06-02/20260602.png"))
