# Packages ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(showtext)

# Fonts ------------------------------------------------------------------------
font_add_google("Courier Prime")

plot_font <- "Courier Prime"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

caption <- cmBrand::create_caption(
  social = cmBrand::social_brand(),
  tt_text = cmBrand::tt_text(year = 2026, week = 12, source = "PiDay")
)

# Theme ------------------------------------------------------------------------
theme_clean <- function() {
  theme_void(base_family = plot_font, base_size = 10) +
    theme(
      plot.title = element_text(
        size = rel(1.4),
        hjust = 0.5,
        colour = "white",
        face = "bold",
      ),
      plot.caption = ggtext::element_markdown(
        size = rel(0.6),
        hjust = 0,
        lineheight = 1.5,
        colour = "#999999",
        margin = margin(t = 0, r = 0, b = 5, l = 5, unit = "pt")
      )
    )
}

# Tidytuesday data -------------------------------------------------------------
pi_digits <- tidytuesdayR::tt_load(x = 2026, week = 12) |>
  purrr::pluck("pi_digits")

# Creating a spiral dataset ----------------------------------------------------
k <- 8 # Spacing between digits
r <- 2 # Radius

pi_spiral <- pi_digits |>
  slice_head(n = 1000) |>
  mutate(
    theta = sqrt(k * digit_position),
    radius = r * theta,
    x = radius * cos(theta),
    y = radius * sin(theta),
    angle = theta * 180 / pi,
    digit = factor(digit)
  )

# Plotting ---------------------------------------------------------------------
pi_spiral |>
  ggplot(aes(
    x = x,
    y = y,
    label = digit,
    colour = digit,
    alpha = digit_position
  )) +
  geom_text(size = 1.8) +
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.8) +
  scale_alpha(range = c(1, 0.4)) +
  labs(
    title = "The first 1000 digits of π",
    caption = caption
  ) +
  guides(colour = "none", alpha = "none") +
  theme_clean() +
  theme(plot.background = element_rect(fill = "black", colour = NA)) +
  ggview::canvas(width = 5, height = 5) -> pi_fig

# Export -----------------------------------------------------------------------
ggview::save_ggplot(pi_fig, here::here("2026/2026-03-24/20260324.png"))
