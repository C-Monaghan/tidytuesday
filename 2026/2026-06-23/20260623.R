# Packages ---------------------------------------------------------------------
library(dplyr)
library(stringr)
library(ggplot2)
library(showtext)

library(ggraph)

# Fonts ------------------------------------------------------------------------
font_add_google("Cormorant Garamond")
font_add_google("Open Sans")

body_font <- "Cormorant Garamond"
number_font <- "Open Sans"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

caption <- cmBrand::create_caption(
  social = cmBrand::social_brand(),
  tt_text = cmBrand::tt_text(
    year = 2026,
    week = 25,
    source = "Varican.va"
  )
)

# Theme ------------------------------------------------------------------------
theme_clean <- function() {
  theme_minimal(base_family = body_font, base_size = 10) +
    theme(
      plot.title = ggtext::element_textbox_simple(
        size = rel(1.4),
        face = "bold",
        margin = margin(t = 5, r = 0, b = 5, l = 0)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(1.1),
        margin = margin(t = 5, r = 0, b = 5, l = 0)
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
tt_data <- tidytuesdayR::tt_load(x = 2026, week = 25)

encyclicals <- tt_data$encyclicals
papal_encyclicals <- tt_data$papal_encyclicals
scripture_references <- tt_data$scripture_references

# Data processing --------------------------------------------------------------
papal_summary <- papal_encyclicals |>
  group_by(pope) |>
  summarise(
    n = n(),
    start = min(year),
    end = max(year)
  ) |>
  mutate(
    duration = (end - start + 1),
    label = stringr::str_glue("Pope {pope} \n {start} - {end}"),
    label = ifelse(pope == "Leo XIV", "Pope Leo XIV \n 2026 - Present", label),
    label = forcats::fct_reorder(label, start, min)
  )

# Plotting ---------------------------------------------------------------------
papal_summary |>
  ggplot(aes(x = n, y = forcats::fct_rev(label), fill = duration)) +
  geom_col(colour = "white") +
  geom_text(
    aes(label = n),
    hjust = -0.2,
    size = 3.5,
    family = number_font,
    color = "#2D2D2D"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.1)),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  scale_fill_gradient(
    low = "#E5CAED",
    high = "#582468",
    guide = guide_colorbar(title = "Pontificate\n(years)\n")
  ) +
  labs(
    title = "Papal encyclicals over the years",
    subtitle = "Pope Leo XIII issued 86 encyclicals, far more than any other pontiff",
    x = "Number of encyclicals",
    y = NULL,
    caption = caption
  ) +
  coord_cartesian(clip = "off") +
  theme_clean() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(family = number_font),
    legend.text = element_text(family = number_font),
    legend.title = element_text(hjust = 0.5)
  ) +
  ggview::canvas(width = 5.5, height = 5) -> fig

# Export -----------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-06-23/20260623.png"))
