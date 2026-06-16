# Packages ---------------------------------------------------------------------
library(dplyr)
library(stringr)
library(ggplot2)
library(showtext)

# Fonts ------------------------------------------------------------------------
font_add("GOT", here::here("fonts/GOT.ttf"))
font_add_google("Open Sans")

title_font <- "GOT"
body_font <- "Open Sans"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

caption <- cmBrand::create_caption(
  social = cmBrand::social_brand(),
  tt_text = cmBrand::tt_text(
    year = 2026,
    week = 24,
    source = "UK Baby Names"
  )
)

# GOT colour palette -----------------------------------------------------------
arya_col <- gameofthrones::got_palettes$arya

text_col <- arya_col[2]
geom_col <- arya_col[4]

# House stark sigil
sigil <- png::readPNG(here::here("2026/2026-06-16/assets/wolf.png"))

# Modify the sigil to make it transparent
sigil[,, 4] <- sigil[,, 4] * 0.05

# Theme ------------------------------------------------------------------------
theme_clean <- function() {
  theme_minimal(base_family = body_font, base_size = 10) +
    theme(
      plot.title = ggtext::element_textbox_simple(
        size = rel(1.4),
        colour = text_col,
        face = "bold",
        family = title_font,
        margin = margin(t = 5, r = 0, b = 5, l = 0)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(1.1),
        colour = text_col,
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
tt_data <- tidytuesdayR::tt_load(x = 2026, week = 24)

# Combine into one dataset -----------------------------------------------------
baby_names <- purrr::map2(
  c(1, 2, 3),
  c("england_wales", "n_ireland", "scotland"),
  function(i, name) {
    tt_data[[i]] |>
      mutate(country = name)
  }
) |>
  bind_rows() |>
  janitor::clean_names()

# How many babies are called Arya ----------------------------------------------
arya_names <- baby_names |>
  filter(name == "Arya") |>
  group_by(year) |>
  summarise(n = sum(number, na.rm = TRUE))

arya_names |> filter(year <= 2012)

# Plotting ---------------------------------------------------------------------
arya_names |>
  ggplot(aes(x = year, y = n)) +
  # The time before GOT
  annotate(
    "rect",
    xmin = -Inf,
    xmax = 2011,
    ymin = 0,
    ymax = Inf,
    fill = "#D9D2C3",
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = 2003,
    y = max(arya_names$n) * 0.95,
    label = "BEFORE\nGAME OF THRONES",
    fontface = "bold",
    colour = "grey40"
  ) +
  # GOT releases
  geom_vline(
    xintercept = 2011,
    linewidth = 0.8,
    colour = "grey40",
    linetype = 2
  ) +
  annotate(
    "text",
    x = 2011.5,
    y = max(arya_names$n) * 0.75,
    label = "Game of Thrones premeries",
    fontface = "bold",
    colour = text_col,
    size = 3,
    angle = 270
  ) +
  # The rising of Arya
  geom_area(
    fill = geom_col,
    alpha = 0.8
  ) +
  geom_line(
    linewidth = 1,
    colour = "black"
  ) +
  # Peak of Arya
  geom_point(
    data = arya_names |> slice_max(n),
    colour = geom_col,
    size = rel(4)
  ) +
  annotate(
    "text",
    x = 2022,
    y = max(arya_names$n) * 1.01,
    label = "THE FINAL \nSEASON",
    fontface = "bold",
    colour = "grey40"
  ) +
  # Customisation
  annotation_custom(
    grob = grid::rasterGrob(image = sigil, interpolate = TRUE),
    xmin = 2015,
    xmax = 2023,
    ymin = 0,
    ymax = 400
  ) +
  scale_x_continuous(
    breaks = seq(1995, 2025, by = 5),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = "The rise of Arya after Game of Thrones",
    subtitle = "The number of babies named **Arya** rose sharply after Game of Thrones premiered in 2011",
    x = "Year",
    y = "Babies named Arya",
    caption = stringr::str_glue("{caption} <br> **Note:** Winter is Coming")
  ) +
  theme_clean() +
  theme(
    plot.background = element_rect(fill = "#F3EEE2", colour = NA),
    panel.background = element_rect(fill = "#F3EEE2", colour = NA),
    panel.grid = element_blank(),
    axis.title = element_text(colour = text_col),
    axis.text = element_text(colour = text_col)
  ) +
  ggview::canvas(width = 6, height = 5) -> fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-06-16/20260616.png"))
