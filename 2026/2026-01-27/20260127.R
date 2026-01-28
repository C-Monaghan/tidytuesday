# Package ----------------------------------------------------------------------
library(dplyr)
library(stringr)

library(showtext)
library(ggplot2)

# Fonts ------------------------------------------------------------------------
## I always mess up with fonts so hopefully this time it works 🤞

font_add("fa7-brands", here::here("fonts/FA-7-Regular.otf"))
font_add_google("Noto Sans")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)

# What font to use where
title_font <- "Noto Sans"
body_font <- "Nunito"

# Setting up caption -----------------------------------------------------------
tt <- str_glue(
  "#TidyTuesday: { 2026 } Week { 4 } &bull; Source: Open data CNPJ - December 2025 <br>"
)
li <- str_glue("<span style='font-family:fa7-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa7-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa7-brands'>&#xe671;</span>")

caption_text <- str_glue(
  "{tt} {li} c-monaghan &bull; {gh} c-monaghan &bull; {bs} c-monaghan &bull; #rstats #ggplot2"
)

# Theme ------------------------------------------------------------------------
theme_company <- function() {
  theme_minimal(base_size = 12, base_family = body_font) +
    theme(
      plot.title = ggtext::element_textbox_simple(
        size = rel(1.25),
        hjust = 0,
        face = "bold",
        colour = "#000000",
        family = title_font
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(0.75),
        hjust = 0,
        colour = "#000000E6",
        family = title_font,
        margin = margin(4, 0, 0, 0)
      ),
      plot.caption = ggtext::element_markdown(
        size = rel(0.65),
        hjust = 0,
        lineheight = 1.25,
        colour = "#999999",
        family = body_font
      ),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = rel(0.75)),
      axis.title.x = element_text(hjust = 1, size = rel(0.8)),
      strip.text = element_blank(),
      strip.background.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.spacing.y = unit(2.5, "lines"),
      plot.margin = margin(10, 20, 10, 10)
    )
}

# Load Tidy Tuesday dataset ----------------------------------------------------
tt_data <- tidytuesdayR::tt_load(2026, week = 4) |> purrr::pluck("companies")

# Data processing --------------------------------------------------------------
company_data <- tt_data |>
  select(legal_nature, capital_stock) |>
  # Using code from Nicola Rennie
  # https://github.com/nrennie/tidytuesday/blob/main/2026/2026-01-27/20260127.R
  mutate(
    legal_nature = str_remove_all(legal_nature, "\\(.*?\\)"),
    legal_nature = str_trim(legal_nature),
    legal_nature = str_replace(legal_nature, "-", " "),
    legal_nature = str_to_sentence(legal_nature),
    legal_nature = str_replace(legal_nature, "llc", "LLC"),
    legal_nature = str_replace(legal_nature, "brazil", "Brazil")
  ) |>
  mutate(capital_stock = capital_stock / 1000000000) |>
  group_by(legal_nature) |>
  filter(n() >= 5) |>
  ungroup()

top_traded_summary <- company_data |>
  group_by(legal_nature) |>
  summarise(median_stock = median(capital_stock), .groups = "drop") |>
  arrange(desc(median_stock)) |>
  pull(legal_nature)

company_data <- company_data |>
  mutate(legal_nature = factor(legal_nature, levels = top_traded_summary))

# Plotting ---------------------------------------------------------------------
fig <- company_data |>
  ggplot(aes(
    x = capital_stock,
    y = forcats::fct_rev(legal_nature),
  )) +
  geom_violin(
    colour = "grey30",
    fill = "#56B4E9",
    alpha = 0.8
  ) +
  geom_text(
    data = company_data |> group_by(legal_nature) |> slice_head(),
    aes(
      x = 0.00002,
      y = legal_nature,
      label = str_wrap(legal_nature, 20)
    ),
    hjust = 1,
    alpha = 0.8,
    lineheight = 1.25,
    size = rel(3),
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 1.25,
    colour = "grey20"
  ) +
  labs(
    title = "Publicly traded corporations sit at the extreme upper tail of capital stock",
    subtitle = "Declared share capital distributions across Brazilian legal structures. Each point represents the median declared shared capital, with the violins reflecting the distribution of firms across capital levels.",
    x = "Declared share capital (billions, BRL)",
    y = NULL,
    caption = caption_text
  ) +
  # Using code from Nicola Rennie
  # https://github.com/nrennie/tidytuesday/blob/main/2026/2026-01-27/20260127.R
  scale_x_log10(
    limits = c(0.00000001, 1000),
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000),
    labels = function(x) format(x, scientific = FALSE, drop0trailing = TRUE)
  ) +
  theme_company()

# Export -----------------------------------------------------------------------
cowplot::save_plot(
  filename = here::here("2026", "2026-01-27", "20260127.png"),
  plot = fig,
  base_height = 10,
  base_width = 5
)
