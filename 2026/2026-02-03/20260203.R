# Packages ---------------------------------------------------------------------
pacman::p_load(
  dplyr,
  stringr,
  ggplot2,
  ggridges,
  showtext
)

# Functions --------------------------------------------------------------------
parse_range <- function(x) {
  x <- str_replace_all(x, "\\s+", "")

  lower <- str_extract(x, "^\\d+") |> as.numeric()
  upper <- str_extract(x, "(?<=-)\\d+") |> as.numeric()

  tibble(
    lower = lower,
    upper = upper,
    middle = ifelse(
      !is.na(upper),
      (lower + upper) / 2,
      NA_real_
    )
  )
}

# Fonts ------------------------------------------------------------------------
font_add_google("Oswald")
font_add_google("Noto Sans")

title_font <- "Oswald"
body_font <- "Noto Sans"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# Preparing caption
tt_text <- cmBrand::tt_text(
  year = 2026,
  week = 5,
  source = "GROW Observatory Edible Plants Database"
)

socials <- cmBrand::social_brand()

caption_text <- str_glue(tt_text, socials, " &bull; #rstats #ggplot2")

# Theme ------------------------------------------------------------------------
theme_plants <- function() {
  theme_minimal(base_size = 10, base_family = body_font) +
    theme(
      plot.title = ggtext::element_textbox_simple(
        size = rel(1.25),
        hjust = 0,
        family = title_font,
        margin = margin(t = 5, r = 0, b = 5, l = 0)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(0.75),
        hjust = 0,
        margin = margin(t = 5, r = 0, b = 5, l = 0)
      ),
      plot.caption = ggtext::element_markdown(
        size = rel(0.65),
        hjust = 0,
        lineheight = 1.25,
        colour = "#999999",
        family = body_font
      ),
      strip.text = element_text(
        size = rel(0.85),
        family = title_font
      ),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.title = element_text(hjust = 0.5, size = rel(0.8)),
      legend.text = element_text(size = rel(0.7)),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(
        color = "gray90",
        linewidth = 0.3
      ),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = rel(0.65)),
      axis.text.x = element_text(size = rel(0.8)),
      plot.margin = margin(t = 10, r = 5, b = 10, l = 5)
    )
}

# Data -------------------------------------------------------------------------
edible_plants <- tidytuesdayR::tt_load(x = 2026, week = 5) |>
  purrr::pluck("edible_plants")

edible_plants |>
  select(sunlight) |>
  mutate(
    sunlight = str_replace_all(sunlight, "\\/", " or "),
    sunlight = str_to_title(sunlight),
    sunlight = str_replace_all(sunlight, "Or", "or"),
    sunlight = str_squish(sunlight)
  ) |>
  pull(sunlight) |>
  unique()

# Processing -------------------------------------------------------------------
plot_data <- edible_plants |>
  select(cultivation, common_name, sunlight, water) |>
  filter(cultivation != "Miscellaneous") |>
  mutate(
    sunlight = str_replace_all(sunlight, "\\/", " or "),
    sunlight = str_to_title(sunlight),
    sunlight = str_replace_all(sunlight, "Or", "or"),
    sunlight = str_squish(sunlight),

    sunlight = factor(
      sunlight,
      levels = c(
        "Full Sun",
        "Full Sun or Partial Shade or Full Shade",
        "Full Sun or Partial Shade",
        "Partial Shade"
      )
    ),

    water = str_to_sentence(water),
    water = factor(
      water,
      levels = c("Very low", "Low", "Medium", "High", "Very high")
    )
  ) |>
  count(cultivation, sunlight, water, name = "n_plants") |>
  group_by(cultivation) |>
  mutate(total_plants = sum(n_plants)) |>
  ungroup() |>
  mutate(cultivation = forcats::fct_reorder(cultivation, total_plants))

# Plotting ---------------------------------------------------------------------
subtitle <- str_glue(
  "Plants differ widely in their water needs depending on both their cultivation class and sunlight tolerance. For example, **Brassicas** and **Alliums** are mostly <span style = 'color:#41B6C4;'>**medium**</span> to <span style = 'color:#2C7FB8;'>**high**</span> water plants."
)

plot_data |>
  ggplot(aes(x = n_plants, y = cultivation, fill = water)) +
  geom_col(
    position = position_stack(reverse = TRUE),
    colour = "white",
    width = 0.7,
  ) +
  geom_text(
    aes(label = n_plants),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = rel(1.3),
    fontface = "bold",
    family = body_font
  ) +
  scale_fill_brewer(
    palette = "YlGnBu",
    direction = 1,
    name = "Water requirement",
    guide = guide_legend(
      keywidth = unit(0.8, "cm"),
      keyheight = unit(0.3, "cm")
    )
  ) +
  labs(
    title = "Water Requirements of Edible Plants Vary by Sunlight Needs and Cultivation Class",
    subtitle = subtitle,
    x = "Number of plant varieties",
    y = NULL,
    caption = caption_text,
  ) +
  facet_wrap(
    ~sunlight,
    labeller = labeller(
      sunlight = function(x) {
        case_when(
          x ==
            "Full Sun or Partial Shade or Full Shade" ~ "Adaptable\n(Any Sunlight)",
          TRUE ~ x
        )
      }
    )
  ) +
  theme_plants() +
  ggview::canvas(width = 6, height = 6) -> p

# Export -----------------------------------------------------------------------
ggview::save_ggplot(
  plot = p,
  file = here::here("2026", "2026-02-03", "20260203.png")
)
