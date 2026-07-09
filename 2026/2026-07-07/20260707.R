# Packages ---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(showtext)

library(ggalluvial)

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
    week = 27,
    source = "Ultimate Fighting Championship data"
  )
)

caption <- paste(
  caption,
  "<br>**Note**: A 'catchweight' is the weight agreed to by both participants in a fight which is <br>stipulated to take place outside of the weight classes in the sport."
)

# Preparing caption ------------------------------------------------------------
subtitle <- glue::glue(
  "<span style='color:#E69F00BF;'>**Decision victories (50%)**</span> dominate, but
   <span style='color:#56B4E9BF;'>**knockout (32%)**</span> and
   <span style='color:#009E73BF;'>**submission (18%)**</span> rates vary by division"
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
tt_data <- tidytuesdayR::tt_load(x = 2026, week = 27)

ufc <- tt_data$ultimate_ufc_dataset

# Data processing --------------------------------------------------------------
finishes <- ufc |>
  mutate(
    finish = case_when(
      finish == "SUB" ~ "Submission",
      finish == "KO/TKO" ~ "Knockout",
      finish %in% c("U-DEC", "M-DEC", "S-DEC") ~ "Decision",
      TRUE ~ "Other"
    )
  ) |>
  filter_out(finish == "Other") |>
  count(weight_class, finish) |>
  group_by(weight_class) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

# Ordering the weight categories: lightest → heaviest
weights <- tibble::tribble(
  ~class                  , ~weight , ~label                  ,
  "Women's Strawweight"   ,  52.2   , "Women's Strawweight"   ,
  "Women's Flyweight"     ,  56.7   , "Women's Flyweight"     ,
  "Flyweight"             ,  56.7   , "Men's Flyweight"       ,
  "Women's Bantamweight"  ,  61.2   , "Women's Bantamweight"  ,
  "Bantamweight"          ,  61.2   , "Men's Bantamweight"    ,
  "Women's Featherweight" ,  65.8   , "Women's Featherweight" ,
  "Featherweight"         ,  65.8   , "Men's Featherweight"   ,
  "Lightweight"           ,  70.3   , "Lightweight"           ,
  "Welterweight"          ,  77.1   , "Welterweight"          ,
  "Middleweight"          ,  83.9   , "Middleweight"          ,
  "Light Heavyweight"     ,  93.0   , "Light Heavyweight"     ,
  "Heavyweight"           , 120.2   , "Heavyweight"           ,
  "Catch Weight"          , NA      , "Catch Weight"
) |>
  mutate(
    label = stringr::str_glue("{label} ({weight} kg)"),
    label = ifelse(stringr::str_detect(label, "Catch"), "Catch Weight", label)
  )

finishes <- finishes |>
  mutate(
    weight_class = factor(
      weight_class,
      levels = weights$class,
      labels = weights$label
    )
  )

# Plotting ---------------------------------------------------------------------
finishes |>
  ggplot(aes(axis1 = weight_class, axis2 = finish, y = prop)) +
  geom_alluvium(aes(fill = finish), width = 0.25, alpha = 0.8) +
  geom_stratum(width = 0.3, fill = "grey95", colour = "grey30") +
  geom_text(
    stat = "stratum",
    aes(label = stringr::str_wrap(after_stat(stratum), 10)),
    size = rel(2.25)
  ) +
  ggokabeito::scale_fill_okabe_ito(alpha = 0.75) +
  labs(
    title = "UFC Fight Finishes by Weight Class",
    subtitle = subtitle,
    x = NULL,
    y = NULL,
    caption = caption
  ) +
  guides(fill = "none") +
  theme_clean() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) +
  ggview::canvas(width = 5, height = 7.5) -> fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-07-07/20260707.png"))
