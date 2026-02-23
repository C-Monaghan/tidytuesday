# Packages ---------------------------------------------------------------------
pacman::p_load(
  dplyr,
  stringr,
  ggplot2,
  showtext
)

# Fonts ------------------------------------------------------------------------
font_add_google("Oswald")
font_add_google("Noto Sans")

title_font <- "Oswald"
body_font <- "Noto Sans"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# Data -------------------------------------------------------------------------
sfi_grants <- tidytuesdayR::tt_load(2026, week = 8) |>
  purrr::pluck("sfi_grants")

# IUA Members ------------------------------------------------------------------
iau_members <- c(
  "Maynooth University (MU)",
  "University College Dublin (UCD)",
  "Trinity College Dublin (TCD)",
  "Dublin City University (DCU)",
  "University of Limerick (UL)",
  "University College Cork (UCC)",
  "University of Galway"
)

# How much funding did each IAU member get each year ---------------------------
iau_funding <- sfi_grants |>
  filter(research_body %in% iau_members) |>
  select(start_date, end_date, research_body, current_total_commitment) |>
  mutate(
    year = lubridate::year(start_date),
    research_body = factor(research_body, levels = iau_members)
  ) |>
  group_by(year, research_body) |>
  summarise(
    grant_funding = sum(current_total_commitment) / 1e6,
    .groups = "drop"
  )

# How much overall funding did each IUA they get -------------------------------
total_funding <- iau_funding |>
  group_by(research_body) |>
  summarise(total_funding = sum(grant_funding)) |>
  mutate(
    label = str_glue(
      "<span style='font-weight:bold;'>{research_body}</span><br>
        <span style='font-size:8pt; color:gray50;'>Total funding: €{scales::number(total_funding, accuracy = 1)} million </span>"
    )
  )

# Joining yearly and overall funding -------------------------------------------
iau_funding <- iau_funding |>
  left_join(
    total_funding |> select(research_body, total_funding),
    by = "research_body"
  ) |>
  mutate(
    research_body = forcats::fct_reorder(
      research_body,
      total_funding,
      .desc = TRUE
    )
  )

# For some reason I cannot get the strip texts in the plot to be arranged
# correctly. For example, have TCD and TCD's overall funding be together. Using
# setNames (recommended by ChatGPT seems to fix this issue) ...
label_map <- setNames(
  total_funding$label,
  total_funding$research_body
)

# Plot aesthetics --------------------------------------------------------------
## Title
title <- str_glue(
  "<span style='font-family:{title_font}; font-size:14pt;'>**How much funding did IUA members receive from Science Foundation Ireland**</span>"
)

## Subtitle
subtitle <- sfi_grants |>
  select(research_body, current_total_commitment) |>
  filter(research_body %in% iau_members) |>
  summarise(overall_total = sum(current_total_commitment) / 1e9) |>
  transmute(
    subtitle = str_glue(
      "<span style='font-size:10pt;'>Over a 25 year period, Science Foundation Ireland committed <span style='color:#85BB65;'>**€{round(overall_total, 2)} billion** </span>into university research</span>"
    )
  )

## Caption setup
tt_text <- cmBrand::tt_text(
  year = 2026,
  week = 8,
  source = "Ireland's Open Data Portal"
)

socials <- cmBrand::social_brand()

caption <- str_glue(
  "{title} <br> {subtitle} <br>
  <span style='font-size:7pt; color:#999999;'>{tt_text} {socials} &bull; #rstats #ggplot2 **Note.** IUA = Irish University Association</span>"
)

# Plot -------------------------------------------------------------------------
iau_funding |>
  ggplot(aes(x = year, y = grant_funding, group = research_body)) +
  geom_line(colour = "#85BB65", size = rel(1)) +
  scale_y_continuous(
    labels = scales::label_number(prefix = "€", suffix = "m")
  ) +
  labs(
    x = NULL,
    y = "Funding (€ millions)",
    tag = caption
  ) +
  facet_wrap(
    ~research_body,
    labeller = as_labeller(label_map),
    scales = "fixed"
  ) +
  gghighlight::gghighlight(
    unhighlighted_params = list(linewidth = rel(0.3), colour = "#99999980"),
    use_direct_label = FALSE
  ) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    plot.tag = ggtext::element_textbox_simple(
      family = body_font,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      maxwidth = 0.5,
      lineheight = 1.15
    ),
    plot.tag.position = c(0.45, 0.125),
    strip.text = ggtext::element_markdown(
      size = rel(0.8),
      face = "bold",
      padding = margin(8, 0, 8, 10),
      lineheight = 1.3
    ),
    axis.text.x = element_text(size = rel(0.65)),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "cm"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  ggview::canvas(width = 7, height = 5) -> fig

# Export -----------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-02-24/20260224.png"))
