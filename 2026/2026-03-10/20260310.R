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

# Preparing caption
socials <- cmBrand::social_brand()
tt_text <- cmBrand::tt_text(
  year = 2026,
  week = 10,
  source = "Adam Kucharski - How likely is 'likely'?"
)

caption <- stringr::str_glue(
  "{tt_text}{socials} &bull; #rstats #ggplot2"
)

# Tidytuesday data -------------------------------------------------------------
tt_data <- tidytuesdayR::tt_load(x = 2026, week = 10)

# Extracting individual datasets
absolute_judgements <- tt_data$absolute_judgements
pairwise_comparisons <- tt_data$pairwise_comparisons
respondent_metadata <- tt_data$respondent_metadata

# Data processing + modelling --------------------------------------------------
# Which term was selected between the pairwise comparisons
wins <- pairwise_comparisons |>
  mutate(
    winner = selected,
    loser = if_else(selected == term1, term2, term1)
  ) |>
  select(winner, loser)

# Fitting a Bradley Terry rank model
## Preparing data
bt_data <- BradleyTerry2::countsToBinomial(
  xtabs(~ winner + loser, data = wins)
)

# Fitting model
bt_model <- BradleyTerry2::BTm(
  outcome = cbind(win1, win2),
  player1 = player1,
  player2 = player2,
  data = bt_data
)

# Calculating ratings
bt_results <- BradleyTerry2::BTabilities(model = bt_model) |>
  tibble::as_tibble(rownames = "phrase") |>
  rename(score = ability)

# Creating tidy dataset for plotting -------------------------------------------
bt_plot_data <- bt_results |>
  mutate(
    # Arranging from highest to lowest
    phrase = forcats::fct_reorder(phrase, score),
    # Converting to probabilities
    prob = plogis(score),
    percent = prob * 100,
    # Calculating label positions
    x_pos = ifelse(percent > 50, prob - 0.2, prob + 0.2)
  )

# Plotting ---------------------------------------------------------------------
bt_plot_data |>
  ggplot(aes(x = prob, y = phrase, colour = as.numeric(prob))) +
  geom_vline(
    xintercept = c(0.25, 0.5, 0.75),
    colour = "grey80",
    linewidth = rel(0.25),
    linetype = "dashed"
  ) +
  geom_point(size = rel(3)) +
  scale_colour_gradientn(
    colours = RColorBrewer::brewer.pal(11, "RdBu"),
    values = c(0, 0.5, 1),
    guide = "none"
  ) +
  scale_x_continuous(limits = c(0, 1.05), labels = scales::percent_format()) +
  labs(
    title = "What Do Probability Phrases Really Mean?",
    subtitle = "Results derived from a Bradley-Terry model fitted on 50,000 pairwise  comparisons",
    x = "Perceived probability",
    y = NULL,
    caption = caption
  ) +
  theme_minimal(base_family = body_font, base_size = 10) +
  theme(
    plot.title = element_text(
      size = rel(1.4),
      family = title_font,
      face = "bold",
      margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = rel(0.9),
      margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")
    ),
    plot.caption = ggtext::element_markdown(
      size = rel(0.7),
      hjust = -2,
      halign = 0,
      lineheight = 1.25,
      colour = "grey30",
      margin = margin(t = 5, r = 0, b = 2, l = 0, unit = "pt")
    ),
    axis.title.x = element_text(size = rel(0.9), face = "bold"),
    axis.text.y = element_text(size = rel(0.9)),
    panel.grid = element_blank()
  ) +
  ggview::canvas(width = 5, height = 6) -> fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-03-10/20260310.png"))
