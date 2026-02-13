# Code for the 2024-09-23 TidyTuesday dataset
# In this code we look at the positional movements from a large random sample
# of the data
#
# The below code is heavily inspired by @https://github.com/jbkunst
# ------------------------------------------------------------------------------
rm(list = ls())

library(dplyr)
library(ggplot2)
library(patchwork)
library(furrr)

library(rchess) # For working with chess objects

plan(multisession, workers = parallel::detectCores() - 1)

# Convert moves into PGN format ------------------------------------------------
convert_to_pgn <- function(moves, game_id) {
  move_list <- strsplit(moves, " ")[[1]]

  # Generate the PGN format by numbering the moves
  pgn <- ""

  for (i in seq(1, length(move_list), by = 2)) {
    move_number <- (i + 1) / 2
    if (i < length(move_list)) {
      pgn <- paste0(
        pgn,
        move_number,
        ". ",
        move_list[i],
        " ",
        move_list[i + 1],
        " "
      )
    } else {
      pgn <- paste0(pgn, move_number, ". ", move_list[i])
    }
  }

  # Returning pgn string
  return(pgn)
}

# Processing moves (rchess package) --------------------------------------------
process_moves <- function(p) {
  chss <- Chess$new()
  chss$load_pgn(p)
  chss$history_detail()
}

# Reading in data --------------------------------------------------------------
data <- tidytuesdayR::tt_load(2024, week = 40)
data <- data$chess

# Data processing --------------------------------------------------------------
# Converting moves into pgn format
chess_games <- data %>%
  select(game_id, moves) %>%
  mutate(
    game_id = seq(1:nrow(data)),
    moves = mapply(convert_to_pgn, moves, game_id)
  ) # pgn conversion function

# Converting to game history
chess_games <- chess_games %>%
  mutate(data = future_map(moves, process_moves)) %>%
  select(-moves) %>%
  tidyr::unnest(cols = c(data))

# Creating a chess board -------------------------------------------------------
board <- rchess:::.chessboarddata() %>%
  select(cell, col, row, x, y, cc)

# Join board data with game data to get move origin and destination
chess_games_paths <- chess_games %>%
  left_join(
    board %>% rename(from = cell, x.from = x, y.from = y),
    by = "from"
  ) %>%
  left_join(
    board %>% rename(to = cell, x.to = x, y.to = y) %>% select(-cc, -col, -row),
    by = "to"
  ) %>%
  mutate(
    x_gt_y = abs(x.to - x.from) > abs(y.to - y.from), # Check if x movement is greater than y
    xy_sign = sign((x.to - x.from) * (y.to - y.from)) == 1, # Check sign product of x and y movement
    x_gt_y_equal_xy_sign = x_gt_y == xy_sign
  ) # Check if both conditions hold

# Major pieces by colour -------------------------------------------------------
pieces <- c(
  "a1 Rook",
  "b1 Knight",
  "c1 Bishop",
  "White Queen",
  "White King",
  "f1 Bishop",
  "g1 Knight",
  "h1 Rook"
)

# Filter paths to only include major pieces
# Using every move from the dataset produces a messy overcrowded plot
# So instead we will sample 25000 random moves
chess_games_paths_white <- chess_games_paths %>%
  filter(piece %in% pieces) %>%
  sample_n(25000)

# Plotting white pathways
pathways_white <- chess_games_paths_white %>%
  ggplot() +
  # Adding board data
  geom_tile(data = board, aes(x, y, fill = cc)) +

  # Adding positional movements
  geom_curve(
    data = chess_games_paths_white %>% filter(x_gt_y_equal_xy_sign),
    aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
    position = position_jitter(width = 0.2, height = 0.2),
    curvature = 0.50,
    angle = -45,
    alpha = 0.02,
    color = "white",
    linewidth = 1.02
  ) +
  geom_curve(
    data = chess_games_paths_white %>% filter(!x_gt_y_equal_xy_sign),
    aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
    position = position_jitter(width = 0.2, height = 0.2),
    curvature = -0.50,
    angle = 45,
    alpha = 0.02,
    color = "white",
    linewidth = 1.02
  ) +

  # Customising
  labs(title = "Positional movements of major chess pieces", x = "", y = "") +
  scale_fill_manual(values = c("burlywood3", "burlywood4")) +
  coord_equal() +
  facet_wrap(~ factor(piece, c(white_pieces)), ncol = 4) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 12,
      face = "bold",
      margin = margin(0, 0, 15, 0)
    ),
    strip.text = element_text(
      size = 8,
      face = "bold",
      margin = margin(0, 0, 5, 0)
    ),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
  ) +
  ggeasy::easy_remove_legend() +
  ggeasy::easy_remove_gridlines()

# Exporting --------------------------------------------------------------------
cowplot::save_plot(
  filename = file.path(this.path::this.dir(), "major_piece_movement.png"),
  plot = pathways_white,
  base_width = 9.5
)
