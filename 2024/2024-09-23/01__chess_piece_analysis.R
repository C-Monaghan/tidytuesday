rm(list = ls())

library(dplyr)
library(ggplot2)
library(patchwork)

library(rchess) # For working with chess objects

library(furrr)

plan(multisession, workers = parallel::detectCores() - 1)

# Convert moves into PGN format ------------------------------------------------
convert_to_pgn <- function(moves, game_id) {
  
  move_list <- strsplit(moves, " ")[[1]]
  
  # Generate the PGN format by numbering the moves
  pgn <- ""
  
  for (i in seq(1, length(move_list), by = 2)) {
    move_number <- (i + 1) / 2
    if (i < length(move_list)) {
      pgn <- paste0(pgn, move_number, ". ", move_list[i], " ", move_list[i+1], " ")
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
    moves = mapply(convert_to_pgn, moves, game_id)) # pgn conversion function

# Converting to game history
chess_games <- chess_games %>%
  # filter(game_id %in% c(1:100)) %>%
  mutate(data = future_map(moves, process_moves)) %>%
  select(-moves) %>% 
  tidyr::unnest(cols = c(data))

# Creating a chess board -------------------------------------------------------
board <- rchess:::.chessboarddata() %>%
  select(cell, col, row, x, y, cc)

# Join board data with game data to get move origin and destination 
paths <- chess_games %>%
  left_join(
    board %>% rename(from = cell, x.from = x, y.from = y),
    by = "from"
  ) %>%
  left_join(
    board %>% 
      rename(to = cell, x.to = x, y.to = y) %>% 
      select(-cc, -col, -row),
    by = "to"
  ) %>%
  mutate(
    x_gt_y = abs(x.to - x.from) > abs(y.to - y.from), # Check if x movement is greater than y
    xy_sign = sign((x.to - x.from)*(y.to - y.from)) == 1, # Check sign product of x and y movement
    x_gt_y_equal_xy_sign = x_gt_y == xy_sign) # Check if both conditions hold

# Major pieces by colour -------------------------------------------------------
white_pieces <- c(
"a1 Rook", "b1 Knight", "c1 Bishop", "White Queen", 
"White King", "f1 Bishop", "g1 Knight", "h1 Rook")

black_pieces <- c(
  "a8 Rook", "b8 Knight", "c8 Bishop", "Black Queen", 
  "Black King", "f8 Bishop", "g8 Knight", "h8 Rook"
)

# Filter paths to only include major pieces
paths_pieces_white <- paths %>% 
  filter(piece %in% white_pieces) %>%
  sample_n(25000)

paths_pieces_black <- paths %>% 
  filter(piece %in% black_pieces) %>%
  sample_n(25000)

# Plotting white pathways
pathways_white <- paths_pieces_white %>%
  ggplot() +
  geom_tile(data = board, aes(x, y, fill = cc)) + # Chessboard tiles
  geom_curve(
    data = paths_pieces_white %>% filter(x_gt_y_equal_xy_sign),
    aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
    position = position_jitter(width = 0.2, height = 0.2), # Prevent overlapping
    curvature = 0.50,
    angle = -45,
    alpha = 0.02,
    color = "white",
    size = 1.02) +
  geom_curve(
    data = paths_pieces_white %>% filter(!x_gt_y_equal_xy_sign),
    aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
    position = position_jitter(width = 0.2, height = 0.2),
    curvature = -0.50,
    angle = 45,
    alpha = 0.02,
    color = "white",
    size = 1.02
  ) +
  labs(title = "White", x = "", y = "") +
  scale_fill_manual(values =  c("burlywood3", "burlywood4")) + # Traditional board colours
  coord_equal() +
  facet_wrap(~ factor(piece, c(white_pieces)), ncol = 4) +
  theme_void(base_family = "Segou UI") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
    strip.text = element_text(size = 6, face = "bold"),
    panel.grid = element_line(color = "gray70"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    plot.margin = margin(4, 4, 4, 4)
  ) +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend() +
  ggeasy::easy_remove_gridlines()

# Plotting black
pathways_black <- paths_pieces_black %>%
  ggplot() +
  geom_tile(data = board, aes(x, y, fill = cc)) + # Chessboard tiles
  geom_curve(
    data = paths_pieces_black %>% filter(x_gt_y_equal_xy_sign),
    aes(
      x = x.from,
      y = y.from,
      xend = x.to,
      yend = y.to
    ),
    position = position_jitter(width = 0.2, height = 0.2), # Prevent overlapping
    curvature = 0.50,
    angle = -45,
    alpha = 0.02,
    color = "white",
    size = 1.02
  ) +
  geom_curve(
    data = paths_pieces_black %>% filter(!x_gt_y_equal_xy_sign),
    aes(
      x = x.from,
      y = y.from,
      xend = x.to,
      yend = y.to
    ),
    position = position_jitter(width = 0.2, height = 0.2),
    curvature = -0.50,
    angle = 45,
    alpha = 0.02,
    color = "white",
    size = 1.02
  ) +
  labs(title = "Black", x = "", y = "") +
  scale_fill_manual(values =  c("burlywood3", "burlywood4")) + # Traditional chess colours
  coord_equal() +
  facet_wrap(~ factor(piece, c(black_pieces)), ncol = 4) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
    strip.text = element_text(size = 6, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing = unit(0.1, "lines"),  # Reducing space between facets
    plot.margin = margin(4, 4, 4, 4)  # Tightening the plot margins
  ) +
  ggeasy::easy_remove_legend() +
  ggeasy::easy_remove_gridlines()

# Plotting together with patchwork
major_pathways <- pathways_white + pathways_black +
  plot_annotation(
    title = "Movement of Major Chess Pieces: White vs Black",
    subtitle = "Paths for each major chess piece across multiple games",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic")
      ))

# Exporting --------------------------------------------------------------------
export_path <- "./2024/2024-09-23/"

cowplot::save_plot(
  filename = file.path(export_path, "major_piece_movement.png"),
  plot = major_pathways, base_width = 9.5)

