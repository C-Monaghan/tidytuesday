# Loading packages -------------------------------------------------------------
pacman::p_load(
  tidyverse,        # Easily Install and Load the 'Tidyverse'
  ggtext,           # Improved Text Rendering Support for 'ggplot2'
  showtext,         # Using Fonts More Easily in R Graphs
  ggeasy,           # Makes theming plots easier
  glue,             # Interpreted String Literals
  ggfx              # Pixel Filters for "ggplot2" and "grid"
)

# Visualization Parameters -----------------------------------------------------
# Plot aesthetics
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"  

# Icons
tt <- str_glue("#TidyTuesday: { 2024 } Week { 41 } &bull; Source: National Park Species<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")

# Text
title_text <- str_glue("Abundance of birds per national park")
caption_text  <- str_glue("{tt} {li} c-monaghan &bull; {gh} c-monaghan &bull; #rstats #ggplot2")

# Fonts
font_add("fa6-brands", here::here("fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf"))
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
font_add_google("Merriweather Sans", regular.wt = 400, family = "text")
showtext_auto(enable = TRUE)

# Theme
theme_set(theme_minimal(base_size = 14))

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  plot.background       = element_rect(fill = "white", colour = "white"),
  panel.background      = element_rect(fill = "white", colour = "white"),
  panel.grid            = element_blank(),
  panel.grid.major.x    = element_blank(),
  axis.text.x           = element_text(size = 12, family = "text"),
  axis.text.y           = element_blank(),
  legend.position       = "bottom"
)

# Reading in data --------------------------------------------------------------
data <- tidytuesdayR::tt_load(2024, week = 41)

species <- data$most_visited_nps_species_data

# Getting abundance of species per park ----------------------------------------
bird_abundance <- species %>%
  select(ParkName, CategoryName) %>%
  group_by(ParkName, CategoryName) %>%
  summarise(Abundance = n()) %>%
  rename(Park_name = ParkName, Species = CategoryName)  %>%
  mutate(Park_name = as.factor(Park_name)) %>%
  filter(Species == "Bird")

# Plotting ---------------------------------------------------------------------
bird_abundance_plot <- bird_abundance %>%
  ggplot() +
  geom_hline(
    data = data.frame(y = c(0:4) * 125), 
    aes(yintercept = y), 
    color = "lightgrey") + 
  geom_col(
    aes(x = reorder(str_wrap(Park_name, 16), Abundance),
        y = Abundance,
        fill = Abundance),
    position = "dodge2", show.legend = TRUE, alpha = .9) +
  coord_polar() + 
  scale_y_continuous(
    limits = c(-200, 500),
    expand = c(0, 0),
    breaks = c(0, 100, 200, 300, 400)
  ) + 
  scale_fill_gradientn(
    "Abundance",
    colours = c("#e9b91c","#db9a17","#ce7b12","#be471b", "#ae1324")
  ) +
  guides(
    fill = guide_colorsteps(
      barwidth = 15, 
      barheight = 1, 
      title.position = "top", 
      title.hjust = .5
    )
  ) +
  labs(
    title = title_text,
    caption = caption_text, 
    x = NULL,
    y = NULL) +
  theme(    
    # Title
    plot.title = element_text(
      size = rel(5),
      family = "title",
      face = "bold",
      colour = title_col,
      lineheight = 1.1,
      hjust = 0.5,
      margin = margin(t = 5, b = 5)),
    # Caption
    plot.caption = element_markdown(
      size = rel(1.25),
      family = "caption",
      colour = caption_col,
      lineheight = 1.1,
      hjust = 0.5,
      margin = margin(t = 5, b = 5)),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
    )

# Export -----------------------------------------------------------------------
export_path <- "./2024/2024-10-07/"

cowplot::save_plot(filename = file.path(export_path, "Bird_abundance.png"),
                   plot = bird_abundance_plot,
                   base_height = 5.5)

