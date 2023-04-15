library(tidyverse)
library(ggplot2)
library(baseballr)

data1 <- baseballr::scrape_statcast_savant(start_date = Sys.Date()-1, end_date = Sys.Date()-1)

mlbplayerids <- baseballr::chadwick_player_lu()

player_id_df <- mlbplayerids %>% 
  filter(name_first == "Justin", name_last == "Steele", !is.na(key_mlbam))

player_id <- as.vector(player_id_df)

player <- data1 %>% 
  filter(pitcher == player_id_df$key_mlbam)

#create strike zone
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)


sz <- tibble(x,z)

#smaller quadrants within k zone
x1 <- c(-.317, -.317)
z1 <- c(1.6, 3.5)

sz1 <- tibble(x1, z1)

x2 <- c(0.316, 0.316)
z2 <- c(1.6, 3.5)

sz2 <- tibble(x2, z2)

x3 <- c(-.95, .95)
z3 <- c(2.233, 2.233)

sz3 <- tibble(x3, z3)

x4 <- c(-.95, .95)
z4 <- c(2.866, 2.866)

sz4 <- tibble(x4, z4)


player_plot <- ggplot() +
  geom_point(data = player, aes(x = plate_x, y = plate_z, color = pitch_type), 
             size = 2.5) +
  stat_ellipse(data = player, aes(x = plate_x, y = plate_z, color = pitch_type,
                                  fill = pitch_type), geom = "polygon",
               level = 0.4, alpha = 0.1) +
  geom_path(data = sz, aes(x = x, y = z)) +
  geom_path(data = sz1, aes(x = x1, y = z1), linetype = "dashed", alpha = 0.6) +
  geom_path(data = sz2, aes(x = x2, y = z2), linetype = "dashed", alpha = 0.6) +
  geom_path(data = sz3, aes(x = x3, y = z3), linetype = "dashed", alpha = 0.6) +
  geom_path(data = sz4, aes(x = x4, y = z4), linetype = "dashed", alpha = 0.6) +
  theme_minimal() +
  labs(
    title = paste(player_id$name_first," ", player_id$name_last, "'s Pitch Chart", sep = ''),
    caption = "Data: baseballR | @ajaypatell8",
    subtitle = "4/14 vs. LAD",
    x = "Catcher's Perspective (Feet)",
    y = "Feet From Home Plate On Y Axis"
  ) +
  xlim(-2.5, 2.5) +
  coord_equal() +
  theme(legend.position = "top")

ggsave("pitcher_pitch.png", player_plot, dpi = "retina", bg = "white", height = 6, width = 8)


