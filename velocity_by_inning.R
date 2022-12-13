library(tidyverse)
library(ggplot2)
library(readr)

savantData <- read_csv("SavantData.csv")
mlbplayerids <- baseballr::get_chadwick_lu()

id <- mlbplayerids %>% 
  filter(name_given == "George Joseph", name_last == "Kirby")


plot <- savantData %>% 
  filter(game_year == 2022, pitcher == id$key_mlbam, !is.na(release_speed)) %>% 
  group_by(inning, pitch_name) %>% 
  summarise(avg_velo = mean(release_speed), pitches = n()) %>% 
  ggplot(aes(x = inning, y = avg_velo)) +
  geom_point(aes(size = pitches, color = pitch_name)) +
  theme_ajay() +
  ggtitle(paste(sep = '', id$name_first, ' ', id$name_last, "'s Velocity By Inning")) +
  labs(
    x = "Inning",
    y = "Velocity",
    caption = "Data: BaseballSavant | @ajaypatell8"
  ) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))

ggsave("velocity_inning.png", plot, dpi = 'retina', height = 8, width = 8)
