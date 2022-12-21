library(tidyverse)
library(ggplot2)
library(baseballr)
library(readr)

savantData <- read_csv("SavantData.csv")

mlbplayerids <- baseballr::chadwick_player_lu()

strip_id <- mlbplayerids %>% 
  filter(name_first == "Pierce", name_last == "Johnson")

strip_id <- as.vector(strip_id)

pitcher_chart <- savantData %>% 
  filter(pitcher == strip_id$key_mlbam) %>% 
  filter(!is.na(plate_x)) %>% 
  mutate(pfx_z = pfx_z * 12, pfx_x = pfx_x *12) %>% 
  select(player_name, pitcher, plate_x, plate_z, pfx_x, pfx_z, pitch_type, pitch_name, game_year) %>%
  ggplot(aes(x = pfx_x, y = pfx_z, group = pitch_name)) +
  facet_wrap(~game_year) +
  stat_ellipse(aes(x = pfx_x, y = pfx_z, fill = pitch_name), geom = "polygon", level = 0.6) +
  theme_ajay() +
  ggtitle(paste(strip_id$name_first," ", strip_id$name_last, "'s Movement Chart", sep = '')) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(x = "Horizontal Movement",
       y = "Vertical Movement",
       caption = 'Data: BaseballSavant | @ajaypatell8') +
  ylim(-20, 20) +
  xlim(-20, 20) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")

ggsave("pitcher_chart.png", dpi = 'retina', width = 8, height = 8)
