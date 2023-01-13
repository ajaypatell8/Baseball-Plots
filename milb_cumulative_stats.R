library(data.table)
library(tidyverse)
library(baseballr)

ids <- baseballr::chadwick_player_lu()

milb_hitter <- baseballr::milb_batter_game_logs_fg('20178', 2022)

milb_hitter$Date <- as.Date(as.character(milb_hitter$Date))

milb_hitter <- milb_hitter %>% 
  setorder(Date) %>% 
  mutate(game_number = row_number(),
         hr_cumul = cumsum(HR),
         sb_cumul = cumsum(SB),
         ops_cumul = cummean(OPS))

ggplot(milb_hitter, aes(x = game_number, y = ops_cumul)) +
  geom_line() +
  labs(x = 'Game Number',
       y = 'OPS',
       title = paste(milb_hitter$player_name,"- Cumulative OPS", sep = " "),
       subtitle = '2022',
       caption = 'Data: Fangraphs via baseballr | @ajaypatell8') +
  theme_minimal()
