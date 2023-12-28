library(baseballr)
library(data.table)
library(tidyverse)
library(gt)
library(mlbplotR)
library(gtExtras)

#read in existing savant data // currently contains to end of 2023 season
savantData <- fread("/Users/ajaypatel/Downloads/SavantData.csv")

#just 2023, only bbes
savantData <- savantData %>% 
  filter(game_year == 2023, type == 'X')

#add in pulled flyballs column
savantData <- savantData %>% 
  mutate(pulled_flyball = if_else(hit_direction == "Pull" & launch_angle >= 25 & launch_angle <= 50,
                                  1, 0))

#find pulled flyball rate
pfb <- savantData %>% 
  filter(!is.na(pulled_flyball)) %>% 
  group_by(batter_name, batter) %>% 
  summarise(pfb_rate = sum(pulled_flyball) / n() * 100, bbe = n())

#filter out below 50 bbe
pfb <- pfb %>% filter(bbe >= 50) %>% arrange(desc(pfb_rate))

table <- pfb %>% 
  head(10) %>% 
  ungroup() %>% 
  gt::gt() %>% 
  mlbplotR::gt_fmt_mlb_headshot(columns = "batter") %>% 
  cols_label(
    batter_name = "Player",
    batter = "",
    pfb_rate = "Pulled Flyball%",
    bbe = "BBE"
  ) %>% 
  gtExtras::gt_theme_538() %>% 
  fmt_number(columns = c(pfb_rate), decimals = 2) %>% 
  tab_header(
    title = "2023 MLB Pulled Flyball Rate Leaders",
    subtitle = "Min 50 BBE | @ajaypatel8_"
  ) 

gtsave(table, "pfb_rate.png", vwidth = 900, vheight = 900)




