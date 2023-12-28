library(baseballr)
library(data.table)
library(tidyverse)
library(lubridate)

#read in existing savant data // currently contains to end of 2023 season
savantData <- fread("/Users/ajaypatel/Downloads/SavantData.csv")

#read in new data (these should be the new dates we want to add to the df)
data <- baseballr::scrape_statcast_savant(start_date = "2023-04-05", end_date = "2023-04-05")

#add in year/month/day
data <- data %>% 
  mutate(game_date = as.Date(game_date),
         year = year(game_date),
         month = month(game_date),
         day = day(game_date))

#relocate cols
data <- data %>% 
  relocate(c(year, month, day), .after = game_date)

#add in batter and pitcher names
mlbplayerids <- baseballr::get_chadwick_lu()

mlbnames <- mlbplayerids %>% 
  select(key_mlbam, name_first, name_last) %>% 
  mutate(name = paste(name_first, name_last, sep = " ")) %>% 
  select(-name_first, -name_last)

#join names to data
data <- left_join(data, mlbnames, by = c("pitcher" = "key_mlbam")) %>% 
  rename(batter_name = name)

data <- left_join(data, mlbnames, by = c("batter" = "key_mlbam")) %>% 
  rename(pitcher_name = name)

#save a little memory
rm(mlbnames, mlbplayerids)

#add in spray angle
data <- data %>% 
  mutate(spray_angle_est = atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75)

#add in hit direction
data <- data %>% 
  mutate(hit_direction = case_when(
    stand == "R" & spray_angle_est < -15 | stand == "L" & spray_angle_est > 15 ~ "Pull",
    spray_angle_est >= -15 & spray_angle_est <= 15 ~ "Center",
    stand == "R" & spray_angle_est > 15 | stand == "L" & spray_angle_est < -15 ~ "Oppo"))

#convert date type to bind
savantData$game_date <- as.Date(savantData$game_date)

#bind data
savantData <- rbind(savantData, data)

#save csv
write_csv(savantData, "/Users/ajaypatel/Downloads/SavantData.csv")

