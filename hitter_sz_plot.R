library(devtools)
library(baseballr)
library(tidyverse)

date401407 <- baseballr::scrape_statcast_savant(start_date = '2021-04-01', end_date = '2021-04-07',
                                                player_type = "batter")

date408414 <- baseballr::scrape_statcast_savant(start_date = '2021-04-08', end_date = '2021-04-14',
                                                player_type = "batter")

date415421 <- baseballr::scrape_statcast_savant(start_date = '2021-04-15', end_date = '2021-04-21',
                                                player_type = "batter")

date422428 <- baseballr::scrape_statcast_savant(start_date = '2021-04-22', end_date = '2021-04-28',
                                                player_type = "batter")

date429505 <- baseballr::scrape_statcast_savant(start_date = '2021-04-29', end_date = '2021-05-05',
                                                player_type = "batter")

date506512 <- baseballr::scrape_statcast_savant(start_date = '2021-05-06', end_date = '2021-05-12',
                                                player_type = "batter")

date513519 <- baseballr::scrape_statcast_savant(start_date = '2021-05-13', end_date = '2021-05-19',
                                                player_type = "batter")

date520526 <- baseballr::scrape_statcast_savant(start_date = '2021-05-20', end_date = '2021-05-26',
                                                player_type = "batter")

date527602 <- baseballr::scrape_statcast_savant(start_date = '2021-05-27', end_date = '2021-06-02',
                                                player_type = "batter")

date603609 <- baseballr::scrape_statcast_savant(start_date = '2021-06-03', end_date = '2021-06-09',
                                                player_type = "batter")

date610616 <- baseballr::scrape_statcast_savant(start_date = '2021-06-10', end_date = '2021-06-16',
                                                player_type = "batter")

date617623 <- baseballr::scrape_statcast_savant(start_date = '2021-06-17', end_date = '2021-06-23',
                                                player_type = "batter")

date624630 <- baseballr::scrape_statcast_savant(start_date = '2021-06-24', end_date = '2021-06-30',
                                                player_type = "batter")

date701707 <- baseballr::scrape_statcast_savant(start_date = '2021-07-01', end_date = '2021-07-07',
                                                player_type = "batter")

date708714 <- baseballr::scrape_statcast_savant(start_date = '2021-07-08', end_date = '2021-07-14',
                                                player_type = "batter")

date715721 <- baseballr::scrape_statcast_savant(start_date = '2021-07-15', end_date = '2021-07-21',
                                                player_type = "batter")

date722728 <- baseballr::scrape_statcast_savant(start_date = '2021-07-22', end_date = '2021-07-28',
                                                player_type = "batter")

date729804 <- baseballr::scrape_statcast_savant(start_date = '2021-07-29', end_date = '2021-08-04',
                                                player_type = "batter")

date805811 <- baseballr::scrape_statcast_savant(start_date = '2021-08-05', end_date = '2021-08-11',
                                                player_type = "batter")

date812818 <- baseballr::scrape_statcast_savant(start_date = '2021-08-12', end_date = '2021-08-18',
                                                player_type = "batter")

date819825 <- baseballr::scrape_statcast_savant(start_date = '2021-08-19', end_date = '2021-08-25',
                                                player_type = "batter")

date826901 <- baseballr::scrape_statcast_savant(start_date = '2021-08-26', end_date = '2021-09-01',
                                                player_type = "batter")

date902908 <- baseballr::scrape_statcast_savant(start_date = '2021-09-02', end_date = '2021-09-08',
                                                player_type = "batter")

date909915 <- baseballr::scrape_statcast_savant(start_date = '2021-09-09', end_date = '2021-09-15',
                                                player_type = "batter")

date916922 <- baseballr::scrape_statcast_savant(start_date = '2021-09-16', end_date = '2021-09-22',
                                                player_type = "batter")

date923929 <- baseballr::scrape_statcast_savant(start_date = '2021-09-23', end_date = '2021-09-29',
                                                player_type = "batter")


date9301003 <- baseballr::scrape_statcast_savant(start_date = '2021-09-30', end_date = '2021-10-03',
                                                 player_type = "batter")


#combine dfs (rbind combines rows with same columns)
SavantData21 <- rbind(date401407, date408414, date415421, date422428, date429505, date506512,
                      date513519, date520526, date527602, date603609, date610616, date617623,
                      date624630, date701707, date708714, date715721, date722728, date729804,
                      date805811, date812818, date819825, date826901, date902908, date909915,
                      date916922, date923929, date9301003)

View(SavantData21)

SavantObs = SavantData21[sample(nrow(SavantData21),10),]

SavantData21$pitch_name

SavantData21 %>% 
  select(player_name, events, launch_speed, release_speed, pitch_name) %>% 
  filter(events == "home_run", pitch_name == "Sinker") %>% 
  group_by(player_name) %>% 
  dplyr::summarise(HR_SIs = n(), AvgEV = mean(launch_speed)) %>% 
  arrange(desc(HR_SIs))

#
baseballr::playerid_lookup(last_name = "Stanton", first_name = "Giancarlo")


giancarlo <- SavantData21 %>% 
  filter(batter == 519317) %>% 
  group_by(pitch_type) %>% 
  mutate(num_pitch = n()) %>% 
  #so not every single pitch
  filter(num_pitch >= 50)

#create strike zone
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)

#store in dataframe
sz <- data_frame(x,z)


giancarlo <- ggplot() +
  geom_hex(data = giancarlo, aes(x = plate_x, y = plate_z, color = pitch_type, alpha = 0.6)) +
  geom_path(data = sz, aes(x = x, y = z)) +
  theme_ajay() +
  facet_wrap(~pitch_type, nrow = 2) +
  labs(
    title = "Giancarlo Stanton's Pitch Chart",
    subtitle = "Min 50 Each Pitches | 2021",
    caption = "Data: baseballR | @ajaypatell8",
    x = "Feet From Home Plate On X Axis",
    y = "Feet From Home Plate On Y Axis"
  ) +
  theme(legend.position = "none")

ggsave("stanton.png", giancarlo, dpi = "retina", width = 8, height = 8)

