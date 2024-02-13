library(data.table)
library(tidyverse)

#read in existing savant data // currently contains to end of 2023 season
savantData <- fread("/Users/ajaypatel/Downloads/SavantData.csv")

#filter to fastballs only
ff <- savantData %>% 
  filter(pitch_type == 'FF', !is.na(plate_x), !is.na(release_speed), !is.na(delta_run_exp))

#remove SavantData for memory
rm(savantData)

#adjust lefty numbers
ff <- ff %>% 
  mutate(pfx_x = if_else(p_throws == 'L', -pfx_x, pfx_x),
         release_pos_x = if_else(p_throws == 'L', -release_pos_x, release_pos_x))

#define features and target
variables <- c("game_year", "release_pos_x", "release_pos_z", "release_speed", 
               "vaa", "pfx_x", "pfx_z", "delta_run_exp")

# Create a matrix of features and a vector for the target variable
model_data <- ff %>% 
  select(all_of(variables))

#split data based on game year
train_data <- model_data[game_year < 2023, ]
test_data <- model_data[game_year == 2023, ]
#assign labels
train_labels <- model_data[game_year < 2023]$delta_run_exp
test_labels <- model_data[game_year == 2023]$delta_run_exp

#load in xgboost
library(xgboost)

xgb_model <- xgboost(
  data = as.matrix(train_data[, 2:7]), 
  label = train_labels, 
  nrounds = 100, 
  objective = "reg:squarederror")

#importance matrix for the model
importance_matrix <- xgb.importance(colnames(train_data[, 2:7]), model = xgb_model)
xgb.plot.importance(importance_matrix)

#make predictions
predictions <- predict(xgb_model, as.matrix(test_data[, 2:7]))  

#add predictions to ff dataframe
ff_2023 <- ff %>%
  filter(game_year == 2023) %>% 
  mutate(xrv = predictions) 

#rescale so it's on a stuff+ scale
library(psych)

ff_2023 <- ff_2023 %>% 
  #$ is to extract the values // psych packages stores it as a dataframe
  mutate(xrv_scaled = psych::rescale(-xrv, mean = 100, sd = 50)$t.t.scale.x.....sd...mean.)

#remove other dataframes for memory
rm(ff, model_data, test_data, train_data)

#leaders and laggers in fastball stuff 
library(gt)
library(gtExtras)
library(mlbplotR)

table <- ff_2023 %>% 
  group_by(pitcher_name, pitcher) %>% 
  #summarise xrv
  summarise(rel_x = mean(release_pos_x),
            rel_z = mean(release_pos_z),
            velo = mean(release_speed),
            hb = mean(pfx_x), 
            ivb = mean(pfx_z) * 12,
            vaa = mean(vaa),
            stuff = mean(xrv_scaled, na.rm = TRUE),
            pitches = n()) %>% 
  filter(pitches >= 750) %>% 
  arrange(desc(stuff)) %>% 
  ungroup() %>% 
  #show best 5 and worst 5
  gt_preview(top_n = 5, bottom_n = 5) %>% 
  #add color formatting
  gt_hulk_col_numeric(columns = 'stuff') %>% 
  fmt_number(columns = 'stuff') %>% 
  cols_label(
    stuff = 'Stuff+',
    pitcher_name = "Pitcher",
    pitcher = '',
    pitches = "Pitches"
  ) %>% 
  cols_move(pitches, after = pitcher) %>% 
  mlbplotR::gt_fmt_mlb_dot_headshot(columns = pitcher) %>% 
  #title/subtitle
  tab_header(title = "Best Fastballs in 2023",
             subtitle = "Min 750 Thrown | @ajaypatel8_ | X Values Are Standardized For L/R") %>% 
  gt_theme_538() %>% 
  fmt_number(columns = c('rel_x', 'rel_z', 'velo', 'hb', 'ivb', 'vaa'), decimals = 1)

gtsave(table, 'ff_stuff_table.png')  

#by inning analysis
ff_innings <- ff_2023 %>% 
  group_by(pitcher_name, pitcher, inning) %>% 
  summarise(stuff = mean(xrv_scaled), pitches = n()) 

#filter to only pitchers who have pitched in the first
first <- unique(ff_innings$pitcher[ff_innings$inning == 1])

ff_innings <- ff_innings[ff_innings$pitcher %in% first, ]

#ungroup dataframe
ff_innings <- ff_innings %>% ungroup()

library(ggthemes)

#bryce miller by inning
bryce <- ggplot(ff_innings %>% filter(pitcher_name == 'Bryce Miller'), aes(x = inning, y = stuff)) +
  geom_point(aes(size = pitches, fill = 'SEA', color = 'SEA'), shape = 21) +
  xlim(0, 9) +
  ggthemes::theme_clean() +
  labs(
    title = "Bryce Miller - Fastball Stuff By Inning",
    x = "Inning",
    y = "Stuff+",
    caption = "@ajaypatel8_",
    tag = 682243
  ) +
  scale_fill_mlb(type = 'secondary') +
  scale_color_mlb() +
  theme(
    plot.tag = mlbplotR::element_mlb_dot_headshot(size = 5, hjust = 1, vjust = 1),
    plot.tag.position = c(1, 1)
  )  +
  #manually set the scale for x-axis
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

#spencer strider by inning
spencer <- ggplot(ff_innings %>% filter(pitcher_name == 'Spencer Strider'), aes(x = inning, y = stuff)) +
  geom_point(aes(size = pitches, fill = 'ATL', color = 'ATL'), shape = 21) +
  xlim(0, 9) +
  ggthemes::theme_clean() +
  labs(
    title = "Spencer Strider - Fastball Stuff By Inning",
    x = "Inning",
    y = "Stuff+",
    caption = "@ajaypatel8_",
    tag = 675911
  ) +
  scale_fill_mlb(type = 'secondary') +
  scale_color_mlb() +
  theme(
    plot.tag = mlbplotR::element_mlb_dot_headshot(size = 5, hjust = 1, vjust = 1),
    plot.tag.position = c(1, 1)
  )  +
  #manually set the scale for x-axis
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

#save plots
ggsave("bryce.png", bryce, dpi = 'retina', width = 8, height = 8)
ggsave("spencer.png", spencer, dpi = 'retina', width = 8, height = 8)
