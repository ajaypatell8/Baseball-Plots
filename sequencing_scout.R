library(data.table)
library(tidyverse)
library(baseballr)

#read in data
data <- fread("/Users/ajaypatel/Downloads/SavantData.csv")

#2023 only
data <- data %>% filter(game_year == 2023) %>% 
  #no missing pitches
  filter(!is.na(pitch_name))

#add xwOBA
data <- data %>% 
  mutate(player_results = case_when(
    !is.na(woba_value) & is.na(estimated_woba_using_speedangle) ~ woba_value,
    !is.na(estimated_woba_using_speedangle) ~ estimated_woba_using_speedangle,
    TRUE ~ NA
  ),
  batting_team = if_else(inning_topbot == "Top", away_team, home_team),
  pitching_team = if_else(inning_topbot == 'Top', home_team, away_team))

#define pitching team and hitting team
pitchingTeam <- "HOU"
team <- "NYY"

#get pitcher xwOBA against first three pitches
pitcher_seq <- data %>% 
  filter(pitching_team == pitchingTeam) %>% 
  group_by(game_date, pitcher_name, pitcher, at_bat_number) %>% 
  slice(1:3) %>% 
  mutate(pitches = paste(pitch_type, collapse = ','),
         xwOBA = first(player_results)) %>% 
  ungroup() %>% 
  select(-at_bat_number, -pitch_type, -player_results) %>% 
  unique() %>% 
  group_by(pitcher_name, pitcher, pitches) %>% 
  summarise(times_thrown = n(), xwOBA = mean(xwOBA))

#get batter xwOBA against first three pitches
batter_seq <- data %>% 
  filter(batting_team == team) %>% 
  group_by(game_date, batter_name, batter, at_bat_number) %>% 
  slice(1:3) %>% 
  mutate(pitches = paste(pitch_type, collapse = ','),
         xwOBA = first(player_results)) %>% 
  ungroup() %>% 
  select(-at_bat_number, -pitch_type, -player_results) %>% 
  unique() %>% 
  group_by(batter_name, batter, pitches) %>% 
  summarise(times_seen = n(), xwOBA = mean(xwOBA))

library(gt)
library(gtExtras)
library(mlbplotR)
library(bstfun)

#make opposing pitcher sequencing table--------------------------------------------------------
pitcherName <- "Justin Verlander"

game_pitcher_seq <- pitcher_seq %>% 
  filter(pitcher_name == pitcherName, times_thrown >= 20) %>% 
  arrange(-desc(xwOBA)) %>% 
  filter(row_number() <= 3 | row_number() >= n() -2)

pitcher_seq_table <- game_pitcher_seq %>% 
  ungroup() %>% 
  select(-pitcher_name) %>% 
  gt() %>% 
  gt_theme_538() %>% 
  cols_label(
    pitcher = '',
    pitches = "Sequence",
    times_thrown = "Times Thrown"
  ) %>% 
  gt_fmt_mlb_dot_headshot(columns = "pitcher") %>% 
  fmt_number(columns = xwOBA, decimals = 3) %>% 
  data_color(
    columns = c(xwOBA),
    colors = scales::col_numeric(
      palette = c(
        "blue", "white", "red"),
      domain = c(min(game_pitcher_seq$xwOBA), max(game_pitcher_seq$xwOBA)))
  )  %>% 
  tab_header(
    title = paste(pitcherName, "'s Three Best and Worst Sequences", sep = ''),
    subtitle = "2023 Only | Min 30 Times Thrown | @ajaypatel8_"
  ) %>% 
  tab_footnote(
    footnote = "Sequences Are Only The First Three Pitches of a PA",
    locations = cells_column_labels(columns = pitches)
  ) %>% 
  as_ggplot()

#make lineup's seq table------------------------------------------------------------------------------
lineup_1 <- c("DJ LeMahieu", "Aaron Judge", "Juan Soto")
            
lineup_2 <- c("Anthony Rizzo", "Gleyber Torres", "Alex Verdugo") 

lineup_3 <- c("Giancarlo Stanton", "Anthony Volpe", "Jose Trevino")

lineup_1 <- factor(lineup_1)
lineup_2 <- factor(lineup_2)
lineup_3 <- factor(lineup_3)


#pitcher sequences -> highlight these in tables
pitcher_sequences <- unique(game_pitcher_seq$pitches)

#get top three stats
top_three <- batter_seq %>% 
  filter(batter_name %in% lineup_1, times_seen >= 15) %>% 
  filter(!is.na(xwOBA)) %>% 
  arrange(desc(xwOBA)) %>% 
  filter(row_number() <= 3 | row_number() >= n() -2) %>% 
  #order according to lineup
  mutate(batter_name = factor(batter_name, levels = lineup_1)) %>% 
  arrange(batter_name)

#get middle three stats
middle_three <- batter_seq %>% 
  filter(batter_name %in% lineup_2, times_seen >= 15) %>% 
  filter(!is.na(xwOBA)) %>% 
  arrange(desc(xwOBA)) %>% 
  filter(row_number() <= 3 | row_number() >= n() -2) %>% 
  #order according to lineup
  mutate(batter_name = factor(batter_name, levels = lineup_2)) %>% 
  arrange(batter_name)

#get bottom three stats
bottom_three <- batter_seq %>% 
  filter(batter_name %in% lineup_3, times_seen >= 15) %>% 
  filter(!is.na(xwOBA)) %>% 
  arrange(desc(xwOBA)) %>% 
  filter(row_number() <= 3 | row_number() >= n() -2) %>% 
  #order according to lineup
  mutate(batter_name = factor(batter_name, levels = lineup_3)) %>% 
  arrange(batter_name)

#make the three tables
batter_seq_table1 <- top_three %>% 
  ungroup() %>% 
  gt(groupname_col = "batter_name") %>% 
  gt_theme_538() %>% 
  cols_label(
    batter = '',
    pitches = "Sequence",
    times_seen = "Times Seen"
  ) %>% 
  gt_fmt_mlb_dot_headshot(columns = "batter") %>% 
  fmt_number(columns = xwOBA, decimals = 3) %>% 
  gt_hulk_col_numeric(columns = xwOBA) %>% 
  tab_footnote(
    footnote = "Sequences Are Only The First Three Pitches of a PA",
    locations = cells_column_labels(columns = pitches)
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = 'bold')
    ),
    locations = cells_body(
      columns = pitches,
      rows = pitches %in% pitcher_sequences
    )
  ) %>% 
  as_ggplot()

#middle three
batter_seq_table2 <- middle_three %>% 
  ungroup() %>% 
  gt(groupname_col = "batter_name") %>% 
  gt_theme_538() %>% 
  cols_label(
    batter = '',
    pitches = "Sequence",
    times_seen = "Times Seen"
  ) %>% 
  gt_fmt_mlb_dot_headshot(columns = "batter") %>% 
  fmt_number(columns = xwOBA, decimals = 3) %>% 
  gt_hulk_col_numeric(columns = xwOBA) %>% 
  tab_footnote(
    footnote = "Sequences Are Only The First Three Pitches of a PA",
    locations = cells_column_labels(columns = pitches)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = 'bold')
    ),
    locations = cells_body(
      columns = pitches,
      rows = pitches %in% pitcher_sequences
    )
  ) %>% 
  as_ggplot()

#bottom three
batter_seq_table3 <- bottom_three %>% 
  ungroup() %>% 
  gt(groupname_col = "batter_name") %>% 
  gt_theme_538() %>% 
  cols_label(
    batter = '',
    pitches = "Sequence",
    times_seen = "Times Seen"
  ) %>% 
  gt_fmt_mlb_dot_headshot(columns = "batter") %>% 
  fmt_number(columns = xwOBA, decimals = 3) %>% 
  gt_hulk_col_numeric(columns = xwOBA) %>% 
  tab_footnote(
    footnote = "Sequences Are Only The First Three Pitches of a PA",
    locations = cells_column_labels(columns = pitches)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = 'bold')
    ),
    locations = cells_body(
      columns = pitches,
      rows = pitches %in% pitcher_sequences
    )
  ) %>% 
  as_ggplot()

#gtsave(batter_seq_table, "/Users/ajaypatel/Downloads/RFiles/baseball/batter_seq.png")

#use patchwork to combine tables
library(patchwork)

#final plot
final <- pitcher_seq_table / (batter_seq_table1 + batter_seq_table2 + batter_seq_table3) +
  plot_annotation(
    #usually use sys.Date()
    title = paste("Sequencing Scout For", team, "On 2024-03-28"),
    subtitle = "Data: @baseballR | @ajaypatel8_",
    caption = paste(team),
    theme = theme(plot.title = element_text(family = "Arial", face = 'italic'),
                  plot.subtitle = element_text(face = "italic", size = 8),
                  plot.caption = element_mlb_logo())
  )

ggsave("/Users/ajaypatel/Downloads/RFiles/baseball/sequencing/seq_scout.png", final,
       dpi = 'retina', width = 5, height = 8)


  
