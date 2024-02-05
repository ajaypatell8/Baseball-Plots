library(tidyverse)
library(ggplot2)
library(baseballr)
library(ggpubr)
library(mlbplotR)
library(cfbplotR)
library(ggtext)
library(gt)
library(gtExtras)
library(bstfun)
library(patchwork)
library(data.table)

#be sure to set wd to source file
pitcher_summary_year <- function (player_first_name, player_last_name, season) {
  
  library(tidyverse)
  library(ggplot2)
  library(baseballr)
  library(ggpubr)
  library(mlbplotR)
  
  
  mlbplayerids <- read_csv("/Users/ajaypatel/Downloads/mlbplayerids.csv")
  
  player_id_df <- mlbplayerids %>% 
    filter(name_first == player_first_name, name_last == player_last_name, !is.na(key_mlbam),
           !is.na(key_fangraphs))
  
  player_id <- as.vector(player_id_df)
  
  data1 <- data.table::fread("/Users/ajaypatel/Downloads/SavantData.csv")
  
  player <- data1 %>% 
    filter(pitcher == player_id_df$key_mlbam, game_year == season)
  
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
  
  #for the plot
  player <- player %>% 
    filter(pitch_name != "NA", !is.na(pitch_name)) %>% 
    rename("Pitch"  = pitch_name) 
  
  #add player team
  player <- player %>% 
    mutate(opp_team = if_else(inning_topbot == "Top", away_team, home_team),
           team = if_else(inning_topbot == "Top", home_team, away_team))
  
  library(cfbplotR)
  library(ggtext)
  
  #get player headshots
  headshot <- mlbplotR::load_headshots() %>% filter(savant_id == player_id_df$key_mlbam) %>% 
    pull(espn_headshot)
  
  
  if(length(headshot) == 0 || is.na(headshot)) {
    headshot = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Major_League_Baseball_logo.svg/2560px-Major_League_Baseball_logo.svg.png"
  }
  
  loc_chart <- ggplot() +
    geom_point(data = player, aes(x = plate_x, y = plate_z, color = Pitch), 
               size = 2.5, alpha = 0.1) +
    stat_ellipse(data = player, aes(x = plate_x, y = plate_z, color = Pitch,
                                    fill = Pitch), geom = "polygon",
                 level = 0.4, alpha = 0.65) +
    geom_path(data = sz, aes(x = x, y = z)) +
    geom_path(data = sz1, aes(x = x1, y = z1), linetype = "dashed", alpha = 0.6) +
    geom_path(data = sz2, aes(x = x2, y = z2), linetype = "dashed", alpha = 0.6) +
    geom_path(data = sz3, aes(x = x3, y = z3), linetype = "dashed", alpha = 0.6) +
    geom_path(data = sz4, aes(x = x4, y = z4), linetype = "dashed", alpha = 0.6) +
    theme_minimal() +
    labs(
      title = "Location",
      x = "Catcher's Perspective (Feet)",
      y = "Feet From Home Plate On Y Axis"
    ) +
    xlim(-2.5, 2.5) +
    ylim(0, 5) +
    coord_equal() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle_image(title_image = headshot,
                  title = "Location",
                  image_height = 55) +
    theme_title_image(size = 14, hjust = 0.25, vjust = 0.25) 
  
  #ggsave("pitcher_pitch.png", loc_chart, dpi = "retina", bg = "white", height = 8, width = 8)
  
  #get team logo
  teams <- player %>% arrange(game_date) %>% slice(n()) %>% pull(team)
  
  teamlogo <- mlbplotR::load_mlb_teams() %>% filter(team_abbr == teams) %>% 
    pull(team_logo_espn)
  
  mov_chart <- player %>% 
    filter(pitcher == player_id_df$key_mlbam) %>% 
    filter(!is.na(plate_x)) %>% 
    mutate(pfx_z = pfx_z * 12, pfx_x = pfx_x *12) %>% 
    select(player_name, pitcher, plate_x, plate_z, pfx_x, pfx_z, Pitch) %>%
    ggplot(aes(x = pfx_x, y = pfx_z, group = Pitch)) +
    geom_point(aes(color = Pitch), alpha = 0.1) +
    stat_ellipse(aes(x = pfx_x, y = pfx_z, color = Pitch, fill = Pitch), geom = "polygon",
                 level = 0.6, alpha = 0.65)+
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(x = "Horizontal Movement (Inches)",
         y = "Induced Vertical Break") +
    ylim(-25, 25) +
    xlim(-25, 25) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    coord_equal() +
    ggtitle_image(title_image = teamlogo,
                  title = "Movement",
                  image_height = 55,
                  image_side = "right") +
    theme_title_image(size = 14, hjust = 0.25, vjust = 0.25)
  
  #ggsave("pitcher_chart.png", dpi = 'retina', width = 8, height = 8)
  
  #make box score table
  library(gt)
  library(gtExtras)
  library(bstfun)
  
  game_pks = unique(player$game_pk)
  
  column_names <- c("IP", "Pitches", "SO", "BB", "H", "ER", "PlayerName")
  
  summaries <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  colnames(summaries) <- column_names

  for(game_pk in game_pks) {
    summary <- baseballr::mlb_player_game_stats(person_id = player_id_df$key_mlbam, game_pk =game_pk) %>% 
      filter(group == "pitching") %>% 
      select(innings_pitched, number_of_pitches, strike_outs, base_on_balls, hits, earned_runs)
    
    summary <- summary %>% 
      mutate(PlayerName = paste(player_id_df$name_first, player_id_df$name_last)) %>% 
      rename("IP" = "innings_pitched",
             "Pitches" = "number_of_pitches",
             "SO" = "strike_outs",
             "BB" = "base_on_balls",
             "H" = "hits",
             "ER" = "earned_runs")

    summaries <- rbind(summaries, summary)    
  }
  
  #function to convert innings to fractional amount
  innings_to_fraction <- function(x) {
    parts <- as.numeric(strsplit(as.character(x), "\\.")[[1]])
    if (length(parts) == 1) {
      return(parts)
    } else {
      return(parts[1] + parts[2]/3)
    }
  }
  
  # Convert innings pitched to fractional representation
  summaries$IP <- sapply(summaries$IP, innings_to_fraction)
  
  #convert data type
  summaries$IP <- as.numeric((as.character(summaries$IP)))
  
  #basic box score stats
  summary <- summaries %>% 
    group_by(PlayerName) %>% 
    summarise(IP = sum(IP),
              Pitches = mean(Pitches),
              SO = sum(SO) / IP * 9,
              BB = sum(BB) / IP * 9,
              H = sum(H) / IP * 9,
              ER = sum(ER) / IP * 9) %>% 
    select(PlayerName, IP, Pitches, SO, BB, H, ER) %>% 
    gt()
  
  summary_bs <- summary$`_data`

  #defined vectors  
  csw_vector <- c("called_strike", "swinging_strike", "swinging_strike_blocked", "foul_tip")
  swings_vector <- c("hit_into_play", "swinging_strike", "swinging_strike_blocked", "foul_tip",
                     "foul")
  whiffs_vector <- c("swinging_strike", "swinging_strike_blocked", "foul_tip")
  
  #add xwOBA
  player <- player %>% 
    mutate(xwOBA = case_when(
      !is.na(woba_value) & is.na(estimated_woba_using_speedangle) ~ woba_value,
      !is.na(estimated_woba_using_speedangle) ~ estimated_woba_using_speedangle,
      TRUE ~ NA
    ))
  
  #stats from savant
  adv_summary <- player %>% 
    filter(!is.na(release_spin_rate)) %>% 
    group_by(Pitch) %>% 
    mutate(csw = (if_else(description %in% csw_vector, 1, 0)),
           swings = if_else(description %in% swings_vector, 1, 0),
           whiff = if_else(description %in% whiffs_vector, 1, 0)) %>% 
    summarise(used = n()/nrow(player),
              velocity = mean(release_speed), spin_rate = mean(release_spin_rate, na.rm = TRUE),
              csw = sum(csw) / n(),
              swings = sum(swings),
              whiffs = sum(whiff) / swings,
              ev_allowed = mean(launch_speed[type=="X"], na.rm = TRUE),
              xwOBA = mean(xwOBA, na.rm = TRUE)) %>% 
    select(-swings)
  
  table1 <- summary_bs %>% 
    gt() %>% 
    cols_label(
      PlayerName = "Pitcher",
      SO = "K/9",
      Pitches = "Pitches Per Outing",
      BB = "BB/9",
      H = "H/9",
      ER = "ERA"
    ) %>% 
    gt::fmt_number(columns = c(SO, BB, H, ER), decimals = 2) %>% 
    fmt_number(columns = c(Pitches, IP), decimals = 1) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = ER,
        rows = ER < 3.3
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = ER,
        rows = ER > 4.2
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = H,
        rows = H < 4.9
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = H,
        rows = H > 5.9
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = SO,
        rows = SO > 8.3
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = SO,
        rows = SO < 7
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = BB,
        rows = BB < 2.0
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = BB,
        rows = BB > 2.7
      )
    )  %>% 
    gt_theme_538() %>% 
    as_ggplot()
  
  
  table2 <- adv_summary %>% 
    arrange(-used) %>% 
    gt() %>% 
    cols_label(
      used = "% Used",
      velocity = "Velocity",
      spin_rate = "Spin Rate",
      csw = "CSW%",
      whiffs = "Whiff%",
      ev_allowed = "AVG Exit Velocity"
    ) %>% 
    cols_move(
      columns = ev_allowed,
      after = spin_rate
    ) %>% 
    cols_move(
      columns = whiffs,
      after = ev_allowed
    ) %>% 
    fmt_number(
      columns = everything(),
      decimals = 2
    ) %>% 
    fmt_number(
      columns = c(ev_allowed, velocity),
      decimals = 1
    ) %>% 
    fmt_number(
      columns = xwOBA,
      decimals = 3
    ) %>% 
    fmt_number(
      columns = spin_rate,
      decimals = 0
    ) %>% 
    fmt_percent(
      columns = c(used, whiffs, csw),
      decimals = 0
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = csw,
        rows = csw > 0.30
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color ="#E39FF6")
      ),
      locations = cells_body(
        columns = csw,
        rows = csw < 0.25
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color ="#E39FF6")
      ),
      locations = cells_body(
        columns = whiffs,
        rows = whiffs < 0.20
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color ="#99EDC3")
      ),
      locations = cells_body(
        columns = whiffs,
        rows = whiffs >= 0.25
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color ="#99EDC3")
      ),
      locations = cells_body(
        columns = ev_allowed,
        rows = ev_allowed < 80
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color ="#E39FF6")
      ),
      locations = cells_body(
        columns = ev_allowed,
        rows = ev_allowed > 90
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color ="#E39FF6")
      ),
      locations = cells_body(
        columns = xwOBA,
        rows = xwOBA > 0.340
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color ="#99EDC3")
      ),
      locations = cells_body(
        columns = xwOBA,
        rows = xwOBA < 0.300
      )
    ) %>% 
    gt_theme_538() %>% 
    sub_missing(
      columns = 2:8,
      missing_text = "---"
    ) %>% 
    as_ggplot()
  
  library(patchwork)
  
  final <- (loc_chart + mov_chart) / (table1 / table2) +
    plot_annotation(
      title = paste(player_id_df$name_first," ",player_id_df$name_last,"'s Season Summary", sep = ''),
      caption = "Data: @baseballR | @ajaypatel8_",
      subtitle = paste(season),
      theme = theme(plot.title = element_text(hjust = 0.5, size = 18),
                    plot.subtitle = element_text(hjust = 0.5),
                    legend.position = "top",
                    plot.caption = element_text(hjust = 0.965)
      )) + 
    plot_layout(guides = "collect") 
  
  #save
  ggsave("/Users/ajaypatel/Downloads/RFiles/baseball/SPSummaries/season_summary.png", final, dpi = 'retina', width = 8, height = 8)
  
  
}

pitcher_summary_year("Caleb", "Ferguson", 2023)

#for manual checking
player_first_name <- "Caleb"
player_last_name <- "Ferguson"
season = 2023
