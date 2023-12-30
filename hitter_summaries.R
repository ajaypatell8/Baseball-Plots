batter_summary_name <- function(player_first_name, player_last_name, start_date, end_date) {
  
  library(readr)
  library(tidyverse)
  library(ggplot2)
  library(gtExtras)
  library(gt)
  library(patchwork)
  library(baseballr)
  library(mlbplotR)
  library(cfbplotR)
  library(ggtext)
  library(GeomMLBStadiums)
  library(bstfun)
  
  mlbplayerids <- read_csv("/Users/ajaypatel/Downloads/mlbplayerids.csv")
  
  player_id_df <- mlbplayerids %>% 
    filter(name_first == player_first_name, name_last == player_last_name, !is.na(key_mlbam),
           !is.na(key_fangraphs))
  
  player_key_mlbam <- player_id_df$key_mlbam
  
  data <- baseballr::scrape_statcast_savant(start_date = start_date, end_date = end_date, 
                                            playerid = player_key_mlbam, player_type = "batter")
  
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
  
  #add player team
  data <- data %>% 
    mutate(opp_team = if_else(inning_topbot == "Top", home_team, away_team),
           team = if_else(inning_topbot == "Top", away_team, home_team))
  
  #for plotting
  data <- data %>% 
    rename("Pitch"  = pitch_name)

  #get player headshots
  headshot <- mlbplotR::load_headshots() %>% filter(savant_id == player_id_df$key_mlbam) %>% 
    pull(espn_headshot)
  
  
  if(length(headshot) == 0 || is.na(headshot)) {
    headshot = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Major_League_Baseball_logo.svg/2560px-Major_League_Baseball_logo.svg.png"
  }  
  
  #add pitch groups
  data <- data %>% 
    mutate(Pitch1 = 
             case_when(
               pitch_type == "FF" ~ "Fastball",
               pitch_type == "FA" ~ "Fastball",
               pitch_type == "SI" ~ "Fastball",
               pitch_type == "ST" ~ "Breaking Ball",
               pitch_type == "SL" ~ "Breaking Ball",
               pitch_type == "KC" ~ "Breaking Ball",
               pitch_type == "CU" ~ "Breaking Ball",
               pitch_type == "EP" ~ "Breaking Ball",
               pitch_type == "FC" ~ "Breaking Ball",
               pitch_type == "SC" ~ "Breaking Ball",
               pitch_type == "CH" ~ "Offspeed",
               pitch_type == "FS" ~ "Offspeed",
               TRUE ~ "Other"
             ))
  
  swings_vector <- c("hit_into_play", "swinging_strike", "swinging_strike_blocked", "foul_tip",
                     "foul")
  
  data <- data %>% 
    mutate(swung = if_else(description %in% swings_vector,1,0))
  
  location <- ggplot() +
    geom_point(data = data %>% filter(swung == 1), aes(x = plate_x, y = plate_z, color = Pitch1), 
               size = 2.5, alpha = 0.4) +
    stat_ellipse(data = data %>% filter(swung==1), aes(x = plate_x, y = plate_z, color = Pitch1,
                                    fill = Pitch1), geom = "polygon",
                 level = 0.4, alpha = 0.3) +
    geom_path(data = sz, aes(x = x, y = z)) +
    geom_path(data = sz1, aes(x = x1, y = z1), linetype = "dashed", alpha = 0.6) +
    geom_path(data = sz2, aes(x = x2, y = z2), linetype = "dashed", alpha = 0.6) +
    geom_path(data = sz3, aes(x = x3, y = z3), linetype = "dashed", alpha = 0.6) +
    geom_path(data = sz4, aes(x = x4, y = z4), linetype = "dashed", alpha = 0.6) +
    theme_minimal() +
    labs(
      title = "Swing Chart",
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
                  title = "Swing Chart",
                  image_height = 55) +
    theme_title_image(size = 14, hjust = 0.5, vjust = 0.25) 

    
  
  #bbe chart
  bbe <- data %>% 
    filter(type == "X") %>% 
    #edit events type
    mutate(Result = case_when(
      events == "double_play" ~ "Field Out",
      events == "field_out" ~ "Field Out",
      events == "fielders_choice" ~ "Field Out",
      events == "force_out" ~ "Field Out",
      events == "grounded_into_double_play" ~ "Field Out",
      events == "home_run" ~ "Home Run",
      events == "double" ~ "Double",
      events == "single" ~ "Single",
      events == "triple" ~ "Triple",
      TRUE ~ "Other"
    ))
  
  #get team logo
  teamlogo <- mlbplotR::load_mlb_teams() %>% filter(team_abbr == unique(data$team)) %>% 
    pull(team_logo_espn)

  
  spray <- bbe %>% 
    ggplot(aes(x=hc_x, y=hc_y)) + 
    geom_spraychart(stadium_segments = "all")+
    geom_point(aes(fill = Result), shape = 21, alpha = 0.8, size = 4) +
    theme_minimal() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5), 
          axis.text = element_blank()) +
    coord_fixed() +
    scale_y_reverse() +
    scale_fill_discrete(breaks=c("Field Out", "Single", "Double", "Triple", "Home Run")) +
    ggtitle_image(title_image = teamlogo,
                  title = "Batted Ball Chart",
                  image_height = 55,
                  image_side = "right") +
    theme_title_image(size = 14, hjust = 0.5, vjust = 0.25) +
    labs(x = "Hit Coordinates (X)",
         y = "Hit Coordinates (Y)") +
    guides(fill=guide_legend(nrow=2, byrow=TRUE)) 
  
  #first summary table
  logs <- baseballr::bref_daily_batter(start_date, end_date)
  
  logs <- logs %>% 
    filter(bbref_id == player_id_df$key_bbref)
  
  #get median launch angle and ev
  median_la <- median(bbe$launch_angle, na.rm = TRUE)
  median_ev <- median(bbe$launch_speed, na.rm = TRUE)
  
  summary1 <- logs %>% 
    group_by(Name) %>% 
    summarise(PA, median_la = median_la, median_ev = median_ev,
              SO = SO / PA, BB = BB / PA, XBH = `X2B` + `X3B` + HR, OPS, SB) %>% 
    rename(PlayerName = Name, K = SO) %>% 
    gt()
  
  summary1 <- summary1$`_data`
  
  table1 <- summary1 %>% 
    gt() %>% 
    fmt_percent(columns = c(K, BB)) %>% 
    fmt_number(columns = OPS, decimals = 3) %>% 
    fmt_number(columns = c(median_la, median_ev), decimals = 1) %>% 
    gt_theme_538() %>% 
    cols_label(
      PlayerName = "Batter",
      K = "K%",
      BB = "BB%",
      median_la = "Median LA",
      median_ev = "Median EV"
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = K,
        rows = K < 0.16
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = BB,
        rows = BB > 0.10
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = median_ev,
        rows = median_ev > 92
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = OPS,
        rows = OPS > 0.775
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = median_ev,
        rows = median_ev < 86
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = K,
        rows = K > 0.22
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = OPS,
        rows = OPS < 0.715
      )
    ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#E39FF6")
    ),
    locations = cells_body(
      columns = BB,
      rows = BB < 0.06
    )
  ) %>% 
    bstfun::as_ggplot()
  
  #table 2
  csw_vector <- c("called_strike", "swinging_strike", "swinging_strike_blocked", "foul_tip")
  swings_vector <- c("hit_into_play", "swinging_strike", "swinging_strike_blocked", "foul_tip",
                     "foul")
  whiffs_vector <- c("swinging_strike", "swinging_strike_blocked", "foul_tip")
  contact_vector <- c("hit_into_play", "foul", "foul_tip")
  
  dis <- data %>% 
    group_by(batter) %>% 
    mutate(csw = (if_else(description %in% csw_vector, 1, 0)),
           swings = if_else(description %in% swings_vector, 1, 0),
           whiff = if_else(description %in% whiffs_vector, 1, 0),
           contact = if_else(description %in% contact_vector, 1, 0),
           out_of_zone = if_else(zone > 9, 1, 0)) %>% 
    summarise(Pitches = n(), BBE = sum(type == "X"),
      gb_rate = sum(bb_type == "ground_ball") / sum(type == "X"),
              ld_rate = sum(bb_type == "line_drive") / sum(type == "X"),
              fb_rate = sum(bb_type == "fly_ball") / sum(type == "X"),
              zone_rate = sum(out_of_zone == 0) / n(),
              contact_rate = sum(contact) / sum(swings),
              zone_contact_rate = sum(contact[out_of_zone==0]) / sum(swings[out_of_zone==0]),
              out_zone_contact_rate = sum(contact[out_of_zone==1]) / sum(swings[out_of_zone==1]),
              whiffs = sum(whiff) / sum(swings),
              pitches_in_zone = sum(out_of_zone==0),
              pitches_out_zone = sum(out_of_zone==1),
              zoneswing_rate = sum(swings[out_of_zone==0]) / pitches_in_zone,
              chase_rate = sum(swings[out_of_zone==1]) / pitches_out_zone) %>% 
    select(Pitches,BBE, gb_rate, ld_rate, fb_rate, zone_rate,zoneswing_rate, chase_rate,
           contact_rate, whiffs) %>% 
    gt()
  
  summary2 <- dis$`_data`
  
  table2 <- summary2 %>% 
    gt() %>% 
    fmt_percent(columns = everything(), decimals = 0) %>% 
    fmt_number(columns = c(Pitches, BBE), decimals = 0) %>% 
    gt_theme_538() %>% 
    cols_label(
      gb_rate = "GB%",
      ld_rate = "LD%",
      fb_rate = "FB%",
      zone_rate = "Zone%",
      zoneswing_rate = "zSwing%",
      chase_rate = "oSwing%",
      contact_rate = "Contact%",
      whiffs = "Whiff%"
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = zoneswing_rate,
        rows = zoneswing_rate > 0.75
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = chase_rate,
        rows = chase_rate < 0.25
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = contact_rate,
        rows = contact_rate > 0.85
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = whiffs,
        rows = whiffs < 0.19
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = whiffs,
        rows = whiffs > 0.27
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = contact_rate,
        rows = contact_rate < 0.75
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = chase_rate,
        rows = chase_rate > 0.35
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = zoneswing_rate,
        rows = zoneswing_rate < 0.55
      )
    ) %>% 
    sub_missing(
      columns = 3:10,
      missing_text = "---"
    ) %>% 
    bstfun::as_ggplot()
  
  disp <- data %>% 
    group_by(Pitch) %>% 
    mutate(csw = (if_else(description %in% csw_vector, 1, 0)),
           swings = if_else(description %in% swings_vector, 1, 0),
           whiff = if_else(description %in% whiffs_vector, 1, 0),
           contact = if_else(description %in% contact_vector, 1, 0),
           out_of_zone = if_else(zone > 9, 1, 0)) %>% 
    summarise(used = n()/ nrow(data),
      gb_rate = sum(bb_type == "ground_ball") / sum(type == "X"),
              ld_rate = sum(bb_type == "line_drive") / sum(type == "X"),
              fb_rate = sum(bb_type == "fly_ball") / sum(type == "X"),
              zone_rate = sum(out_of_zone == 0) / n(),
              contact_rate = sum(contact) / sum(swings),
              zone_contact_rate = sum(contact[out_of_zone==0]) / sum(swings[out_of_zone==0]),
              out_zone_contact_rate = sum(contact[out_of_zone==1]) / sum(swings[out_of_zone==1]),
              whiffs = sum(whiff) / sum(swings),
              pitches_in_zone = sum(out_of_zone==0),
              pitches_out_zone = sum(out_of_zone==1),
              zoneswing_rate = sum(swings[out_of_zone==0]) / pitches_in_zone,
              chase_rate = sum(swings[out_of_zone==1]) / pitches_out_zone) %>% 
    select(Pitch, used,gb_rate, ld_rate, fb_rate, zone_rate,zoneswing_rate, chase_rate,
           contact_rate, whiffs) %>% 
    arrange(desc(used)) %>% 
    slice(1:4) %>% 
    gt()
  
  summary3 <- disp$`_data`
  
  table3 <- summary3 %>% 
    gt() %>% 
    fmt_percent(columns = everything(), decimals = 0) %>% 
    gt_theme_538() %>% 
    cols_label(
      gb_rate = "GB%",
      ld_rate = "LD%",
      fb_rate = "FB%",
      zone_rate = "Zone%",
      zoneswing_rate = "zSwing%",
      chase_rate = "oSwing%",
      contact_rate = "Contact%",
      whiffs = "Whiff%"
    ) %>% 
    tab_header(
      title = "By Pitch Type"
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = zoneswing_rate,
        rows = zoneswing_rate > 0.75
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = chase_rate,
        rows = chase_rate < 0.25
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = contact_rate,
        rows = contact_rate > 0.85
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#99EDC3")
      ),
      locations = cells_body(
        columns = whiffs,
        rows = whiffs < 0.19
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = whiffs,
        rows = whiffs > 0.27
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = contact_rate,
        rows = contact_rate < 0.75
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = chase_rate,
        rows = chase_rate > 0.35
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#E39FF6")
      ),
      locations = cells_body(
        columns = zoneswing_rate,
        rows = zoneswing_rate < 0.55
      )
    ) %>% 
    sub_missing(
      columns = 3:10,
      missing_text = "---"
    ) %>% 
    bstfun::as_ggplot()
  
  #final plot
  
  final <- (location + spray) / (table1 / table2 / table3) +
    plot_annotation(
      title = paste(player_id_df$name_first," ",player_id_df$name_last,"'s Batter Summary", 
                    sep = ''),
      subtitle = paste(start_date, " To ", end_date, sep = ''),
      theme = theme(plot.title = element_text(hjust = 0.5, size = 18),
                    plot.subtitle = element_text(hjust = 0.5)),
      caption = "Data: @baseballR | @ajaypatel8_")


  playername <- paste(player_first_name, player_last_name)
  
  ggsave(paste("/Users/ajaypatel/Downloads/RFiles/baseball/BatterSummaries/", end_date,"/", playername, ".png" ,sep = ''), 
         final, dpi = 'retina', width = 8, 
         height = 9)
  
}

batter_summary_name("Vaughn", "Grissom", "2023-04-14", "2023-08-23")


