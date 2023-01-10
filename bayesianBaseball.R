library(baseballr)
library(bayesboot)
library(tidyverse)

#scrape fg leaderboards
hitters <- baseballr::fg_bat_leaders(2019, 2022, qual = 0)

#get plate appearances for filtering later
pas <- hitters %>% 
  group_by(Name) %>% 
  summarise(pas = sum(PA))

#bayesian // shoutout adrian
lst = list()
players = unique(hitters$Name)

for (player in players){
  hitters_player = hitters %>% filter(Name == player)
  #bayesian bootstrapping happening here // can edit the stat 
  b <- bayesboot(as.vector(hitters_player$wOBA), mean)
  s = summary(b)
  mean_wOBA = s$value[1]
  lci = s$value[3]
  uci = s$value[4]
  df = data.frame('mean'=mean_wOBA,'LCI'=lci,'UCI'=uci)
  lst[[player]] = df
}

#binding results
df = dplyr::bind_rows(lst)
df$Name = players
df = df %>% arrange(mean)

#join pa average
df <- left_join(df, pas, by = c('Name'))

#smaller df
df_500 <- df %>% 
  filter(pas >= 500) %>% 
  arrange(desc(mean))

#graph of intervals for players
df_500 %>%
  slice(1:5) %>% 
  ggplot(aes(x= reorder(factor(Name, level = Name), mean), y=mean)) + 
  geom_pointrange(aes(ymin=(LCI),
                      ymax=(UCI)))+
  coord_flip() +
  theme_minimal() +
  labs(
    x = 'wOBA',
    y = '',
    title = 'Bayesian Bootstrapping To Estimate wOBA',
    caption = 'Data: @baseballr | @ajaypatell8'
  )

#shortstop graph
shortstops <- c("Dansby Swanson", 'Trea Turner', 'Xander Bogaerts',
                'Carlos Correa')

df %>% 
  filter(Name %in% shortstops) %>% 
  ggplot(aes(x= reorder(factor(Name, level = Name), mean), y=mean)) + 
  geom_pointrange(aes(ymin=(LCI),
                      ymax=(UCI)))+
  coord_flip() +
  theme_minimal() +
  labs(
    x = 'wOBA',
    y = '',
    title = 'Bayesian Bootstrapping To Estimate wOBA',
    caption = 'Data: @baseballr | @ajaypatell8'
  )
