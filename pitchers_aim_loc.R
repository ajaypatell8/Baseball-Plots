library(baseballr)
library(tidyverse)
library(data.table)

#read in data
savantData <- fread("/Users/ajaypatel/Downloads/SavantData.csv")

#filter to one pitcher only, we'll work with him
cole <- savantData %>% 
  #filtering game year for some
  filter(pitcher_name == "Gerrit Cole") 

#create a count variable
cole <- cole %>% 
  #remove missing data
  filter(!is.na(pitch_type), !is.na(plate_x)) %>% 
  mutate(count = as.factor(paste(balls, "-", strikes, sep = "")))

#by pitch type and count, get the mean and sd locations
cole_locations <- cole %>% 
  group_by(pitcher_name, pitch_type, count) %>% 
  summarise(mean_plate_x = mean(plate_x),
            sd_plate_x = sd(plate_x),
            mean_plate_y = mean(plate_z),
            sd_plate_y = sd(plate_z),
            mean_rv = mean(delta_run_exp),
            sd_rv = sd(delta_run_exp)
  )

#function to generate our samples
generate_samples <- function(mean_val, sd_val, n_samples) {
  rnorm(n_samples, mean = mean_val, sd = sd_val)
}

#set the seed for reproducibility
set.seed(123)

#number of samples to generate
n_samples <- 1000

#empty df to store simulated data in
simulated_data <- data.frame()

# Loop through each row in the original data frame
for (i in 1:nrow(cole_locations)) {
  # Extract relevant information from the original data frame
  mean_x <- cole_locations$mean_plate_x[i]
  sd_x <- cole_locations$sd_plate_x[i]
  mean_y <- cole_locations$mean_plate_y[i]
  sd_y <- cole_locations$sd_plate_y[i]
  mean_rv <- cole_locations$mean_rv[i]
  sd_rv <- cole_locations$sd_rv[i]
  
  # Generate random samples for plate x and y and rv
  simulated_x <- generate_samples(mean_x, sd_x, n_samples)
  simulated_y <- generate_samples(mean_y, sd_y, n_samples)
  simulated_rv <- generate_samples(mean_rv, sd_rv, n_samples)
  
  # Create a temporary data frame for the simulated pitch locations
  temp_data <- data.frame(
    pitcher_name = rep(cole_locations$pitcher_name[i], n_samples),
    pitch_type = rep(cole_locations$pitch_type[i], n_samples),
    count = rep(cole_locations$count[i], n_samples),
    plate_x = simulated_x,
    plate_y = simulated_y,
    rv = simulated_rv
  )
  
  # Append the temporary data frame to the main simulated data frame
  simulated_data <- rbind(simulated_data, temp_data)
}

#do fastballs only
simulated_data_small <- simulated_data %>% 
  filter(pitch_type == 'FF')

#define the strike zone
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
#store in dataframe
sz <- data.frame(x,z)

#flip rv so higher is better for pitchers
simulated_data_small <- simulated_data_small %>% 
  mutate(rv = -1 * rv)

player_name <- unique(simulated_data_small$pitcher_name)
pitch <- unique(simulated_data_small$pitch_type)

ggplot(simulated_data_small) +
  geom_raster(aes(x = round(plate_x), y = round(plate_y), fill = rv)) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), na.value = "grey50") +
  geom_path(data = sz, aes(x=x, y=z)) +
  coord_equal()+
  labs(title = paste(player_name, "Simulated Pitch Locations"), 
       subtitle = paste(pitch, "Only | Sampled Using Normal Distributions | Higher Run Value Is Better For The Pitcher"),
       x = "Plate X (Catcher's Perspective)", 
       y = "Plate Y",
       fill = "Run Value",
       caption = "Data: @baseballr | @ajaypatel8_") +
  theme_minimal() +
  facet_wrap(~count) +
  theme(
    legend.position = "right"
  ) +
  xlim(-2, 2) +
  ylim(0, 5)

