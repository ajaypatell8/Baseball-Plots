library(baseballr)
library(readr)
library(tidyverse)
library(caret)
library(robustbase)
library(gt)
library(gtExtras)
library(mlbplotR)

# Read in existing savant data loaded through python // currently contains 2024 season up to 8/26
savantData <- read_csv("/Users/ajaypatel/Downloads/SavantData.csv") 

# Select relevant columns 
savantData <- savantData %>% 
  select(pitcher_name, pitcher, p_throws, pitching_team, pitch_type, 
         release_pos_x, release_pos_z, release_extension, pfx_x, pfx_z) %>% 
  # Ensure no missing data
  filter(!is.na(release_pos_z), !is.na(release_pos_x), !is.na(release_extension)) %>% 
  # Standardize horizontal movement and release
  mutate(pfx_x = if_else(p_throws == 'L', -pfx_x, pfx_x),
         release_pos_x = if_else(p_throws == 'L', -release_pos_x, release_pos_x)) %>% 
  # Convert movement to inches
  mutate(pfx_x = 12 * pfx_x, pfx_z = 12 * pfx_z)

# Scale the release position variables to cluster
release_points <- scale(savantData[, c("release_pos_x", "release_pos_z", "release_extension")])

# Perform k-means clustering
# For reproducibility
set.seed(42)  
# Feel free to adjust number of clusters
k <- 10
clusters <- kmeans(release_points, centers = k)

# Add clusters to our csv
savantData$cluster <- clusters$cluster

# Group by cluster and pitch type, then calculate average movement
cluster_mov <- savantData %>%
  group_by(cluster, pitch_type) %>%
  summarize(
    avg_pfx_x = mean(pfx_x, na.rm = TRUE),
    avg_pfx_z = mean(pfx_z, na.rm = TRUE),
    .groups = "drop"
  )

# Merge the cluster movement back to savantData for comparison
savantData <- savantData %>%
  left_join(cluster_mov, by = c("pitch_type", "cluster"))

# Calculate differences from average movement based on pitch type and cluster
savantData <- savantData %>%
  mutate(
    residual_x = pfx_x - avg_pfx_x,
    residual_z = pfx_z - avg_pfx_z
  )

# Calculate Mahalanobis distance for the residuals based on covariance
cov_matrix <- covMcd(savantData[, c("residual_x", "residual_z")])
mahalanobis_dist <- mahalanobis(savantData[, c("residual_x", "residual_z")],
                                center = cov_matrix$center, cov = cov_matrix$cov)

# Get an idea of out distribution
summary(mahalanobis_dist)

# Set a threshold for identifying outliers
threshold <- quantile(mahalanobis_dist, 0.95)
savantData$Outlier <- mahalanobis_dist > threshold

# Plot the residuals
ggplot(savantData, aes(x = residual_x, y = residual_z, color = outlier)) +
  geom_point() +
  labs(title = "Outlier Detection in Pitch Movement",
       x = "Residual X Movement (Actual Movement - Average Movement In A Player's Respective Cluster)",
       y = "Residual Z Movement",
       caption = "Data: @baseballR | @ajaypatel8_") +
  ggthemes::theme_clean() +
  theme(legend.position = "top")

# Create a table of the top ten pitches with the highest outlier rate
table <- savantData %>% 
  group_by(pitcher_name, pitcher, pitch_type) %>% 
  summarise(pitches = n(),
            outlier_rate = sum(Outlier) / pitches) %>% 
  # Min. 200 pitches
  filter(pitches >= 200) %>% 
  arrange(desc(outlier_rate)) %>% 
  head(10) %>% 
  ungroup() %>% 
  gt() %>% 
  cols_label(pitcher_name = "Pitcher",
             pitcher = "",
             pitch_type = "Pitch Type",
             pitches = "Pitches Thrown",
             outlier_rate = "Outlier Rate") %>% 
  mlbplotR::gt_fmt_mlb_dot_headshot(columns = pitcher) %>% 
  tab_header(title = "Pitchers With The Highest Outlier Rate",
             subtitle = "Min. 200 Thrown In 2024 | @ajaypatel8_") %>% 
  gt_theme_538() %>% 
  fmt_percent(columns = outlier_rate)

# Save our table
gtsave(table, "pitcher_outliers.png")  
