source("importationAndCleanup.R")
library(ggplot2)
library(dplyr)


# total_values <- bikeSharing %>% group_by(yr) %>% 
#   summarise(total_casual = sum(casual), 
#   total_registered = sum(registered),
#   total_cnt = sum(cnt))

total_values <- bikeSharing %>% group_by(yr) %>% 
  summarise(total_cnt = sum(cnt))

percentages <- total_values[2, 2:4] / total_values[1, 2:4]
