library(DMwR)
library(dplyr)
library(lubridate)

bikeSharing <- read.csv("Dataset/day.csv", header = TRUE)


# Verify if all entries on the dataset are from 2011 and 2012
all_the_dates <- bikeSharing$dteday
all_the_dates[year(all_the_dates) != 2011 && year(all_the_dates) != 2012]

# Verify if all entries on the dataset have correct sums for casual and registered
subset(bikeSharing, casual + registered != cnt)

# Change the values of "yr" from 0/1 to 2011/2012
change_year <- function(x) {
  sapply(x, function(x) {
    switch(as.character(x),
           "0" = "2011",
           "1" = "2012")
  })
}

bikeSharing$yr <- change_year(bikeSharing$yr)
bikeSharing$yr <- factor(bikeSharing$yr, levels = c(2011, 2012), ordered = TRUE)


# Verify if the values of "season" are within bounds
unique(bikeSharing$season)

# Change the column "season" from numbers to names
change_season <- function(x) {
  sapply(x, function(x) {
    switch(
      as.character(x),
      "1" = "Spring",
      "2" = "Summer",
      "3" = "Fall",
      "4" = "Winter"
    )
  })
}

bikeSharing$season <- change_season(bikeSharing$season)
bikeSharing$season <- factor(bikeSharing$season, levels = c("Spring", "Summer", "Fall", "Winter"), ordered = TRUE)

# Change the column "month" from numbers to names
bikeSharing$mnth <- month(bikeSharing$mnth, label = TRUE, abbr = FALSE)

# Change the column "holiday" to Boolean
bikeSharing$holiday <- as.logical(bikeSharing$holiday)

# Create 4 new columns with "real" values of temp/atemp/hum/windspeed columns
bikeSharing <- mutate(bikeSharing, temperature = bikeSharing$temp * 41)

bikeSharing <- mutate(bikeSharing, feeling_temp = bikeSharing$atemp * 41)

bikeSharing <- mutate(bikeSharing, hummidity = bikeSharing$hum * 100)

bikeSharing <- mutate(bikeSharing, real_windspeed = bikeSharing$windspeed * 67)


# Change "weekday" column from values 0-6 to Monday/Tuesday/.../Saturday/Sunday
bikeSharing$weekday <- wday(bikeSharing$dteday, label = TRUE, abbr = FALSE)
