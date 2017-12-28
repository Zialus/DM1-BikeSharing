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
bikeSharing$yr <-
  factor(bikeSharing$yr,
         levels = c(2011, 2012),
         ordered = TRUE)


# Verify if the values of "season" are within bounds
unique(bikeSharing$season)

# Change the column "season" from numbers to names
change_season <- function(x) {
  sapply(x, function(x) {
    switch(
      as.character(x),
      "1" = "Winter",
      "2" = "Spring",
      "3" = "Summer",
      "4" = "Fall"
    )
  })
}

bikeSharing$season <- change_season(bikeSharing$season)
bikeSharing$season <-
  factor(
    bikeSharing$season,
    levels = c("Spring", "Summer", "Fall", "Winter"),
    ordered = TRUE
  )

# Change the values of "weathersit"
change_workd <- function(x) {
  sapply(x, function(x) {
    switch(
      as.character(x),
      "1" = "Clear",
      "2" = "Mist",
      "3" = "Light rain/snow",
      "4" = "Heavy rain/snow"
    )
  })
}

bikeSharing$weathersit <- change_workd(bikeSharing$weathersit)
bikeSharing$weathersit <-
  factor(
    bikeSharing$weathersit,
    levels = c("Clear", "Mist", "Light rain/snow", "Heavy rain/snow"),
    ordered = TRUE
  )

# Change the column "month" from 0-12 to January/.../December
bikeSharing$mnth <- month(bikeSharing$mnth, label = TRUE, abbr = FALSE)

# Change "weekday" column from values 0-6 to Monday/.../Sunday
bikeSharing$weekday <- wday(bikeSharing$dteday, label = TRUE, abbr = FALSE)

# Change the column "holiday" to Boolean
bikeSharing$holiday <- as.logical(bikeSharing$holiday)

# Change the column "holiday" to Boolean
bikeSharing$workingday <- as.logical(bikeSharing$workingday)

# Create 4 new columns with "real" values of temp/atemp/hum/windspeed columns
bikeSharing <- mutate(bikeSharing, real_temp = bikeSharing$temp * 41)

bikeSharing <- mutate(bikeSharing, real_atemp = bikeSharing$atemp * 41)

bikeSharing <- mutate(bikeSharing, real_hum = bikeSharing$hum * 100)

bikeSharing <- mutate(bikeSharing, real_windspeed = bikeSharing$windspeed * 67)
