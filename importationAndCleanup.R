library(DMwR)
library(dplyr)
library(lubridate)

# Import dataset from csv
bikeSharing <- read.csv("Dataset/day.csv", header = TRUE)

# Verify if all entries on the dataset are only from 2011 or 2012
subset(bikeSharing, year(dteday) != 2011 & year(dteday) != 2012)

# Verify if all entries on the dataset have correct sums for casual and registered
subset(bikeSharing, casual + registered != cnt)

# Verify if the values of "season" are within bounds
unique(bikeSharing$season)

# Verify if the values of "mnth" are within bounds
unique(bikeSharing$mnth)

# Verify if the values of "weekday" are within bounds
unique(bikeSharing$weekday)

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
    levels = c("Winter", "Spring", "Summer", "Fall"),
    ordered = TRUE
  )


# Change the values of "weathersit"
change_weathersit <- function(x) {
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
bikeSharing$weathersit <- change_weathersit(bikeSharing$weathersit)
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

# Change the column "workingday" to Boolean
bikeSharing$workingday <- as.logical(bikeSharing$workingday)

# Create 4 new columns with "real" values of temp/atemp/hum/windspeed columns
bikeSharing <- mutate(bikeSharing, real_temp = bikeSharing$temp * 41)

bikeSharing <- mutate(bikeSharing, real_atemp = bikeSharing$atemp * 41)

bikeSharing <- mutate(bikeSharing, real_hum = bikeSharing$hum * 100)

bikeSharing <- mutate(bikeSharing, real_windspeed = bikeSharing$windspeed * 67)


# Add day of the year column
bikeSharing$day <- yday(bikeSharing$dteday)

# Add day within season
add_day_w_season <- function(days, seasons) {
  mapply(function(x, y) {
    current_day <- x
    season <- y
    currentyear <- year(current_day)
    switch(
      as.character(season),
      "Winter" = season_first_day <-
        as.Date(paste(currentyear, "12-21", sep = "-")),
      "Spring" = season_first_day <-
        as.Date(paste(currentyear, "03-21", sep = "-")),
      "Summer" = season_first_day <-
        as.Date(paste(currentyear, "06-21", sep = "-")),
      "Fall" = season_first_day <-
        as.Date(paste(currentyear, "09-23", sep = "-"))
    )
    if (yday(current_day) < yday("2012-3-21")) {
      tmp <- year(season_first_day) - 1
      season_first_day <- paste(tmp, "12-21", sep = "-")
      ceiling(difftime(current_day, season_first_day, units = "days"))
    }
    else{
      ceiling(difftime(current_day, season_first_day, units = "days"))
    }
  }, days, seasons)
}

bikeSharing$seasonday <- add_day_w_season(bikeSharing$dteday, bikeSharing$season)
