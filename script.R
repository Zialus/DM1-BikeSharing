library(DMwR)
library(dplyr)
library(lubridate)

bikeSharing <- read.csv("Dataset/day.csv", header = TRUE)


# Verify if all entries on the dataset are from 2011 and 2012
all_the_dates <- bikeSharing$dteday
all_the_dates[year(all_the_dates) != 2011 && year(all_the_dates) != 2012]

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
change_month <- function(x) {
  sapply(x, function(x) {
    switch(
      as.character(x),
      "1" = "January",
      "2" = "February",
      "3" = "March",
      "4" = "April",
      "5" = "May",
      "6" = "June",
      "7" = "July",
      "8" = "August",
      "9" = "September",
      "10" = "October",
      "11" = "November",
      "12" = "December"
    )
  })
}

bikeSharing$mnth <- change_month(bikeSharing$mnth)
bikeSharing$mnth <-
  factor(
    bikeSharing$mnth,
    levels = c(
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    ), ordered = TRUE
  )


# Create 4 new columns with "real" values of temp/atemp/hum/windspeed columns
bikeSharing <- mutate(bikeSharing, temperature = bikeSharing$temp * 41)

bikeSharing <- mutate(bikeSharing, feeling_temp = bikeSharing$atemp * 41)

bikeSharing <- mutate(bikeSharing, hummidity = bikeSharing$hum * 100)

bikeSharing <- mutate(bikeSharing, real_windspeed = bikeSharing$windspeed * 67)


# TODO: fix this thing!!!
# Change "weekday" column from values 0-6 to Monday/Tuesday/.../Saturday/Sunday
bikeSharing$weekday <- as.character(as.numeric(bikeSharing$weekday))
bikeSharing$weekday <-
  factor(
    wday(bikeSharing$dteday, label = TRUE),
    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    labels = c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )
  )
