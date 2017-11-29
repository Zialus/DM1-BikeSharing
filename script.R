library(DMwR)
library(dplyr)
library(lubridate)

DataToInvestigate <- read.csv("Dataset/day.csv", header = TRUE)

# Verify if all entries on the dataset are from 2011 and 2012
all_the_dates <- DataToInvestigate$dteday
all_the_dates[year(all_the_dates) != 2011 && year(all_the_dates) != 2012]

# Verify if the values of "season" are within bounds
unique(DataToInvestigate$season)

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

DataToInvestigate$season <- change_season(DataToInvestigate$season)
DataToInvestigate$season <- factor(DataToInvestigate$season, levels = c("Spring", "Summer", "Fall", "Winter"))

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

DataToInvestigate$mnth <- change_month(DataToInvestigate$mnth)
DataToInvestigate$mnth <-
  factor(
    DataToInvestigate$mnth,
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
    )
  )
