library(DMwR)
library(dplyr)
library(lubridate)

DataToInvestigate <- read.csv("Dataset/day.csv", header = TRUE)

# verificar se existem linha com valores "na" -> não existe nenhuma
nrow(DataToInvestigate[!complete.cases(DataToInvestigate),])
(rows_with_na <- DataToInvestigate[!complete.cases(DataToInvestigate),])

# verificar se existe alguma data diferente de 2011 ou 2012
all_the_dates <- DataToInvestigate$dteday
all_the_dates[year(all_the_dates) != 2011 && year(all_the_dates) != 2012]

# verificar se os valores de "season" são 1,2,3 ou 4
unique(DataToInvestigate$season)

# mudar valores da coluna "season" de 1,2,3 e 4 para spring, summer, fall, winter
change_season <- function(x){
  sapply(x, function(x){
    switch(as.character(x),
           "1" = "spring",
           "2" = "summer",
           "3" = "fall",
           "4" = "winter")
  })
}

DataToInvestigate$season <- change_season(DataToInvestigate$season)
DataToInvestigate$season <- factor(DataToInvestigate$season, levels = c("spring", "summer", "fall", "winter"))
