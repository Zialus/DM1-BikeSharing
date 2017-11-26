library(DMwR)
library(lubridate)

DataToInvestigate <- read.csv("Dataset/day.csv")

#verificar se existem linha com valores "na" -> não existe nenhuma
nrow(DataToInvestigate[!complete.cases(DataToInvestigate),])
(rows_with_na <- DataToInvestigate[!complete.cases(DataToInvestigate),])

