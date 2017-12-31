source("importationAndCleanup.R")

library(DMwR)

set.seed(1234)
trPerc <- 0.7
sp <- sample(1:nrow(bikeSharing), as.integer(trPerc*nrow(bikeSharing)))
tr <- bikeSharing[sp,]
ts <- bikeSharing[-sp,]

