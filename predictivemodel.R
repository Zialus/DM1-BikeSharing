source("importationAndCleanup.R")

library(performanceEstimation)
library(DMwR)
library(randomForest)
library(e1071)
library(caret)
library(gbm)


res <- performanceEstimation(
  PredTask(cnt ~ ., bikeSharing),
  workflowVariants("standardWF", learner = c("rpartXse", "svm")),
  EstimationTask(
    metrics = c("rmse", "mae"),
    method = CV(nReps = 1, nFolds = 10)
  )
)

summary(res)

plot(res)
