#
# randomforest.r
#
# Latest version of caret:
# devtools::install_github('topepo/caret/pkg/caret')
#
library(mlbench)
library(parallel)
library(doParallel)
library(foreach)
library(haven)
library(MASS)
library(ggplot2)
library(caret)
library(randomForest)
library(pROC)
library(party)
library(dplyr)
library(ggraph)
library(igraph)
library(rpart.plot)
library(iml)
#
#
cl <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cl)
#
#setwd("c:/")
dat <- read_dta("Dropbox/ICPSRML2019_admin/assignments/CCES2016_abbreviated.dta")
#
dat$voted2016 <- factor(dat$voted2016, labels=c("Didntvote","Voted"))
dat$ideology <- factor(dat$ideology)
dat$partyid <- factor(dat$partyid)
dat$education <- factor(dat$education)
#
dat <- na.omit(dat)
#
# Pre-process and impute missing data
#
impute <- preProcess(dat, method=c("center","scale"))
dat.imputed <- predict(impute, dat)
#
# Split data
#
set.seed(1985)
trainIndex <- createDataPartition(dat.imputed$voted2016, p=0.2, list = FALSE, times = 1)
#
train <- dat.imputed[trainIndex,]
test <- dat.imputed[-trainIndex,]
#
# Set control parameters for model training
#
fitCtrl <- trainControl(method = "cv",
                        number = 10,
                        ## repeats = 5,
                        ## Estimate class probabilities
                        classProbs = TRUE,
                        ## returnData = TRUE,
                        savePredictions = TRUE,
                        ## Search "grid" or "random"
                        search = "random",
                        ## Up-sampling
                        sampling = "up",
                        ## Use cluster
                        allowParallel = TRUE)
#
# Set testing grids
#
rfGrid <-  expand.grid(mtry = 2:5)
#
gbmGrid <-  expand.grid(n.trees = c(1:20)*100,
                        interaction.depth=c(2,3),
                        shrinkage = c(0.01, 0.05),
                        n.minobsinnode=c(5,10,15))
#
#
# Single tree
#
singletree <- rpart(voted2016 ~ ., data = train,
  control = rpart.control(minsplit = 20, minbucket = 5))
#
# Plot tree
#
rpart.plot(singletree)
#
train.pred <- predict(singletree, train, type="class")
test.pred <- predict(singletree, test, type="class")
#
confusionMatrix(train$voted2016, train.pred)
confusionMatrix(test$voted2016, test.pred)
#
rf.res <- train(voted2016 ~ .,
    data=train,
    method="rf",
    trControl=fitCtrl,
    tuneGrid=rfGrid,
    metric="Accuracy",
    verbose=FALSE)
#
rf.res
#
confusionMatrix(predict(rf.res, train, type="raw"), train$voted2016)
confusionMatrix(predict(rf.res, test, type="raw"), test$voted2016)
#
gbm.res <- train(voted2016 ~ .,
    data=train,
    method="gbm",
    trControl=fitCtrl,
    tuneGrid=gbmGrid,
    metric="Accuracy",
    verbose=FALSE)
#
gbm.res
#
confusionMatrix(predict(gbm.res, train, type="raw"), train$voted2016)
confusionMatrix(predict(gbm.res, test, type="raw"), test$voted2016)
#
# Ensemble predictions
#
rf.pred <- predict(rf.res, test, type="prob")[,"Voted"]
gbm.pred <- predict(gbm.res, test, type="prob")[,"Voted"]
combined.pred <- (rf.pred + gbm.pred) / 2
roc(test$voted2016 ~ rf.pred)
roc(test$voted2016 ~ gbm.pred)
roc(test$voted2016 ~ combined.pred)
#
# Plot feature effects
#
X <- test[which(names(test) != "voted2016")]
predictor <- Predictor$new(rf.res, data = X, y = test$voted2016, type="prob", class="Voted")
#
ale <- FeatureEffect$new(predictor, feature = "age", method = "pdp+ice")
ale$plot()
#
# Measure interactions (Friedman's H-statistic)
#
interact <- Interaction$new(predictor, feature = "age", parallel=TRUE)
plot(interact)
#
ale <- FeatureEffect$new(predictor, feature = "age", method = "pdp+ice")
ale$plot()
#
#
#
stopCluster(cl)
registerDoSEQ()
#
