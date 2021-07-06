library("caret")
library("knitr")
library("mlbench")
library("parallel")
library("doParallel")
library("foreach")
library("haven")
library("MASS")
library("ggplot2")
library("randomForest")
library("pROC")
library("party")
library("dplyr")
library("ggraph")
library("igraph")
library("rpart.plot")

CCES2016 <- read_dta("C:/Users/ual-laptop/Downloads/CCES2016_abbreviated_1.dta")

dat <- na.omit(CCES2016)
dat$voted2016 <- factor(dat$voted2016, labels=c("No_Vote", "Voted"))
#Split data
set.seed(1985)
trainIndex <- createDataPartition(dat$voted2016, p=0.2, list = FALSE, times = 1)
#
train <- dat[trainIndex,]
test <- dat[-trainIndex,]
#Set control parameters for model training
#
fitCtrl <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 2,
                        summaryFunction=twoClassSummary,
                        ## Estimate class probabilities
                        classProbs = TRUE,
                        ## returnData = TRUE,
                        savePredictions = TRUE,
                        ## Search "grid" or "random"
                        search = "random",
                        ## Down-sampling
                        sampling = "down",
                        ## Use cluster
                        allowParallel = TRUE)
library("e1071")

# Set testing grid for random forest
#
rfGrid <-  expand.grid(mtry = 1:6)
set.seed(1985)
rf.res <- train(voted2016 ~ .,
                data=train,
                method="rf",
                trControl=fitCtrl,
                tuneGrid=rfGrid,
                #tuneLength=10,
                metric="ROC",
                verbose=FALSE)

#
rf.res
#
#Random Forest 
# Extract predictions
#
confusionMatrix(predict(rf.res, train, type="raw"), train$voted2016)
#
#Confusion Matrix and Statistics
#
#
confusionMatrix(predict(rf.res, test, type="raw"), test$voted2016)
#
#Confusion Matrix and Statistics
#
# Variable importance
#
rfImp <- varImp(rf.res)
plot(rfImp)

# Use Shapley values to measure feature effects
#
X <- dat[which(names(dat) != "voted2016")]
predictor <- Predictor$new(rf.res, data = X, y = css_3$Major, type="prob", class="Voted")

#
shapley <- Shapley$new(predictor, x.interest = X[1,])
shapley$plot()
shapley$results

