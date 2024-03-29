# Machine learning of institutional variables
# Torbet McNeil
# tmcneil@email.arizona.edu
# 2021-7-4

library("caret")
library("parallel")
library("iml")
cl <- makeCluster(detectCores() - 1)
# Create folders
dir.create(path = "data")
dir.create(path = "output")

# Read in data
comp <- read.csv("C:/Users/ual-laptop/Desktop/css_3.csv")
#Change to relative path: comp <- read.csv(file = "data/css_3.csv")

comp$Major <- factor(comp$Major, labels=c("No_DS", "DS"))

# Split data
set.seed(1985)

# Create train and test indices
trainIndex <- createDataPartition(comp$Major, p=0.2, list = FALSE, times = 1)

train <- comp[trainIndex,]
test <- comp [-trainIndex,]

#Set control parameters for model training
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

# Set testing grid for random forest
rfGrid <-  expand.grid(mtry = 1:6)
set.seed(1985)
rf.res <- train(Major ~ .,
                data=train,
                method="rf",
                trControl=fitCtrl,
                tuneGrid=rfGrid,
                #tuneLength=10,
                metric="ROC",
                verbose=FALSE)
rf.res
#Random Forest 
# Extract predictions
confusionMatrix(predict(rf.res, train, type="raw"), train$Major)
#
#Confusion Matrix and Statistics
#
confusionMatrix(predict(rf.res, test, type="raw"), test$Major)
#
#Confusion Matrix and Statistics

# Variable importance
rfImp <- varImp(rf.res)
plot(rfImp)
#
##For the random forest, the training accuracy was ____%, and the testing accuracy was ___%. The three most important features from the random forest were_______. 

# Use Shapley values to measure feature effects

X <- comp[which(names(comp) != "Major")]
predictor <- Predictor$new(rf.res, data = X, y = comp$Major, type="prob", class="DS”)
library("iml")

shapley <- Shapley$new(predictor, x.interest = X[1,])
shapley$plot()
shapley$results

##I used Shapley values to predict the effects of  on  from the random forest. In #this observation, does not influence the predicted value of the model as the Shapley value is #0.00;  had the 8th/9th smallest contribution in magnitude to the model. 

