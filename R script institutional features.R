install.packages("caret")
install.packages("randomForest")
install.packages("mlbench")
install.packages("parallel")
install.packages("doParallel")
install.packages("foreach")
install.packages("haven")
install.packages("MASS")

install.packages("pROC")
install.packages("party")
install.packages("dplyr")
install.packages("ggraph")
install.packages("igraph")
install.packages("rpart.plot")

library("caret")

library("mlbench")
library("parallel")
library("doParallel")
library("foreach")
library("haven")
library("MASS")

library("randomForest")
library("pROC")
library("party")
library("dplyr")
library("ggraph")
library("igraph")
library("rpart.plot")

cl <- makeCluster(detectCores() - 1)

dir.create(path = "data")
dir.create(path = "output")

practice <- read.csv("C:/Users/ual-laptop/Desktop/Taxonomy.csv")

comp <- read.csv("C:/Users/ual-laptop/Desktop/css.csv")

#Split data
set.seed(1985)

#
train <- comp[trainIndex,]
test <- comp [-trainIndex,]
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

# Set testing grid for random forest
#
rfGrid <-  expand.grid(mtry = 1:6)
#
set.seed(1985)
rf.res <- train(Major ~ .,
                data=train,
                method="rf",
                trControl=fitCtrl,
                tuneGrid=rfGrid,
                #tuneLength=10,
                metric="ROC",
                verbose=FALSE)
#
rf.res

