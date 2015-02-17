library(caret)
library(randomForest)
library(gbm)
library(plyr)

# read the csv file for training from my working directory and delete columns with NAs
data_tr_original<- read.csv("pml-training.csv", na.strings= c("NA",""," "))
data_tr_NAs <- apply(data_tr_original, 2, function(x) {sum(is.na(x))})
data_tr_clean <- data_tr_original[,which(data_tr_NAs == 0)]

# delete for the prediction useless identifier columns eg row number
data_tr_clean <- data_tr_clean[8:length(data_tr_clean)]

# divide the cleaned training data into separate training and testing set
inTrain <- createDataPartition(y = data_tr_clean$classe, p = 0.7, list = FALSE)
training <- data_tr_clean[inTrain, ]
testing <- data_tr_clean[-inTrain, ]

#compare boosting ('gbm'), and random forests (rf) to cross validate feature selection
# with cross validation to find the best method 
# performing three runs of a five fold cross validation
fitControl <- trainControl(method = "repeatedcv",number = 5,repeats = 3)
# this one takes quite a while (at least on my laptop)
modelgbm <- train(classe ~ .,method='gbm',data = training,trControl=fitControl, verbose=FALSE) 
modelrf<-train(classe ~ .,method='rf',data = training,trControl=fitControl)

# evaluate the model using the remaining 30% of the data as testing set
predictTestinggbm <- predict(modelgbm, testing) 
confusionMatrix(testing$classe, predictTestinggbm) #accuracy 0.9626

predictTestingrf <- predict(modelrf, testing)
confusionMatrix(testing$classe, predictTestingrf) #accuracy 0.992

#Choose the better performing model (random forest in this case) 
# on the testing set for use on the real prediction test set

# first load prediction test data and apply the same transformation as above for the training data
data_te_original <- read.csv("pml-testing.csv", na.strings= c("NA",""," "))
data_te_NAs <- apply(data_te_original, 2, function(x) {sum(is.na(x))})
data_te_clean <- data_te_original[,which(data_te_NAs == 0)]
data_te_clean <- data_te_clean[8:length(data_te_clean)]

# Prediction of the classes in the real prediction test set
predictTest <- predict(modelrf, data_te_clean)
