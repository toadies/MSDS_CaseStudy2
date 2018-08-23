#
# Untappd: Beer me - Prediction of beer for connoisseurs
#

# Read the data
master.user.profile <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/MasterUserProfile.csv")

# add preference column that has 
# Subset random 80% of the observations for training, only the columns that we use as Predictors and the remaining observations for test
train.user.profile <- master.user.profile[sample(nrow(master.user.profile), nrow(master.user.profile) * 0.8), c(1:39,45)]
test.user.profile <- master.user.profile[!(master.user.profile$CheckinId %in% train.user.profile$CheckinId), c(1:39,45)]

# Load couple of libraries for Random Forest Model
library(randomForest)
library(h2o)

# First get a model using the randomForest library
# -  ERROR 1: Can not handle categorical predictors with more than 53 categories. 
#             - IBU has 102 categories, ABV has 115 categories - IBU is Numeric but the problem may be with ABV which is a factor. Try converting
#               to numeric? - Easy to change in Excel - Seem to have worked!!!
#
user.profile.rf <- randomForest(train.user.profile[,-c(1,40)], train.user.profile[,40])


# Get some details about the model
user.profile.rf
user.profile.rf$importance
plot(user.profile.rf)
plot(sort(user.profile.rf$importance))

# Let's do some prediction for the test data
user.rating.prediction <- round(predict(user.profile.rf, test.user.profile[,-c(1,40)]), 2)
user.rating.prediction <- round(predict(user.profile.rf, train.user.profile[,-c(1,40)]), 2)

# How good did we do? Evaluate the results
library(pROC)
library(ggplot2)
library(reshape2)
library(caret)
library(ModelMetrics)

user.rating.reference <-  as.factor(test.user.profile[,40])

# Confusion Matrix
confusionMatrix(data=as.factor(user.rating.prediction), as.factor(user.rating.reference), positive='yes')
# ERROR: `data` and `reference` should be factors with the same levels. - May be we need to round the predicted values to nearest 0.25?

# Let's round and see
user.rating.prediction.round <- as.factor(round(user.rating.prediction/0.25) * 0.25)
confusionMatrix(data=user.rating.prediction.round, reference=user.rating.reference, positive='yes')
# ERROR: The data contain levels not found in the data.
# Levels (Predicted): 1.25 1.5 2.25 2.5 2.75 3 3.25 3.5 3.75 4 4.25 4.5 4.75
# Levels (Test)     : 1 1.25 2 2.25 2.5 2.75 3 3.25 3.5 3.75 4 4.25 4.5 4.75 5
#
# Found somewhere to set the levels for both the prediction and reference from 1 to 5 in 0.25 increments
#

levels(user.rating.prediction.round) <- seq(1,5,0.25)
levels(user.rating.reference) <- seq(1,5,0.25)
confusionMatrix(data=user.rating.prediction.round, reference=user.rating.reference, positive='yes')

# Second get a model using limited features
user.profile.rf.2 <- randomForest(train.user.profile[,c(2, 3, 4, 38, 39)], as.factor(train.user.profile[,40]>=3.5))
user.profile.rf.2
user.profile.rf.2$importance
plot(user.profile.rf.2)
plot(sort(user.profile.rf.2$importance))

user.profile.rf.2 <- randomForest(train.user.profile[,-c(1,40)], as.factor(train.user.profile[,40]>=3.5))
user.rating.prediction.2 <- predict(user.profile.rf.2, test.user.profile[,-c(1,40)])


