install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)

data = read.csv("/Users/kodavati/Downloads/training.csv")

data$purchased_previously = as.factor(data$purchased_previously)
data$opened_previously = as.factor(data$opened_previously)
data$test_var = as.factor(data$test_var)
data$none_open_buy = as.factor(data$none_open_buy)

set.seed(123) 
index = createDataPartition(data$none_open_buy, p = 0.75, list = FALSE)
trainData = data[index,]
testData = data[-index,]

model = naiveBayes(none_open_buy ~ ., data = trainData)

trainPred = predict(model, trainData)
testPred = predict(model, testData)

trainMatrix = table(Observed = trainData$none_open_buy, Predicted = trainPred)
testMatrix = table(Observed = testData$none_open_buy, Predicted = testPred)

print("Training Confusion Matrix:")
print(trainMatrix)
print("Testing Confusion Matrix:")
print(testMatrix)

trainAcc = sum(diag(trainMatrix)) / sum(trainMatrix)
testAcc = sum(diag(testMatrix)) / sum(testMatrix)

print(paste("Training Accuracy:", trainAcc))
print(paste("Testing Accuracy:", testAcc))
