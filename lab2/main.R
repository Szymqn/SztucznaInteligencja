install.packages(c("kernlab", "tidyverse", "e1071", "caTools", "caret", "class"))

library(kernlab)
library(tidyverse)
library(e1071)
library(caTools)
library(caret)
library(class)
library(caret)

set.seed(42)

#### iris ####
data("iris")
head(iris)

rows <- sample.int(nrow(iris), size = round(nrow(iris)/3), replace = F)
iris.train <- iris[-rows,]
iris.test <- iris[rows,]

# Bayes Classifier
iris.NB <- naiveBayes(x = subset(iris.train, select = -c(Species)), y = iris.train['Species'])
iris.NB.pred <- predict(iris.NB, newdata = iris.test)

confusion_matrix <- table(iris.NB.pred, iris.test$Species)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")

# KNN
iris.KNN.tC <- trainControl(method  = "cv",
                          number  = 5)

iris.KNN.fit <- train(Species ~ .,
                  method     = "knn",
                  tuneGrid   = expand.grid(k = 1:10),
                  trControl  = trControl,
                  metric     = "Accuracy",
                  data       = iris)
iris.KNN.fit

#### spam ####
data(spam)
head(spam)

rows <- sample.int(nrow(spam), size = round(nrow(spam)/3), replace = F)
spam.train <- spam[-rows,]
spam.test <- spam[rows,]

# Bayes Classifier
spam.NB <- naiveBayes(x = subset(spam.train, select = -c(type)), y = spam.train['type'])
spam.NB.pred <- predict(spam.NB, newdata = spam.test)

spam.confusion_matrix <- table(spam.NB.pred, spam.test$type)
spam.accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(spam.confusion_matrix)
cat("Accuracy:", round(spam.accuracy * 100, 2), "%\n")

# KNN
spam.KNN.tC <- trainControl(method  = "cv",
                            number  = 5)

spam.KNN.fit <- train(type ~ .,
                      method     = "knn",
                      tuneGrid   = expand.grid(k = 1:10),
                      trControl  = trControl,
                      metric     = "Accuracy",
                      data       = spam)

spam.KNN.fit