library(kernlab)
library(e1071)
library(tidyverse)

data(iris)
rows.iris <- sample.int(nrow(iris), size = round(nrow(iris)/3), replace = F, set.seed(100))
iris.train <- iris[-rows.iris,]
iris.test <- iris[rows.iris,]
iris.acc <- c()
iris.gamma <- c()

for (i in 1:10)
{
  gamma = i/10
  svm.model <- svm(x = iris.train[,-5], y = iris.train[,5],kernel = "radial", gamma = gamma)
  svm.result <- predict(svm.model,newdata = iris.test[,-5])
  table(svm.result,iris.test$Species)
  acc <- sum(svm.result==iris.test$Species)/length(iris.test$Species)
  iris.acc <- append(iris.acc, acc)
  iris.gamma <- append(iris.gamma, gamma)
}

iris.acc
iris.gamma

data(spam)

rows.spam <- sample.int(nrow(spam), size = round(nrow(spam)/3), replace = FALSE, set.seed(100))
spam.train <- spam[-rows.spam, ]
spam.test <- spam[rows.spam, ]
spam.acc <- c()
spam.gamma <- c()

for (i in 1:10) {
  gamma <- i/10
  svm.model <- svm(x = spam.train[, -58], y = spam.train[, 58], kernel = "radial", gamma = gamma)
  svm.result <- predict(svm.model, newdata = spam.test[, -58])
  acc <- sum(svm.result == spam.test[, 58]) / nrow(spam.test)
  spam.acc <- append(spam.acc, acc)
  spam.gamma <- append(spam.gamma, gamma)
}

spam.acc
spam.gamma

plot(iris.gamma, iris.acc, type = "l", col = "blue", ylim = c(0, 1),
     xlab = "Gamma", ylab = "Accuracy", main = "Accuracy vs. Gamma")
points(iris.gamma, iris.acc, col = "blue", pch = 19)

lines(spam.gamma, spam.acc, type = "l", col = "red")
points(spam.gamma, spam.acc, col = "red", pch = 19)

legend("bottomright", legend = c("Iris", "Spam"), col = c("blue", "red"), lty = 1, cex = 0.8)

