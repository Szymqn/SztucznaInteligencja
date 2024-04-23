install.packages(c("kernlab", "adabag", "randomForest"))

library(kernlab)
library(adabag)
library(randomForest)
library(ggplot2)

data(spam)

rows <- sample.int(nrow(spam), size = round(nrow(spam)/3), replace = F)
spam.train <- spam[-rows,]
spam.test <- spam[rows,]

spam.model.bagging<-bagging (formula = type~., data = spam.train, nbagg = 20)
spam.bagging.predict <- predict(spam.model.bagging, newdata = spam.test[,-58])
error.bagging2 <- sum(spam.test[,58] != spam.bagging.predict$class)/length(spam.bagging.predict$class)
acc.bagging2 <- 1 - error.bagging2

model.boosting <- boosting(formula = type~., data = spam.train, mfinal = 200)
boosting.result <- predict(model.boosting, newdata = spam.test)
error.boosting1 <- sum(spam.test[,58] != boosting.result$class)/length(boosting.result$class)
acc.boosting1 <- 1 - error.boosting1

model.rf <- randomForest( x = spam.train[, -58], y = spam.train[,58],do.trace = 25, ntree = 5000, importance = T)
rf.result <- predict(model.rf, newdata = spam.test[,-58])
error.rf1 <- sum(spam.test[,58] != rf.result)/length(rf.result)
acc.rf1 <- 1 - error.rf1

rows <- sample.int(nrow(spam), size = round(nrow(spam)/3), replace = F)
spam.train <- spam[-rows,]
spam.test <- spam[rows,]

spam.cart.model <-rpart(formula = type~., data = spam.train)
spam.cart.predict <- predict(spam.cart.model, newdata = spam.test[,-58], type = "class")
error.cart <- sum(spam.test[,58] != spam.cart.predict)/length(spam.cart.predict)
acc.cart <- 1 - error.cart

data <- data.frame(
  Classifier = c( "Decision Tree", "Random Forest", "Bagging", "Boosting"),
  Accuracy = c(acc.cart, acc.rf1, acc.bagging2, acc.boosting1)
)

plot <- ggplot(data, aes(x = Classifier, y = Accuracy, fill = Classifier)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = 
         "Accuracy of Decision tree, Random forest, Bagging and Boosting",
          x = "Classifier",
          y = "Accuracy",
          fill = "Classifier") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

print(plot)

