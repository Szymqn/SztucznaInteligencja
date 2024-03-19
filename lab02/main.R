install.packages(c("kernlab", "tidyverse", "e1071",
                   "caTools", "caret", "class"))

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
                  trControl  = iris.KNN.tC,
                  metric     = "Accuracy",
                  data       = iris)
iris.KNN.fit

cat("Accuracy:", mean(iris.KNN.fit[["results"]][["Accuracy"]]))

#### spam ####
data(spam)
head(spam)
table(spam['type'])

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
                      trControl  = iris.KNN.tC,
                      metric     = "Accuracy",
                      data       = spam)
spam.KNN.fit

cat("Accuracy:", mean(spam.KNN.fit[["results"]][["Accuracy"]]))


# Load required library
library(ggplot2)

# Create a data frame with the provided data
data <- data.frame(
  Database = c("iris", "spam", "iris", "spam"),
  Classifier = c("Bayes", "Bayes", "KNN", "KNN"),
  Accuracy = c(0.98, 0.98, 0.968666, 0.795154)
)

# Create the plot
plot <- ggplot(data, aes(x = Database, y = Accuracy, fill = Classifier)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Accuracy of Bayes and KNN Classifiers",
       x = "Database",
       y = "Accuracy",
       fill = "Classifier") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
print(plot)

