library(rpart)
library(kernlab)
library(pROC)
library(mltools)

data(spam)

rows <- sample.int(nrow(spam), size = round(nrow(spam)/3), replace = F)
spam.train <- spam[-rows,]
spam.test <- spam[rows,]
spam.cart.model <-rpart(formula = type~., data = spam.train)

spam.cart.predict <- predict(spam.cart.model, newdata = spam.test[,-58], type = "class")
error.cart <- sum(spam.test[,58] != spam.cart.predict)/length(spam.cart.predict)
acc.cart <- 1 - error.cart

plot(spam.cart.model)
text(spam.cart.model)

spam.cart.model <-rpart(formula = type~., data = spam.train)

spam.cart.predict <- predict(spam.cart.model, newdata = spam.test[,-58], type = "class")
error.cart <- sum(spam.test[,58] != spam.cart.predict)/length(spam.cart.predict)

plot(spam.cart.model)
text(spam.cart.model)

mcc <- mcc(spam.test[,58], spam.cart.predict)
mcc

auc(spam.test[,58], as.numeric(spam.cart.predict)-1)
spam.cart.predict <- predict(spam.cart.model, newdata = spam.test[,-58], type = "prob")[,1]
auc(spam.test[,58], as.numeric(spam.cart.predict))

