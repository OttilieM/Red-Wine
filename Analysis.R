##HEADER ####
## Who: Ottilie Mitchell
## What: Analysis
## Last edited:12/11/2020
####

##CONTENTS ####
##1.0 Load data
  ##1.1 Split data by type
  ##1.2 Test and train data
##2.0 Subset selection
  ##2.1 Regsubsets
    ##2.1.1 How many variables are optimal
  ##2.2 Forwards stepwise selection
  ##2.3 Backwards stepwise selection
  ##2.4 How do they differ?
##3.0 Cross validation
  ##3.1 Create model
  ##3.2 Run prediction model
  ##3.3 Cross validation errors
  ##3.4 Plot results
##4.0 Linear regression
  ##4.1 Simple linear regression
    ##4.1.1 Quality and alcohol
    ##4.1.2 Quality and all variables
    ##4.1.3 Quality and significant variables
  ##4.2 Polynomial linear regression
    ##4.2.1 Quality and alcohol
  ##4.3 How well did the linear models perform?
  ##4.4 Testing linear models
## 5.0 Logistic regression
  ## 5.1 Logistic regression with alcohol
  ## 5.2 Logistic regression with all variables (except quality)
  ## 5.3 Logistic regression with all significant variables
  ##5.4 Testing logistic regression
##6.0 Decision trees
  ##6.1 Train data
  ##6.2 Test data
  ##6.2 Pruning?
  ##6.3 How well does the pruned tree perform?
##7.0 Bagging
  ##7.1 Train data
  ##7.2 Test data
##8.0 Random forest
  ##8.1 Train data
  ##8.2 Test data
##9.0 Boosting
  ##9.1 Train data
  ##9.2 Test data
##10.0 Which model performed best?
##11.0 Which variables are most important?
##12.0 Decision trees with red and white
  ##12.1 White
    ##12.1.1 Train data
    ##12.1.2 Test data
    ##12.1.3 Pruning?
    ##12.1.4 How well does the pruned tree perform?
  ##12.2 Red
    ##12.2.1 Train data
    ##12.2.2 Test data
    ##12.2.3 Pruning?
    ##12.2.4 How well does the pruned tree perform?
##13.0 Importance of variables
  ##13.1 White
  ##13.2 Red


##1.0 Load data
setwd("~/Data Science/C7081 - Statistical analysis for data science/Assessment")

my_data <- read.csv("winequality-whiteandred.csv")

##1.1 Split data by type
type <- split(my_data, my_data$type)
str(type)
white <- type$white
white <- type$white[,2:13]
red <- type$red
red <- type$red[,2:13]

##1.2 Test and train data
set.seed(1)
pd <- sample(2, nrow(my_data), replace = T, prob = c(0.7, 0.3))
train <- my_data[pd==1,]
test <- my_data[pd==2,]

##2.0 Subset selection
library(leaps)
##2.1 Regsubsets
regfit.full <- regsubsets(quality~., my_data, nvmax = 13)
reg.summary <- summary(regfit.full) #best 2 variables are alcohol and volatile acidity
reg.summary
##2.1.1 How many variables are optimal
reg.summary$adjr2
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R Squared", type = "l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)
# 11 variables is optimal

##2.2 Forwards stepwise selection
regfit.fwd <- regsubsets(quality~., data = my_data, nvmax = 13, method = "forward")
reg.summary.fwd <- summary(regfit.fwd) #best 2 variables are alcohol and volatile acidity
reg.summary.fwd
plot(reg.summary.fwd$adjr2, xlab= "Number of variables", ylab = "Adjusted R Squared", type = "l")
which.max(reg.summary.fwd$adjr2)
points(11, reg.summary.fwd$adjr2[11], col = "red", cex = 2, pch = 20)

##2.3 Backwards stepwise selection
regfit.bwd <- regsubsets(quality~., data = my_data, nvmax = 13, method = "backward")
reg.summary.bwd <- summary(regfit.bwd)
reg.summary.bwd #best alcohol and volatile acidity
plot(reg.summary.bwd$adjr2, xlab= "Number of variables", ylab = "Adjusted R Squared", type = "l")
which.max(reg.summary.bwd$adjr2)
points(11, reg.summary.bwd$adjr2[11], col = "red", cex = 2, pch = 20)

##2.4 How do they differ?
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)
#backwards stepwise is different after 4

par(mfrow = c(1,1))
plot(regfit.full, scale = "r2")
plot(regfit.fwd, scale = "r2")
plot(regfit.bwd, scale = "r2")
#can see that for all methods citric.acid is the least significant
#forward stepwise, could remove fixed.acidity and citic acid


##3.0 Cross validation
library(leaps)
regfit.best <- regsubsets(quality~., data = train, nvmax = 13)
coef(regfit.best, 12)
reg.summary <- summary(regfit.best)

##3.1 Create model
k = 10
set.seed(1)
folds <- sample(1:k, nrow(my_data), replace = T)
cv.errors <- matrix(NA, k, 12, dimnames = list(NULL, paste(1:12)))

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

##3.2 Run prediction model
for(j in 1:k){
  best.fit <- regsubsets(quality~., data = my_data[folds!=j,],
                        nvmax = 12)
  for(i in 1:12){
    pred <- predict(best.fit, my_data[folds==j,], id=i)
    cv.errors[j, i]=mean((my_data$quality[folds==j]-pred)^2)
  }
}

##3.3 Cross validation errors
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

##3.4 Plot results
plot(mean.cv.errors, type = "b")
which.min(mean.cv.errors)
points(11, mean.cv.errors[11], col = "red", cex = 2, pch = 20)

##4.0 Linear regression
##4.1 Simple linear regression
##4.1.1 Quality and alcohol
lm.fit <- lm(quality~alcohol, data = train)
summary(lm.fit)

plot(train$alcohol,train$quality, data = my_data, xlab = "Alcohol", ylab = "Quality")
abline(lm.fit, col = "red") #Doesn't fit the model very well

##4.1.2 Quality and all variables
lm.fit1 <- lm(quality~., data = train)
summary(lm.fit1)
##4.1.3 Quality and significant variables
lm.fit2 <- lm(quality~. -fixed.acidity -citric.acid -chlorides -pH, data = train)
summary(lm.fit2)

##4.2 Polynomial linear regression
##4.2.1 Quality and alcohol
lm.fit3 <- lm(quality~poly(alcohol, 5), data = train)
summary(lm.fit3)

##4.3 How well did the linear models perform?
AIC(lm.fit) #10592.89 (simple linear with alcohol)
AIC(lm.fit1) #10017.59 (simple linear with all variables)
AIC(lm.fit2) #10045.82 (simple linear with all significant variables)
AIC(lm.fit3) #10546 (polynomial linear regression with alcohol)
## best model was the lm.fit1 (simple linear with all variables)

##4.4 Testing linear models
predictTest <- predict(lm.fit2, newdata = test)
#MSE
lm.MSE <- mean((test[,"quality"] - predictTest)^2) #predictions of quality wrong by 0.54
lm.RMSE <- sqrt(lm.MSE)
lm.RMSE #0.733

## 5.0 Logistic regression
rating <- ifelse(as.integer(my_data$quality) > 6,1,0) #less than quality score 6 is bad, 6 or more quality score good
my_data <- data.frame(my_data, rating)
table(my_data$rating)
#need to add this to the test and train data as well
set.seed(1)
pd <- sample(2, nrow(my_data), replace = T, prob = c(0.7, 0.3))
train <- my_data[pd==1,]
test <- my_data[pd==2,]

## 5.1 Logistic regression with alcohol
par(mfrow= c(1,1))
glm.fit <- glm(my_data$rating~my_data$alcohol, family = "binomial", data = test)
summary(glm.fit) #AIC 5471
plot(train$alcohol, train$rating)
abline(glm.fit, col = "red") #better fit

## 5.2 Logistic regression with all variables (except quality)
glm.fit1 <- glm(test$rating~. -test$quality, data = test, family = "poisson")
summary(glm.fit1) #AIC 1739.4

## 5.3 Logistic regression with all significant variables
glm.fit2 <- glm(rating~. -quality -type -citric.acid -chlorides -free.sulfur.dioxide -total.sulfur.dioxide, data = test, family = "poisson")
summary(glm.fit2) #AIC 1746.5

##5.4 Testing logistic regression
prediction <- predict(glm.fit1, newdata = test, type = "response")

table(test$rating, prediction >0.5) #above 0.5 is all good
(1540+100)/(1540+100+292+64)
#82.16%
#glm.fit1 the best model and significantly more accurate than linear regression
glm.MSE <- mean(predict(glm.fit1, newdata = test, type = "response")^2) #0.2354 (better)
glm.RMSE <- sqrt(glm.MSE)
glm.RMSE #0.485

##6.0 Decision trees
library(tree)

tree_data <- my_data[,2:14] #removing type variable as a character variable
tree_data <- tree_data[,-12] #removing quality variable

tree_train <- train[, 2:14]
tree_train <- tree_train[,-12]

tree_test <- test[, 2:14]
tree_test <- tree_test[,-12]

##6.1 Train data

tree_train$rating <- as.factor(tree_train$rating) #rating as a factor
tree_test$rating <- as.factor(tree_test$rating)

tree1 <- tree(rating~., data = tree_train)
summary(tree1) #error 18.31%

plot(tree1)
text(tree1) #"0" is good, "1" is bad

##6.2 Test data
tree_prediction <- predict(tree1, newdata = tree_test, type = "class")
test_rating <- tree_test[,"rating"] #taking only the rating column from the test data set

table(tree_prediction, test_rating)

(1550+74)/(1550+74+54+318) #81.37% 
tree.MSE <- mean(predict(tree1, newdata = tree_test)^2)
tree.MSE #0.376
tree.RMSE <- sqrt(tree.MSE)
tree.RMSE #0.612
#logistic regression performed better

##6.2 Pruning?
set.seed(1)
cv.wine <- cv.tree(tree1, FUN = prune.misclass) #crossvalidation
plot(cv.wine$size, cv.wine$dev, type = "b") #best after 3 varibales

prune.wine <- prune.misclass(tree1, best = 3)
summary(prune.wine) #error rate 18.31%
plot(prune.wine)
text(prune.wine, pretty = 0)

##6.3 How well does the pruned tree perform?
tree.pred <- predict(prune.wine, newdata = tree_test, type = "class")
table(tree.pred, test_rating) 

(1550+74)/(1550+74+54+318) #81.36%
#performed the same as the unpruned tree
pruned.MSE <- mean(predict(prune.wine, newdata = tree_test)^2)
pruned.MSE #0.367
pruned.RMSE <- sqrt(pruned.MSE)
pruned.RMSE #0.606

##7.0 Bagging
library(randomForest)
##7.1 Train data
set.seed(1)
bag.wine <- randomForest(rating~., data = tree_train, mtry = 12, importance = T) #uses full set of predictors

##7.2 Test data
yhat.bag <- predict(bag.wine, newdata = tree_test, type = "class")
table(yhat.bag, test_rating)
(1539+219)/(1539+219+173+65)
#88.08% accuracy
test_rating <- as.numeric(test_rating)
yhat.bag <- as.numeric(yhat.bag)
bagging.MSE <- mean((yhat.bag-test_rating)^2)
bagging.MSE #0.119
bagging.RMSE <- sqrt(bagging.MSE)
bagging.RMSE #0.345

##8.0 Random forest
#random forest uses a random set of predictors for splitting
#optimal number of variables = sqrt(K)
sqrt(12)
#optimal number of variables = 3.464102
set.seed(1)

##8.1 Train data
rf_wine <- randomForest(rating~., data = tree_train, mtry = 3.46, importance = T)

##8.2 Test data
rf_prediction <- predict(rf_wine, newdata = tree_test, type = "class")

table(rf_prediction, test_rating)
(1552+208)/(1552+208+52+184)
#88.18% this is the best model so far
rf_prediction <- as.numeric(rf_prediction)
RandomForest.MSE <- mean((rf_prediction-test_rating)^2)
RandomForest.MSE #0.118
RandomForest.RMSE <- sqrt(RandomForest.MSE)
RandomForest.RMSE #0.344

##9.0 Boosting
library(gbm)

##9.1 Train data
set.seed(1)
tree_train$rating <- as.character(tree_train$rating)
boost_wine <- gbm(rating~., data = tree_train, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4)

##9.2 Test data
pred_boost <- predict(boost_wine, newdata = tree_test, n.trees = 5000, type = "response")

table(pred_boost > 0.5, test_rating)

(1505+232)/(1505+232+99+160)
#87.02% accurate
pred_boost <- as.numeric(pred_boost)
Boost.MSE <- mean((pred_boost-test_rating)^2)
Boost.RMSE <- sqrt(Boost.MSE) 
Boost.RMSE #1.070
#randomForest performed the best

##10.0 Which model performed best?
library(ggplot2)
library(forcats)
library(tidyverse)
results <- data.frame(name = c("Linear regression", "Logistic regression", "Decision tree", "Pruned tree", "Bagging", "Random Forest", "Boosting"), 
                      RMSE =c(0.733, 0.485, 0.613, 0.606, 0.345, 
                               0.344, 1.07))

results$name <- as.factor(results$name)

plot(results$name, results$RMSE,
     main = "RMSE of all models",
     xlab = "Model",
     ylab = "RMSE",
     cex.axis = 0.5)

bar <- results %>%
  #ordering model names from best to worst
  mutate(name = fct_reorder(name, RMSE)) %>%
  ggplot( aes(x=name, y=RMSE)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  #add bar labels
  geom_text(aes(label=RMSE)) +
  #flip the graph
  coord_flip() +
  xlab("Models") +
  theme_bw()
bar

##11.0 Which variables are the most important?
varImpPlot(rf_wine)

##12.0 Decision trees with red and white
##12.1 White
Good.white <- ifelse(white$quality<=6, 1, 0)
white <- data.frame(white, Good.white)

##12.1.1 Train data
tree.white <- tree(as.factor(Good.white)~.-quality, white)
summary(tree.white) #error rate 21.27%

plot(tree.white)
text(tree.white, pretty = 0)

##12.1.2 Test data
set.seed(1)
train.white <- sample(1:nrow(white), 2449)
test.white <- white[-train.white,]

Good.white.test = Good.white[-train.white]
tree.white <- tree(as.factor(Good.white)~.-quality, white, subset = train.white)

tree.pred <- predict(tree.white, test.white, type = "class")
table(tree.pred, Good.white.test)
(1827+124)/(1827+124+76+422)
#79.67% accuracy

##12.1.3 Pruning?
set.seed(1)
cv.white <- cv.tree(tree.white, FUN = prune.misclass)

plot(cv.white$size, cv.white$dev, type = "b") #best after 3

prune.white <- prune.misclass(tree.white, best = 3)
plot(prune.white)
text(prune.white, pretty = 0)

##12.1.4 How well does the pruned tree perform?
tree.pred <- predict(prune.white, test.white, type = "class")
table(tree.pred, Good.white.test) 
(1827+124)/(1827+124+76+422)

#79.67% (same)

##12.2 Red
Good.red <- ifelse(red$quality<=6, 1, 0)
red <- data.frame(red, Good.red)

##12.2.1 Train data
tree.red <- tree(as.factor(Good.red)~.-quality, red)
summary(tree.red) #error rate 9.94%

plot(tree.red)
text(tree.red, pretty = 0)

##12.2.2 Test data
set.seed(1)
train.red <- sample(1:nrow(red), 798)
test.red <- red[-train.red,]
Good.red.test = Good.red[-train.red]
tree.red <- tree(as.factor(Good.red)~.-quality, data = test.red)
tree.pred <- predict(tree.red, test.red, type = "class")
table(tree.pred, Good.red.test) 
(68+674)/(68+674+41+18)
#92.63% accuracy

##12.2.3 Prune?
set.seed(1)
cv.red <- cv.tree(tree.red, FUN=prune.misclass)
plot(cv.red$size, cv.red$dev, type = "b") #best using 10 variables

prune.red <- prune.misclass(tree.red, best = 10)
plot(prune.red)
text(prune.red, pretty = 0)

##12.2.4 How well did the pruned tree perform?
tree.pred <- predict(prune.red, test.red, type = "class")

table(tree.pred, Good.red.test) 
(60+680)/(60+680+12+49)
#92.38%

##13.0 Importance of variables
##13.1 White
library(randomForest)
set.seed(1)
white.fit <- randomForest(Good.white~. -quality, data = white, importance = T, ntree=2000)
varImpPlot(white.fit)

##13.2 Red
set.seed(1)
red.fit <- randomForest(Good.red~. -quality, data = red, importance = T, ntree=2000)
varImpPlot(red.fit)
