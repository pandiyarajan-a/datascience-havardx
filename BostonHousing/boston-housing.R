if(!require(gbm)) install.packages("gbm")
if(!require(MASS)) install.packages("MASS")

library(purrr)
library(ggplot2)
library(knitr)
library(magrittr)
library(tidyr)
library(stringr)
library(dplyr)
library(kableExtra)
library(stringr)
library(gridExtra)
library(grid)
library(corrr)
library(caret)

# Loading data
data("Boston")

# data structure or descriptio of data
str(Boston)

# Data Analysis
Boston %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins = 10)


Boston %>%
  gather(-medv, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = medv)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ var, scales = "free") +
  ylab('Price')

Boston %>%  correlate() %>% rplot()

################################
# Data Wrangling
################################
boston <- Boston %>% select(rm,lstat,ptratio,medv)


################################
# Cost Function - RMSE 
################################
RMSE <- function(actual,predicted){
  sqrt(mean((actual-predicted)^2))
}

################################
# Data Preparation
################################
set.seed(2019)

validation_index <- createDataPartition(y=boston$medv,times = 1, p = 0.1, list = FALSE)
boston_data_set <- boston[-validation_index,]
validation_set <-  boston[validation_index,]

# Test and Train set
test_index <- createDataPartition(y=boston_data_set$medv,times = 1, p = 0.1, list = FALSE)
train_set <- boston_data_set[-test_index,]
test_set <- boston_data_set[test_index,]

################################
# Prection system
################################

# naive model
mu_train <- mean(train_set$medv)
naive_rmse <- RMSE(test_set$medv, mu_train)
rmse_results <- tibble(method = "Naive model", RMSE = naive_rmse) 

# Simple Linear Regression Model
linear_model <- lm(medv~.,data=train_set) 
prediction = predict(linear_model, test_set[, 1:3])
lm_rmse <- RMSE(test_set$medv,prediction)

rmse_results <- bind_rows(rmse_results, tibble(method = "Linear regression Model", 
                                               RMSE = lm_rmse) ) 


# Gradient Boosting Model

gradient_boost <- gbm(medv ~ . ,data = train_set ,
                     distribution = "gaussian", 
                     n.trees = 10000,
                     shrinkage = 0.01, 
                     interaction.depth = 4)

summary(gradient_boost)
n.trees = seq(from=100 ,to=10000, by=100)  #no of trees-a vector of 100 values 
pred_matrix <- predict(gradient_boost, test_set[, 1:3],n.trees = n.trees) # Generating a Prediction matrix for each Tree

rmses <- apply(pred_matrix,2,function(p){
  RMSE(test_set$medv,p)                   # Calculating The Root Mean squared Test Error
})

gbm_rmse <- rmses[which.min(rmses)]

n.trees[which.min(rmses)]

plot(n.trees , rmses ,  
     pch=19,col="blue",
     xlab="Number of Trees",
     ylab="Test Error", 
     main = "Perfomance of Boosting on Test Set")
abline(h = min(rmses),col="red") 
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)


rmse_results <- bind_rows(rmse_results, tibble(method = "Gradient Boosting Model (GBM)", RMSE = gbm_rmse))

################################################
# Gradient Boosting Model on Validation data set
################################################

gradient_boost <- gbm(medv ~ . ,data = train_set, distribution = "gaussian", 
                      n.trees = n.trees[which.min(rmses)], shrinkage = 0.01, 
                      interaction.depth = 4)
prediction <- predict(gradient_boost, validation_set[, 1:3], n.trees = gradient_boost$n.trees)
v_gbm_rmse <- RMSE(validation_set$medv,prediction)

rmse_results <- bind_rows(rmse_results, tibble(method = "Gradient Boosting Model (GBM) with Validation set", RMSE = v_gbm_rmse))

################################################
# Results 
################################################
rmse_results

################################################
# least RMSE Value on validation set
################################################
v_gbm_rmse
