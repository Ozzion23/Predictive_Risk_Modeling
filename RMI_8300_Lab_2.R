library(tidyverse)
library(ggplot2)
library(dplyr)
library(haven)
library(car)

# 1) Read in Advertising csv file

advertising <- read.csv("C:/Users/asmin/Downloads/Advertising.csv")

View(advertising)

# 2) Divide data into training and testing sets

set.seed(827)

trainIndex <- sample(seq_len(nrow(advertising)), size = 0.7*nrow(advertising))

trainData <- advertising[trainIndex, ]
testData <- advertising[-trainIndex, ]

View(trainData)
View(testData)


# 3) Using training data to find the best model, based on adj. R^2

# Creating regression models

Best1 <- lm(sales ~ TV, data = trainData)
Best2 <- lm(sales ~ TV + radio, data = trainData)
Best3 <- lm(sales ~ TV + radio + newspaper, data = trainData)

summary(Best1)
summary(Best2)
summary(Best3)

# Collect adj R squared values for each model

best_models <- list(Best1, Best2, Best3)
adjusted_r2_values <- sapply(best_models, function(model) summary(model)$adj.r.squared)

# Find the best model by adjusted R squared 
# Best model was Best 2

best_model_index <- which.max(adjusted_r2_values)
Best0 <- best_models[[best_model_index]]

summary(Best0)

# 4) Using testing data find the best model from Best 1,2,3 in terms of RMSE
#    and compare with Best 0 in terms of RMSE

# First, predict sales for each model using testing data

pred_Best1 <- predict(Best1, newdata = testData)
pred_Best2 <- predict(Best2, newdata = testData)
pred_Best3 <- predict(Best3, newdata = testData)
pred_Best0 <- predict(Best0, newdata = testData)

# RMSE calculation function

rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

# Calculate RMSE for each model on testing data

rmse_Best1 <- rmse(testData$sales, pred_Best1)
rmse_Best2 <- rmse(testData$sales, pred_Best2)
rmse_Best3 <- rmse(testData$sales, pred_Best3)
rmse_Best0 <- rmse(testData$sales, pred_Best0)

# Find best model based on RMSE

rmse_values <- c(rmse_Best1, rmse_Best2, rmse_Best3)
Best123_index <- which.min(rmse_values)
Best123 <- list(Best1, Best2, Best3)[[Best123_index]]

Best123

# RMSE comparison for each model

rmse_comparison <- data.frame(
  Model = c("Best1", "Best2", "Best3", "Best0"),
  RMSE = c(rmse_Best1, rmse_Best2, rmse_Best3, rmse_Best0)
)

print(rmse_comparison)

# 5) Conduct residual analysis and calculate VIF for Best 123 and Best 0

# Regression model with whole dataset

Best_0 <- lm(sales ~ TV + radio, data = advertising)

# Residual analysis for Best 0

plot(Best_0$fitted.values, residuals(Best_0),
     main = " Figure 1: Residuals vs. Fitted (Best0 & Best123)", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(residuals(Best_0), main = "Figure 2: Q-Q Plot (Best0 & Best123)")
qqline(residuals(Best_0), col = "red")

hist(residuals(Best_0), main = "Figure 3: Histogram of Residuals (Best0 & Best123)", 
     xlab = "Residuals")

plot(Best_0, which = 3, main = "Figure 4: Linearity (Best0 & Best123)")

vif_Best0 <- vif(Best_0)
vif_Best0
