library(tidyverse)
library(ggplot2)
library(dplyr)
library(haven)
library(caret)
library(car)


# i) Read in Advertising data into R

advertising <- read.csv("C:/Users/asmin/Downloads/Advertising.csv")

# ii) Check and remove NA values

colSums(is.na(advertising)) # No missing values in dataset

advertising <- na.omit(advertising) # If missing values are present use this

# iii) Explore data

dim(advertising)

names(advertising)

summary(advertising)

View(advertising)

plot1 <- advertising %>% 
  ggplot(aes(x = TV, y = sales))+
  geom_point(color = "royalblue")+
  labs(title = "TV vs. Sales", x = "TV", y = "Sales")+
  theme_classic()+
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

plot1

hist(advertising$TV, main = "Histogram of TV", xlab = "TV", col = "royalblue")

hist(advertising$sales, main = "Histogram of Sales", xlab = "Sales", col = "darkred")

# iv) Divide data into training and testing sets

# set seed for reproducibility

set.seed(567)

# Index for training data
trainIndex <- sample(seq_len(nrow(advertising)), size = 0.7*nrow(advertising))

# Splitting data
trainData <- advertising[trainIndex, ]
testData <- advertising[-trainIndex, ]

View(trainData)
View(testData)

# v) Using training data regress sales ~ TV...

# Fit linear regression model
model1 <- lm(sales ~ TV, data = trainData)

summary(model1)

# Plot training data and regression line
plot(trainData$TV, trainData$sales, main = "Sales vs TV",
     xlab = "TV", ylab = "Sales", pch = 19, col = "royalblue")
abline(model1, col = "darkred", lwd = 2)

# Predict Responses Using the Test Data
predicted_sales <- predict(model1, newdata = testData)

# Plot predicted vs actual responses 
plot(testData$sales, predicted_sales, main = "Predicted Sales vs. Actual Sales",
     xlab = "Actual Sales", ylab = "Predicted Sales", pch = 19, col = "darkgreen")
abline(0, 1, col = "red", lwd = 2)

# Calculate RMSE 
rmse <- sqrt(mean((predicted_sales - testData$sales)^2))
print(paste("RMSE:", round(rmse, 2)))

# vi) Using training data regress sales ~ TV + (TV)^2.....

model2 <- lm(sales ~ TV + I(TV^2), data = trainData)

summary(model2)

# Create a sequence of TV values for plotting the regression line
tv_values <- seq(min(trainData$TV), max(trainData$TV), length.out = 100)

# Predict sales using the model for the generated TV values
predicted_sales_line <- predict(model2, newdata = data.frame(TV = tv_values))

# Plotting the training data
plot(trainData$TV, trainData$sales, main = "Sales vs. TV (Quadratic Fit)",
     xlab = "TV", ylab = "Sales", pch = 19, col = "royalblue3")

# Add the fitted regression line
lines(tv_values, predicted_sales_line, col = "red", lwd = 2)

# Predict on the testing data
predict_sales2 <- predict(model2, newdata = testData)

# Plot predicted vs actual responses
plot(testData$sales, predict_sales2, main = "Predicted vs Actual Sales",
     xlab  = "Actual Sales", ylab = "Predicted Sales", pch = 19, col = "darkgreen")
abline(0,1, col = "red", lwd = 2)

# Calculate RMSE 
rmse2 <- sqrt(mean((predict_sales2 - testData$sales)^2))
print(paste("RMSE:", round(rmse2, 2)))

# ANOVA test to compare model 1 and model 2
anova(model1, model2)

# vii) Check if these two model are significantly different

# Although it is not significant, there is a difference between our two models.
# Here we can just compare the two RMSE values we computed for each model and see
# which one is better. The second model has a lower RMSE value (3.59) than the 
# first model (3.62). A lower RMSE value indicates better predictive accuracy, so 
# the second model is better than the first one. The inclusion of the TV^2 value 
# makes our model better. When we plotted the response variable (sales) against the 
# predictor (TV) a linear fitted regression line was not the most optimal to use as the 
# relationship between sales & TV is best represented as a quadratic function which 
# is non linear. Our lower RMSE value for the second model makes sense as the 
# inclusion of a squared predictor ended up improving our predictive accuracy.
# The R-squared and adj. R-squared were nearly the same for each model

