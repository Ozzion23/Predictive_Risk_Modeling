library(tidyverse)
library(ggplot2)
library(dplyr)
library(haven)
library(car)
library(bestglm)
library(caret)

# 1) Read in data
universal_bank <- read.csv("C:/Users/achar/Downloads/UniversalBank.csv")

# 2) Explore data

dim(universal_bank)

summary(universal_bank)

names(universal_bank)

sum(universal_bank$Personal.Loan == 1)
sum(universal_bank$Personal.Loan == 0)

plot(universal_bank$Experience, universal_bank$Income, xlab = "Experience",
     ylab = "Income", main = "Income vs. Experience")

plot(universal_bank$Education, universal_bank$Income, xlab = "Education",
     ylab = "Income", main = "Income vs. Education")

plot(universal_bank$Education, universal_bank$Experience, xlab = "Education",
     ylab = "Experience", main = "Experience vs Education")

plot(universal_bank$Age, universal_bank$Education, xlab = "Age", ylab = "Education",
     main = "Education vs. Age")

# 3) Divide Data into training and testing data

set.seed(366)

trainIndex <- sample(seq_len(nrow(universal_bank)), size = 0.7*nrow(universal_bank))

train_data <- universal_bank[trainIndex, ]
test_data <- universal_bank[-trainIndex, ]


# 4)

# Build the logistic regression model
model <- glm(Personal.Loan ~ Age + Income + Family + CCAvg + Education + 
               Mortgage + Securities.Account + CD.Account + Online + CreditCard,
             data = train_data, family = "binomial")

# Summary of Logit Model
summary(model)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(predicted_classes == test_data$Personal.Loan)
print(paste("Accuracy:", accuracy))

# Create a confusion matrix
conf_matrix <- table(Predicted = predicted_classes, 
                     Actual = test_data$Personal.Loan)
print(conf_matrix)

98/(98+23)

# 5)

# Prepare the data for bestglm
X <- train_data[, c("Age", "Income", "Family", "CCAvg", "Education", 
                    "Mortgage", "Securities.Account", "CD.Account", "Online", 
                    "CreditCard")]
y <- train_data$Personal.Loan

# Create a data frame for bestglm
bestglm_data <- data.frame(y, X)

# Initialize a list to store models and their accuracies
best_models <- list()
accuracies <- numeric(11)

# Loop through the ten predictors
for (i in 1:11) {
  # Find the best model with i predictors
  best_model <- bestglm(Xy = bestglm_data, family = binomial, 
                        IC = "AIC", method = "exhaustive", 
                        nvmax = i, TopModels = 1)
  # Store the model
  best_models[[i]] <- best_model$BestModel
  
  # Make predictions on the test set
  predictions <- predict(best_model$BestModel, newdata = test_data, type = "response")
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  
  # Calculate and store accuracy
  accuracies[i] <- mean(predicted_classes == test_data$Personal.Loan)
  
  print(paste("Model with", i-1, "predictor(s) - Accuracy:", accuracies[i]))
}

# Find the best model based on accuracy
best_model_index <- which.max(accuracies)

print(paste("The best model has", best_model_index - 1, 
            "predictor(s) with an accuracy of", max(accuracies)))

# Display the variables in the best model
print("Variables in the best model:")
print(names(coef(best_models[[best_model_index]])[-1])) # Exclude intercept



