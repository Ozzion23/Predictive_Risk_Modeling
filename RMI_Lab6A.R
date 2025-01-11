
# Load the required libraries
library(ISLR)
library(leaps)         # For subset selection
library(glmnet)        # For ridge and lasso regression
library(MASS)          # For cross-validation

# Load the College dataset
data("College")

# Add the target variable y = log(Enroll / Accept)
College$y <- log(College$Enroll / College$Accept)

# Split the dataset into training and testing sets (e.g., 70% train, 30% test)
set.seed(549)
train_indices <- sample(1:nrow(College), 0.7 * nrow(College))
train_data <- College[train_indices, ]
test_data <- College[-train_indices, ]

# Perform subset selection
subset_model <- regsubsets(y ~ . - Accept - Enroll, data = train_data, nvmax = 16)
subset_summary <- summary(subset_model)

# Identify the best model using BIC or other metrics
best_model_size <- which.min(subset_summary$bic)
best_model <- coef(subset_model, best_model_size)
best_model

plot(subset_model, scale = "bic")

# Predict on the test dataset using the selected model's predictors
test_data_subset <- model.matrix(y ~ . - Accept - Enroll, data = test_data)[, names(best_model), drop = FALSE]
test_predictions_subset <- test_data_subset %*% best_model

# Calculate the test error (Mean Squared Error)
test_error_subset <- mean((test_data$y - test_predictions_subset)^2)
test_error_subset

# Prepare model matrices for ridge & lasso regression
x_train <- model.matrix(y ~ . - Accept - Enroll, train_data)[, -1]
y_train <- train_data$y
x_test <- model.matrix(y ~ . - Accept - Enroll, test_data)[, -1]

# Perform ridge regression with cross-validation
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)
best_lambda_ridge <- ridge_model$lambda.min
best_lambda_ridge

# Show coefficient for the predictors
# Get coefficients based on the best lambda value
ridge_coef <- coef(ridge_model, s = best_lambda_ridge)
ridge_coef_df <- as.matrix(ridge_coef)
ridge_coef_df

# Predict on the test dataset
ridge_predictions <- predict(ridge_model, s = best_lambda_ridge, newx = x_test)

# Calculate the test error (Mean Squared Error)
test_error_ridge <- mean((test_data$y - ridge_predictions)^2)
test_error_ridge

# Perform lasso regression with cross-validation
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda_lasso <- lasso_model$lambda.min
best_lambda_lasso

# Get coefficients based on the best lambda value
lasso_coef <- coef(lasso_model, s = best_lambda_lasso)
lasso_coef_df <- as.matrix(lasso_coef)
lasso_coef_df

# Predict on the test dataset
lasso_predictions <- predict(lasso_model, s = best_lambda_lasso, newx = x_test)

# Calculate the test error (Mean Squared Error)
test_error_lasso <- mean((test_data$y - lasso_predictions)^2)
test_error_lasso

# Compare results of each method
cat("Test Error (Subset Method): ", test_error_subset, "\n")
cat("Test Error (Ridge Regression): ", test_error_ridge, "\n")
cat("Test Error (Lasso Regression): ", test_error_lasso, "\n")

