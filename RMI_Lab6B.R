library(tidyverse)
library(ggplot2)
library(pls)
library(caret)

# Read in cereal data
cereal <- read.csv("C:/Users/achar/Downloads/Cereals.csv")

# Cleaning Data and picking our predictors
X <- cereal[, c("calories", "protein", "fat", "sodium", "fiber", "carbo", 
                "sugars", "potass", "vitamins", "shelf", "weight", "cups")]
y <- cereal$rating

# Remove rows with any missing values
full_data <- complete.cases(X, y)
X <- X[full_data,]
y <- y[full_data]

# Create data frame for modeling
model_data <- data.frame(X, rating = y)

# Set seed for reproducibility
set.seed(320)

# Create train/test split 70-30
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
train_data <- model_data[train_index,]
test_data <- model_data[-train_index,]

# PCR (Principal Components Regression)
# Fit PCR model with CV 
pcr_model <- pcr(rating ~ ., data = train_data, scale = TRUE,
                 validation = "CV", segments = 10)

# Plot PCR results
par(mfrow = c(1,2))
# RMSEP plot
plot(MSEP(pcr_model), main = "PCR - MSEP vs. Components")
# Explained variance plot
plot(explvar(pcr_model),
     type = "l",
     main = "PCR - Explained Variance",
     xlab = "Number of Components",
     ylab = "% Variance Explained")

# Find optimal number of components for PLS
msep_values_pcr <- MSEP(pcr_model)$val[1, , ]

# Find point where rate of MSEP reduction significantly decreases
pcr_ncomp <- which(diff(diff(msep_values_pcr)) > -0.1)[1]
cat("Optimal number of components for PCR: ", pcr_ncomp, "\n")

# Partial Least Squares Regression (PLS)
# Fit PLS model with cross-validation
pls_model <- plsr(rating ~ ., data = train_data,
                  scale = TRUE,
                  validation = "CV", segments = 10)

# Plot PLS results
par(mfrow = c(1, 2))
# RMSEP plot
plot(MSEP(pls_model), main = "PLS - MSEP vs. Components")
# Explained variance plot
plot(explvar(pls_model),
     type = "l",
     main = "PLS - Explained Variance",
     xlab = "Number of Components",
     ylab = "% Variance Explained")

# Find optimal number of components for PLS
msep_values_pls <- MSEP(pls_model)$val[1, , ]

# Find point where rate of MSEP reduction significantly decreases
pls_ncomp <- which(diff(diff(msep_values_pls)) > -0.1)[1]
cat("Optimal number of components for PLS:", pls_ncomp, "\n")


# Make predictions on test set
# PCR predictions
pcr_pred <- predict(pcr_model, newdata = test_data, ncomp = pcr_ncomp)
# PLS predictions
pls_pred <- predict(pls_model, newdata = test_data, ncomp = pls_ncomp)

# Calculate performance metrics
# Function to calculate metrics
calc_metrics <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  r2 <- cor(actual, predicted)^2
  return(c(MSE = mse, RMSE = rmse, R2 = r2))
}

# Calculate metrics for both models
pcr_metrics <- calc_metrics(test_data$rating, pcr_pred)
pls_metrics <- calc_metrics(test_data$rating, pls_pred)

# Print results
cat("\nPCR Performance Metrics:\n")
print(pcr_metrics)
cat("\nPLS Performance Metrics:\n")
print(pls_metrics)

# Variable Importance for PLS
pls_varimp <- as.data.frame(varImp(pls_model, comps = pls_ncomp))
colnames(pls_varimp) <- "PLS Importance"
pls_varimp <- pls_varimp[order(-abs(pls_varimp$`PLS Importance`)), , drop = FALSE]

# Variable Importance for PCR
# Extract the loadings for the selected number of components
pcr_loadings <- as.data.frame(pcr_model$loadings[, 1:pcr_ncomp])
pcr_importance <- rowSums(abs(pcr_loadings))  # Sum of absolute loadings across components
pcr_importance <- as.data.frame(pcr_importance)
colnames(pcr_importance) <- "PCR Importance"
pcr_importance <- pcr_importance[order(-pcr_importance$`PCR Importance`), , drop = FALSE]

# Print the variable importance
cat("\nVariable Importance in PLS:\n")
print(pls_varimp)
cat("\nVariable Importance in PCR:\n")
print(pcr_importance)

# Create comparison plots of actual vs predicted values
par(mfrow = c(1, 2))
# PCR
plot(test_data$rating, pcr_pred,
     main = "PCR: Actual vs Predicted",
     xlab = "Actual Rating",
     ylab = "Predicted Rating")
abline(0, 1, col = "red")

# PLS
plot(test_data$rating, pls_pred,
     main = "PLS: Actual vs Predicted",
     xlab = "Actual Rating",
     ylab = "Predicted Rating")
abline(0, 1, col = "red")


# Additional diagnostic plots
# Plot explained variance for both methods together
par(mfrow = c(1, 1))
plot(1:pcr_model$ncomp, cumsum(explvar(pcr_model)), 
     type = "b", col = "blue",
     xlab = "Number of Components",
     ylab = "Cumulative % Variance Explained",
     main = "Cumulative Variance Explained")
lines(1:pls_model$ncomp, cumsum(explvar(pls_model)), 
      type = "b", col = "red")
legend("bottomright", 
       legend = c("PCR", "PLS"),
       col = c("blue", "red"),
       lty = 1)

# Print summary of both models
cat("\nPCR Model Summary:\n")
summary(pcr_model)
cat("\nPLS Model Summary:\n")
summary(pls_model)












