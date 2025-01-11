library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(MASS)
library(class)

# Read in Data
accidents <- read.csv("C:/Users/achar/Downloads/Accidents.csv")

View(accidents)

# Convert MAX_SEV variable into a factor variable
accidents$MAX_SEV <- as.factor(accidents$MAX_SEV)

# Split dataset into training and testing data
set.seed(42)

trainIndex <- createDataPartition(accidents$MAX_SEV, p = 0.7, list = FALSE)

train_data <- accidents[trainIndex, ]
test_data <- accidents[-trainIndex, ]

# Perform LDA technique
lda_model <- lda(MAX_SEV ~ ., data = train_data)

# Make predictions using LDA 
lda_pred <- predict(lda_model, test_data)
lda_class <- lda_pred$class

# Evaluate LDA performance
lda_CM <- confusionMatrix(lda_class, test_data$MAX_SEV)
print("LDA Confusion Matrix")
print(lda_CM)

# Perform KNN technique
# First Normalize the data
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

accidents_normalized <- as.data.frame(lapply(accidents[, -ncol(accidents)], 
                                             normalize))

# Split normalized data
train_norm <- accidents_normalized[trainIndex, ]
test_norm <- accidents_normalized[-trainIndex, ]

# Find optimal k for KNN using cross validation
k_values <- 1:20
accuracies <- sapply(k_values, function(k){
  knn_pred <- knn(train_norm, test_norm, train_data$MAX_SEV, k = k)
  mean(knn_pred == test_data$MAX_SEV)
})

optimal_k <- k_values[which.max(accuracies)]
print(paste("Optimal k:", optimal_k))

# Perform KNN with optimal k
knn_pred <- knn(train_norm, test_norm, train_data$MAX_SEV, k = optimal_k)

# Evaluate KNN performance
knn_CM <- confusionMatrix(knn_pred, test_data$MAX_SEV)
print("KNN Confusion Matrix")
print(knn_CM)

# Compare LDA and KNN
print(paste("LDA Accuracy: ", round(lda_CM$overall["Accuracy"], 4)))
print(paste("KNN Accuracy: ", round(knn_CM$overall["Accuracy"], 4)))

