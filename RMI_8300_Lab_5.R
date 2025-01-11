library(tidyverse)
library(ggplot2)
library(dplyr)
library(boot)
library(glmnet)

# Read in Accidents Data
advertising <- read.csv("C:/Users/achar/Downloads/Advertising.csv")

# Fit the multiple linear regression model
model1 <- glm(sales ~ TV + radio + newspaper, data = advertising )

# LOOKCV
loocv_result_ad <- cv.glm(advertising, model1)
loocv_mse_ad <- loocv_result_ad$delta[2]

loocv_mse_ad

# k-fold CV (k = 6)
set.seed(784)
kfold_result_ad <- cv.glm(advertising, model1, K = 6)
kfold_mse_ad <- kfold_result_ad$delta[2]

kfold_mse_ad

# Bootstrap for coefficients
set.seed(784)
boot_func_ad <- function(data, index) {
  model1 <- glm(sales ~ TV + radio + newspaper, data = data, subset = index)
  return(coef(model1))
}

boot_result_ad <- boot(advertising, boot_func_ad, R = 1000)

# Compare coefficients
coef_comparison_ad <- data.frame(
  glm = coef(model1),
  boot_mean = apply(boot_result_ad$t, 2, mean),
  boot_se = apply(boot_result_ad$t, 2, sd)
)

coef_comparison_ad

# Print results
print("LOOCV MSE:")
print(loocv_mse_ad)
print("6-fold CV MSE:")
print(kfold_mse_ad)
print("Coefficient Comparison:")
print(coef_comparison_ad)


# Read in Universal Bank data
universal_bank <- read.csv("C:/Users/achar/Downloads/UniversalBank.csv")

# Fit the model
model2 <- glm(Personal.Loan ~ Age + Experience + Income + Family + CCAvg
              + Education + Mortgage + Securities.Account + CD.Account +
                Online + CreditCard, data = universal_bank, family = binomial)

# LOOCV 
loocv_result_bank <- cv.glm(universal_bank, model2)
loocv_error_bank <- loocv_result_bank$delta[2]

loocv_error_bank

# k-fold CV (k = 6)
set.seed(219)
kfold_result_bank <- cv.glm(universal_bank, model2, K = 6)
kfold_error_bank <- kfold_result_bank$delta[2]

kfold_error_bank

# Bootstrap for coefficients
set.seed(219)
boot_func_bank <- function(data, index){
  model2 <- glm(Personal.Loan ~ Age + Experience + Income + Family + CCAvg
                + Education + Mortgage + Securities.Account + CD.Account +
                  Online + CreditCard, data = data, subset = index,
                family = binomial)
  return(coef(model2))
}

boot_result_bank <- boot(universal_bank, boot_func_bank, R = 1000)

# Compare coefficients
coef_comparison_bank <- data.frame(
  glm = coef(model2),
  boot_mean = apply(boot_result_bank$t, 2, mean),
  boot_se = apply(boot_result_bank$t, 2, sd)
)

coef_comparison_bank

# Print Results
print("LOOCV Error:")
print(loocv_error_bank)
print("6-fold CV Error:")
print(kfold_error_bank)
print("Coefficient Comparison:")
print(coef_comparison_bank)







