# Feature Part
rm(list = ls())

data <- read.csv(file = "~/Downloads/BodyFat.csv")
data <- data[2:17]
subset_data <- data[, c(1, 3:5)]

boxplot(data, outline=TRUE,main="Box plot of Raw Data with Outliers")


# Model part
library(glmnet)
library(car)
library(tidyverse)
library(ggplot2)
library(MASS)
library(corrplot)
library(faraway)
library(caret)

data <- read.csv(file = "~/Downloads/BodyFat_cleaned.csv")
#check multilinearity
y <- data[,1]
vif_model <- lm(y ~ data$DENSITY+data$AGE+data$WEIGHT+data$HEIGHT+data$ADIPOSITY+data$NECK+data$CHEST+data$ABDOMEN+data$HIP+data$THIGH+data$KNEE+data$ANKLE+data$BICEPS+data$FOREARM+data$WRIST, data = data)
vif_values <- vif(vif_model)
print(vif_values)

y <- data[,1]
x <- data[,3:16]
x <- scale(x)
x <- as.matrix(x)
#lasso regression
model.cv1 <- cv.glmnet(x,y,family="gaussian", alpha=1)
best_lambda1 <- model.cv1$lambda.1se
fit1 <- glmnet(x,y,alpha = 1,lambda = best_lambda1)
coef_lasso <- coef(fit1,s=best_lambda1)
y_hat_lasso <- predict(fit1,newx = x,s = best_lambda1)
plot(model.cv1)
#ridge regression
model.cv2 <- cv.glmnet(x,y,family="gaussian", alpha=0)
best_lambda2 <- model.cv2$lambda.1se
fit2 <- glmnet(x,y,alpha = 0,lambda = best_lambda2)
coef_ridge <- coef(fit2,s = best_lambda2)
y_hat_ridge <- predict(fit2,newx = x,s = best_lambda2)
plot(model.cv2)
#model evaluation
plot(y, y_hat_lasso, main = "Fitted Line Plot of y vs.y_lasso", xlab = "y", ylab = "y_hat", col = "black")
abline(lm(y_hat_lasso ~ y), col = "red") 
plot(y, y_hat_ridge, main = "Fitted Line Plot of y vs.y_ridge", xlab = "y", ylab = "y_hat", col = "black")
abline(lm(y_hat_ridge ~ y), col = "red")

residuals_lasso <- y - y_hat_lasso
plot(y_hat_lasso, residuals_lasso, main = "Residual Plot", xlab = "lasso_predict", ylab = "Residuals")
abline(h = 0, col = "red")
residuals_ridge <- y - y_hat_ridge
plot(y_hat_ridge, residuals_ridge, main = "Residual Plot", xlab = "ridge_predict", ylab = "Residuals")
abline(h = 0, col = "red")

rsquared_lasso <- 1 - (sum((y - y_hat_lasso)^2) / sum((y - mean(y))^2))
rsquared_ridge <- 1 - (sum((y - y_hat_ridge)^2) / sum((y - mean(y))^2))
print(rsquared_lasso)
print(rsquared_ridge)


# OLS
# Choose features based on VIF, < 5
vif.val <- faraway::vif(data[2:dim(data)[2]])
df.vif <- data.frame(Features = names(vif.val), VIF = as.vector(vif.val))
df.vif



ggplot(data = df.vif) +
  geom_col(aes(x = Features, y = VIF)) +
  ggtitle('Variance Inflation Factors') +
  geom_hline(aes(yintercept = 5, col = 'red')) +
  labs(col = 'Cutoff')


df.vif[which(df.vif$VIF < 3), ]

# for selected features: age, height, ankle, forearm
boxplot(data[,c('AGE', 'HEIGHT', 'ANKLE', 'FOREARM')])

OLS <- lm(BODYFAT ~ WEIGHT + HEIGHT + ANKLE + FOREARM, data = data)
summary(OLS) # Four predictors are statistically significant, and F-statistics is

#Statistic Tests

#calculate R^2
rsquared_lasso <- 1 - (sum((y - y_hat_lasso)^2) / sum((y - mean(y))^2))
rsquared_ridge <- 1 - (sum((y - y_hat_ridge)^2) / sum((y - mean(y))^2))
print(rsquared_lasso)
print(rsquared_ridge)

summary(OLS)$r.squared

#calculate MSE

ls <- summary(vif_model)
mse_lm <- mean(ls$residuals^2)
mse_lasso <- mean((y - y_hat_lasso)^2)
mse_ridge <- mean((y - y_hat_ridge)^2)

print(mse_lm)
print(mse_lasso)
print(mse_ridge)

residuals <- OLS$residuals
MSE_OLS <- mean(residuals^2)
MSE_OLS

# calculate AIC
#For original vif_model
AIC_vif_model <- AIC(vif_model)
print(AIC_vif_model)

# For Lasso
RSS_lasso <- sum((y - y_hat_lasso)^2)
k_lasso <- sum(coef_lasso != 0)
pseudo_AIC_lasso <- length(y) * log(RSS_lasso/length(y)) + 2*k_lasso
print(pseudo_AIC_lasso)

# For Ridge
RSS_ridge <- sum((y - y_hat_ridge)^2)
k_ridge <- sum(coef_ridge != 0)
pseudo_AIC_ridge <- length(y) * log(RSS_ridge/length(y)) + 2*k_ridge
print(pseudo_AIC_ridge)

#AIC for OLS
AIC(OLS)

#calculate BIC for vif_model
bic_vif_model <- BIC(vif_model)
print(bic_vif_model)

# Calculate BIC for Lasso
rss_lasso <- sum((y - y_hat_lasso)^2)
n <- length(y)
k_lasso <- sum(coef_lasso != 0) # number of non-zero coefficients
bic_lasso <- n * log(rss_lasso/n) + k_lasso * log(n)
print(bic_lasso)

# Calculate BIC for Ridge
rss_ridge <- sum((y - y_hat_ridge)^2)
k_ridge <- length(coef_ridge) # Since Ridge doesn't shrink coefficients to zero
bic_ridge <- n * log(rss_ridge/n) + k_ridge * log(n)
print(bic_ridge)

# BIC for OLS
BIC(OLS)


# Compare graph 
categories <- c("MSE", "AIC", "BIC")
MSE <- c(log(mse_lasso), log(mse_ridge), log(MSE_OLS))
AIC <- c(log(pseudo_AIC_lasso), log(pseudo_AIC_ridge), log(AIC(OLS)))
BIC <- c(log(bic_lasso), log(bic_ridge), log(BIC(OLS)))
model_kind <- c("Lasso", "Ridge", "OLS")

df <- data.frame(categories, MSE, AIC, BIC)


bar_heights <- as.matrix(df[, 2:4])
bar_names <- df$categories
bar_colors <- c("red", "blue", "green")  # Customize bar colors if needed

# Create the barplot
barplot(
  bar_heights,
  beside = TRUE,        # Stack bars beside each other
  names.arg = bar_names,
  col = bar_colors,
  xlab = "Categories",
  ylab = "Values",
  main = "MSE, AIC, and BIC of Three Model(log(MSE/AIC/BIC))"
)

# Add a legend
legend("topleft", legend = model_kind, fill = bar_colors)

