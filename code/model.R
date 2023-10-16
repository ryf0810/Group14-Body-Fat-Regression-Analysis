rm(list = ls())
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
set.seeds(1)
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

#Calculate R^2
rsquared_lasso <- 1 - (sum((y - y_hat_lasso)^2) / sum((y - mean(y))^2))
rsquared_ridge <- 1 - (sum((y - y_hat_ridge)^2) / sum((y - mean(y))^2))
print(rsquared_lasso)
print(rsquared_ridge)

#####95%  prediction interval####
se <- sqrt(model.cv1$cvm[model.cv1$lambda == best_lambda1] / nrow(data1))
alpha <- 0.05
t_quantile <- qt(1 - alpha / 2, df = nrow(data1) - 4 -1)
lower_limit <- y_hat_lasso - t_quantile * se
upper_limit <- y_hat_lasso + t_quantile * se
lower_limit
upper_limit

#mulitple linear
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

ml <- lm(BODYFAT ~ WEIGHT + HEIGHT + ANKLE + FOREARM, data = data)
summary(ml) # Four predictors are statistically significant, and F-statistics is

#Statistic Tests

#calculate R^2
rsquared_lasso <- 1 - (sum((y - y_hat_lasso)^2) / sum((y - mean(y))^2))
rsquared_ridge <- 1 - (sum((y - y_hat_ridge)^2) / sum((y - mean(y))^2))
print(rsquared_lasso)
print(rsquared_ridge)

summary(ml)$r.squared

#calculate MSE

ls <- summary(vif_model)
mse_lm <- mean(ls$residuals^2)
mse_lasso <- mean((y - y_hat_lasso)^2)
mse_ridge <- mean((y - y_hat_ridge)^2)

print(mse_lm)
print(mse_lasso)
print(mse_ridge)

residuals <- ml$residuals
MSE_ml <- mean(residuals^2)
MSE_ml

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

#AIC for ml
AIC(ml)

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

# BIC for ml
BIC(ml)


# Compare graph 
categories <- c("MSE", "AIC", "BIC")
MSE <- c(log(mse_lasso), log(mse_ridge), log(MSE_ml))
AIC <- c(log(pseudo_AIC_lasso), log(pseudo_AIC_ridge), log(AIC(ml)))
BIC <- c(log(bic_lasso), log(bic_ridge), log(BIC(ml)))
model_kind <- c("lasso", "ridge", "ml")

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
