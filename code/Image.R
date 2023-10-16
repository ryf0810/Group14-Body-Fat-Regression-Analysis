rm(list = ls())
####box plot####
data <- read.csv(file="BodyFat.csv")
data <- data[2:17]
subset_data <- data[, c(1, 3:5)]

boxplot(data, outline=TRUE,main="Box plot of Raw Data with Outliers")
#####fitted Image####
plot(y, y_hat_lasso, main = "Fitted Line Plot" ,xlab = "y", ylab = expression(hat(y)), col = "black")
abline(lm(y_hat_lasso ~ y), col = "red") 

plot(y, y_hat_ridge, main = "Fitted Line Plot" ,xlab = "y", ylab = expression(hat(y)), col = "black")
abline(lm(y_hat_ridge ~ y), col = "red") 
####Residual Image######
residuals_lasso <- y - y_hat_lasso
plot(y_hat_lasso, residuals_lasso, main = "Residual Plot", xlab = "lasso_predict", ylab = "Residuals")
abline(h = 0, col = "red")

residuals_ridge <- y - y_hat_ridge
plot(y_hat_ridge, residuals_ridge, main = "Residual Plot", xlab = "ridge_predict", ylab = "Residuals")
abline(h = 0, col = "red")

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
