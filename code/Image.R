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
