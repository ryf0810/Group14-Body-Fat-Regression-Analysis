rm(list = ls())
data <- read.csv(file.choose="BodyFat.csv")
data <- data[2:17]
#####data cleaning####
outlier_detect <-function(x) {
  # input x is a feature vector, the function returns the index of outlier in the specifi feature
  return(which(x < quantile(x, 0.1) - 1.5 * IQR(x) | x > quantile(x, 0.9) + 1.5 * IQR(x)))
}

subset_data <- data[, 1:5]
outlier_indices <- lapply(subset_data, outlier_detect)
for (i in 1:5) {
  cat("Outliers in Column", i, ":", outlier_indices[[i]], "\n")
}

data <- data[-182,]

outlier_indices_list <- list()
for (i in 7:ncol(data)) {
  outlier_indices_list[[i-1]] <- outlier_detect(data[, i])
}
for (i in 1:length(outlier_indices_list)) {
  cat("Feature", names(data)[i+1], "Outliers:", length(outlier_indices_list[[i]]), "Indices:", outlier_indices_list[[i]], "\n")
}

data <- data[-39,]
data <- data[-41,]
data <- data[-31,]
data <- data[-86,]
data <- data[-175,]
data <- data[-226,]
data <- data[-182,]
write.csv(data,file = "data2.csv",row.names = F)


