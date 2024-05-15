library(glmnet)
library(Metrics)

# 加载数据
data <- final_dataset
rownames(data) <- data$Month
data$Month <- NULL
raw_features <- data[, 1:202]
log_diff_features <- data[, 203:405]
target <- data$Inflation_Rate

# 定义预测函数
forecast_lasso <- function(train_x, train_y, test_x, test_y, horizon) {
  # 训练LASSO模型
  lasso_model <- train_lasso(train_x, train_y)
  
  # 进行预测
  predictions <- predict(lasso_model, newx = test_x)
  
  # 计算指标
  actuals <- test_y[1:min(horizon, length(test_y))]
  mse <- mean((predictions[1:length(actuals)] - actuals)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(predictions[1:length(actuals)] - actuals))
  mape <- mape(actuals, predictions[1:length(actuals)])
  
  return(list(preds = predictions[1:length(actuals)], mse = mse, rmse = rmse, mae = mae, mape = mape))
}

# 训练LASSO模型并选择最优lambda
train_lasso <- function(x_train, y_train) {
  cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)
  best_lambda <- cv_lasso$lambda.min
  lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)
  return(lasso_model)
}

# 划分训练集和测试集
test_size <- length(target) - 730
train_x_raw <- as.matrix(raw_features[1:730, ])
train_y_raw <- as.matrix(target[1:730])
test_x_raw <- as.matrix(raw_features[731:length(target), ])
test_y_raw <- as.matrix(target[731:length(target)])

train_x_log_diff <- as.matrix(log_diff_features[1:730, ])
train_y_log_diff <- as.matrix(target[1:730])
test_x_log_diff <- as.matrix(log_diff_features[731:length(target), ])
test_y_log_diff <- as.matrix(target[731:length(target)])

# 1个月预测
train_size <- 730
forecast_1_raw <- forecast_lasso(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 1)
forecast_1_log_diff <- forecast_lasso(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 1)
cat("1-Month Forecast:\n")
print(forecast_1_raw)
cat("\n")
print(forecast_1_log_diff)

# 3个月预测
train_size <- 728
train_x_raw <- as.matrix(raw_features[1:train_size, ])
train_y_raw <- as.matrix(target[1:train_size])
test_x_raw <- as.matrix(raw_features[(train_size+1):length(target), ])
test_y_raw <- as.matrix(target[(train_size+1):length(target)])

train_x_log_diff <- as.matrix(log_diff_features[1:train_size, ])
train_y_log_diff <- as.matrix(target[1:train_size])
test_x_log_diff <- as.matrix(log_diff_features[(train_size+1):length(target), ])
test_y_log_diff <- as.matrix(target[(train_size+1):length(target)])

forecast_3_raw <- forecast_lasso(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 3)
forecast_3_log_diff <- forecast_lasso(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 3)
cat("\n3-Month Forecast:\n")
print(forecast_3_raw)
cat("\n")
print(forecast_3_log_diff)

# 12个月预测
train_size <- 719
train_x_raw <- as.matrix(raw_features[1:train_size, ])
train_y_raw <- as.matrix(target[1:train_size])
test_x_raw <- as.matrix(raw_features[(train_size+1):length(target), ])
test_y_raw <- as.matrix(target[(train_size+1):length(target)])

train_x_log_diff <- as.matrix(log_diff_features[1:train_size, ])
train_y_log_diff <- as.matrix(target[1:train_size])
test_x_log_diff <- as.matrix(log_diff_features[(train_size+1):length(target), ])
test_y_log_diff <- as.matrix(target[(train_size+1):length(target)])

forecast_12_raw <- forecast_lasso(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 12)
forecast_12_log_diff <- forecast_lasso(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 12)
cat("\n12-Month Forecast:\n")
print(forecast_12_raw)
cat("\n")
print(forecast_12_log_diff)

# 使用完整训练集训练模型
full_train_x_raw <- as.matrix(raw_features)
full_train_y_raw <- as.matrix(target)
full_train_x_log_diff <- as.matrix(log_diff_features)
full_train_y_log_diff <- as.matrix(target)

# 加载测试集数据
test_data <- final_test_dataset
test_x_raw <- as.matrix(test_data[, 1:202])
test_y_raw <- as.matrix(test_data$Inflation_Rate)
test_x_log_diff <- as.matrix(test_data[, 203:405])
test_y_log_diff <- as.matrix(test_data$Inflation_Rate)

# 使用完整训练集进行预测
full_forecast_1_raw <- forecast_lasso(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 1)
full_forecast_1_log_diff <- forecast_lasso(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 1)

cat("\nFull Dataset 1-Month Forecast:\n")
print(full_forecast_1_raw)
cat("\n")
print(full_forecast_1_log_diff)

full_forecast_3_raw <- forecast_lasso(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 3)
full_forecast_3_log_diff <- forecast_lasso(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 3)

cat("\nFull Dataset 3-Month Forecast:\n")
print(full_forecast_3_raw)
cat("\n")
print(full_forecast_3_log_diff)

full_forecast_12_raw <- forecast_lasso(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 12)
full_forecast_12_log_diff <- forecast_lasso(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 12)

cat("\nFull Dataset 12-Month Forecast:\n")
print(full_forecast_12_raw)
cat("\n")
print(full_forecast_12_log_diff)

full_forecast_49_raw <- forecast_lasso(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 49)
full_forecast_49_log_diff <- forecast_lasso(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 49)

cat("\nFull Dataset 49-Month Forecast:\n")
print(full_forecast_49_raw)
cat("\n")
print(full_forecast_49_log_diff)