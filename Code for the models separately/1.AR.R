library(forecast)
library(Metrics)

# 创建时间序列对象
inflation_ts <- ts(final_dataset$Inflation_Rate, start = 2, end = 731, frequency = 12)

# 定义预测函数
forecast_ar <- function(train_ts, test_ts, horizon) {
  # 训练AR(1)模型
  ar1_model <- arima(train_ts, order = c(1, 0, 0))
  
  # 进行预测
  forecast_obj <- forecast(ar1_model, h = horizon)
  
  # 计算指标
  test_pred <- forecast_obj$mean
  test_mse <- mean((test_ts[1:horizon] - test_pred)^2)
  test_rmse <- sqrt(test_mse)
  test_mae <- mae(test_ts[1:horizon], test_pred)
  test_mape <- mape(test_ts[1:horizon], test_pred)
  
  return(list(preds = test_pred, mse = test_mse, rmse = test_rmse, mae = test_mae, mape = test_mape))
}

# 划分训练集和测试集
test_size <- length(inflation_ts) - 730
train_ts <- window(inflation_ts, end = 730)
test_ts <- window(inflation_ts, start = 731)

# 1个月预测
train_size <- 730
forecast_1 <- forecast_ar(window(train_ts, end = train_size), test_ts, 1)
cat("1-Month Forecast:\n")
print(forecast_1)

# 3个月预测
train_size <- 728
train_ts <- window(inflation_ts, end = 728)
test_ts <- window(inflation_ts, start = 729)
forecast_3 <- forecast_ar(window(train_ts, end = train_size), test_ts, 3)
cat("\n3-Month Forecast:\n")
print(forecast_3)

# 12个月预测
train_size <- 719
train_ts <- window(inflation_ts, end = 719)
test_ts <- window(inflation_ts, start = 720)
forecast_12 <-  forecast_ar(window(train_ts, end = train_size), test_ts, 12)
cat("\n12-Month Forecast:\n")
print(forecast_12)

# 使用完整训练集训练模型
full_train_ts <- window(inflation_ts, end = 731)

# 加载测试集数据
test_data <- final_test_dataset
test_inflation_ts <- ts(test_data$Inflation_Rate, start = 733, end = 782, frequency = 12)

# 使用完整训练集训练模型
full_train_ts <- window(inflation_ts, end = 731)

# 加载测试集数据
test_data <- final_test_dataset
test_inflation_ts <- ts(test_data$Inflation_Rate, start = 733, end = 782, frequency = 12)

custom_forecast_ar <- function(train_ts, test_ts, horizon) {
  # 训练AR(1)模型
  ar1_model <- arima(train_ts, order = c(1, 0, 0))
  
  # 进行预测
  forecast_vals <- predict(ar1_model, n.ahead = horizon)$pred
  
  # 计算指标
  test_pred <- forecast_vals
  test_mse <- mean((test_ts - test_pred)^2, na.rm = TRUE)
  test_rmse <- sqrt(test_mse)
  test_mae <- mean(abs(test_ts - test_pred), na.rm = TRUE)
  test_mape <- mean(abs((test_ts - test_pred) / test_ts), na.rm = TRUE) * 100
  
  return(list(preds = test_pred, mse = test_mse, rmse = test_rmse, mae = test_mae, mape = test_mape))
}

# 使用完整训练集进行预测
full_forecast_1 <- custom_forecast_ar(full_train_ts, test_inflation_ts[1], 1)
cat("\nFull Dataset 1-Month Forecast:\n")
print(full_forecast_1)

full_forecast_3 <- custom_forecast_ar(full_train_ts, test_inflation_ts[1:3], 3)
cat("\nFull Dataset 3-Month Forecast:\n")
print(full_forecast_3)

full_forecast_12 <- custom_forecast_ar(full_train_ts, test_inflation_ts[1:12], 12)
cat("\nFull Dataset 12-Month Forecast:\n")
print(full_forecast_12)

full_forecast_49<- custom_forecast_ar(full_train_ts, test_inflation_ts[1:49], 49)
cat("\nFull Dataset 49-Month Forecast:\n")
print(full_forecast_49)