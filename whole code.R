library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
setwd("C:\\Users\\ASUS\\Desktop\\data")
train_data <- read.xlsx("US_PCE_training.xlsx")

months <- as.character(1:732)
colnames(train_data) <- c("Variable", as.character(1:732))

# 转换数据为长格式
data_long <- pivot_longer(train_data, cols = -Variable, names_to = "Month", values_to = "Value")
data_wide <- pivot_wider(data_long, names_from = Variable, values_from = Value)

data_wide <- data_wide %>%
  mutate(Year = (as.numeric(Month) - 1) %/% 12 + 1,   # 计算年份
         MonthOfYear = (as.numeric(Month) - 1) %% 12 + 1,   # 计算月份
         NewMonth = paste(Year, MonthOfYear, sep = "-")) %>%
  select(-Month, -Year, -MonthOfYear) %>%
  rename(Month = NewMonth)

print(head(data_wide))

data_wide <- data_wide %>%
  mutate(`Personal consumption expenditures` = as.numeric(`Personal consumption expenditures`))

data_wide <- data_wide %>%
  mutate(
    `Personal consumption expenditures` = ifelse(is.na(`Personal consumption expenditures`), 0, `Personal consumption expenditures`),
    PCE_Lag = lag(`Personal consumption expenditures`),  
    Inflation_Rate = ifelse(is.na(PCE_Lag) | PCE_Lag == 0, NA, 
                            (log(`Personal consumption expenditures`) - log(PCE_Lag)) * 12)  # Calculating the inflation rate
  ) %>%
  select(-PCE_Lag)

print(head(data_wide))

# 选择预测变量的列范围
predictor_columns <- 3:(ncol(data_wide) - 2)  
inflation_rate_column <- ncol(data_wide)  # Inflation Rate是最后一列

dataset_raw <- data_wide %>%
  select(all_of(c(predictor_columns)))

# 检查每一列的数据类型
data_types <- sapply(data_wide, class)
list_columns <- names(data_types[data_types == "list"])

# 尝试将列表列中的元素转换为数值
data_wide <- data_wide %>%
  mutate(across(all_of(list_columns), ~sapply(.x, function(item) as.numeric(item[1]))))

dataset_raw <- data_wide %>%
  select(all_of(predictor_columns)) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(everything(), ~ifelse(.x <= 0, NA, .x))) %>%
  na.omit()  

# 对dataset_raw进行对数差分转换
dataset_transformed <- dataset_raw %>%
  mutate(across(everything(), ~log(.x) - log(lag(.x)), .names = "log_diff_{col}")) %>%
  na.omit()  

# 创建新的月份列，从2开始
month_column <- data.frame(Month = 2:(nrow(dataset_transformed) + 1))

# 将Inflation_Rate和月份列合并到处理后的数据集
final_dataset <- dataset_transformed %>%
  mutate(Inflation_Rate = data_wide$`Inflation_Rate`[-1]) %>%
  bind_cols(month_column)  # 绑定月份列

print(head(final_dataset))#可以考虑用没有进行对数差分的结果作为predictor，也可以考虑用进行过的
write.csv(final_dataset, "final_dataset.csv", row.names = FALSE)


#########################################################
test_data_true <- read.xlsx("US_PCE_testing_fake.xlsx")
months <- as.character(733:782)
colnames(test_data_true) <- c("Variable", as.character(733:782))
# 转换数据为长格式
testdata_long <- pivot_longer(test_data_true, cols = -Variable, names_to = "Month", values_to = "Value")
testdata_wide <- pivot_wider(testdata_long, names_from = Variable, values_from = Value)

testdata_wide <- testdata_wide %>%
  mutate(Year = (as.numeric(Month) - 1) %/% 12 + 1,   # 计算年份
         MonthOfYear = (as.numeric(Month) - 1) %% 12 + 1,   # 计算月份
         NewMonth = paste(Year, MonthOfYear, sep = "-")) %>%
  select(-Month, -Year, -MonthOfYear) %>%
  rename(Month = NewMonth)

print(head(testdata_wide))

testdata_wide <- testdata_wide %>%
  mutate(`Personal consumption expenditures` = as.numeric(`Personal consumption expenditures`))

testdata_wide <- testdata_wide %>%
  mutate(
    `Personal consumption expenditures` = ifelse(is.na(`Personal consumption expenditures`), 0, `Personal consumption expenditures`),
    PCE_Lag = lag(`Personal consumption expenditures`),  
    Inflation_Rate = ifelse(is.na(PCE_Lag) | PCE_Lag == 0, NA, 
                            (log(`Personal consumption expenditures`) - log(PCE_Lag)) * 12)  # Calculating the inflation rate
  ) %>%
  select(-PCE_Lag)

print(head(testdata_wide))

# 选择预测变量的列范围
predictor_columns <- 3:(ncol(testdata_wide) - 2)  
inflation_rate_column <- ncol(testdata_wide)  # Inflation Rate是最后一列

testdataset_raw <- testdata_wide %>%
  select(all_of(c(predictor_columns)))

# 检查每一列的数据类型
data_types <- sapply(testdata_wide, class)
list_columns <- names(data_types[data_types == "list"])

# 尝试将列表列中的元素转换为数值
testdata_wide <- testdata_wide %>%
  mutate(across(all_of(list_columns), ~sapply(.x, function(item) as.numeric(item[1]))))

testdataset_raw <- testdata_wide %>%
  select(all_of(predictor_columns)) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(everything(), ~ifelse(.x <= 0, NA, .x))) %>%
  na.omit()  

# 对dataset_raw进行对数差分转换
testdataset_transformed <- testdataset_raw %>%
  mutate(across(everything(), ~log(.x) - log(lag(.x)), .names = "log_diff_{col}")) %>%
  na.omit()  

# 创建新的月份列，从2开始
month_column <- data.frame(Month = 734:782)

# 将Inflation_Rate和月份列合并到处理后的数据集
final_test_dataset <- testdataset_transformed %>%
  mutate(Inflation_Rate = testdata_wide$`Inflation_Rate`[-1]) %>%
  bind_cols(month_column)  # 绑定月份列

print(head(final_test_dataset))#可以考虑用没有进行对数差分的结果作为predictor，也可以考虑用进行过的
write.csv(final_test_dataset, "final_test_dataset.csv", row.names = FALSE)


###################AR###############################

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

########################LASSO########################
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


########################RANDOM FOREST########################
library(randomForest)
library(Metrics)

# 加载数据
data <- final_dataset
rownames(data) <- data$Month
data$Month <- NULL
raw_features <- data[, 1:202]
log_diff_features <- data[, 203:405]
target <- data$Inflation_Rate

# 定义预测函数
forecast_rf <- function(train_x, train_y, test_x, test_y, horizon) {
  # 训练随机森林模型
  rf_model <- train_rf(train_x, train_y)
  
  # 进行预测
  predictions <- predict(rf_model, newdata = test_x)
  
  # 计算指标
  actuals <- test_y[1:min(horizon, length(test_y))]
  mse <- mean((predictions[1:length(actuals)] - actuals)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(predictions[1:length(actuals)] - actuals))
  mape <- mape(actuals, predictions[1:length(actuals)])
  
  return(list(preds = predictions[1:length(actuals)], mse = mse, rmse = rmse, mae = mae, mape = mape))
}

# 训练随机森林模型
train_rf <- function(x_train, y_train) {
  rf_model <- randomForest(x_train, y_train, ntree = 500)
  return(rf_model)
}

# 划分训练集和测试集
test_size <- length(target) - 730
train_x_raw <- as.matrix(raw_features[1:730, ])
train_y_raw <- target[1:730]
test_x_raw <- as.matrix(raw_features[731:length(target), ])
test_y_raw <- target[731:length(target)]

train_x_log_diff <- as.matrix(log_diff_features[1:730, ])
train_y_log_diff <- target[1:730]
test_x_log_diff <- as.matrix(log_diff_features[731:length(target), ])
test_y_log_diff <- target[731:length(target)]

# 1个月预测
train_size <- 730
forecast_1_raw <- forecast_rf(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 1)
forecast_1_log_diff <- forecast_rf(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 1)
cat("1-Month Forecast:\n")
print(forecast_1_raw)
cat("\n")
print(forecast_1_log_diff)

# 3个月预测
train_size <- 728
train_x_raw <- as.matrix(raw_features[1:train_size, ])
train_y_raw <- target[1:train_size]
test_x_raw <- as.matrix(raw_features[(train_size+1):length(target), ])
test_y_raw <- target[(train_size+1):length(target)]

train_x_log_diff <- as.matrix(log_diff_features[1:train_size, ])
train_y_log_diff <- target[1:train_size]
test_x_log_diff <- as.matrix(log_diff_features[(train_size+1):length(target), ])
test_y_log_diff <- target[(train_size+1):length(target)]

forecast_3_raw <- forecast_rf(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 3)
forecast_3_log_diff <- forecast_rf(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 3)
cat("\n3-Month Forecast:\n")
print(forecast_3_raw)
cat("\n")
print(forecast_3_log_diff)

# 12个月预测
train_size <- 719
train_x_raw <- as.matrix(raw_features[1:train_size, ])
train_y_raw <- target[1:train_size]
test_x_raw <- as.matrix(raw_features[(train_size+1):length(target), ])
test_y_raw <- target[(train_size+1):length(target)]

train_x_log_diff <- as.matrix(log_diff_features[1:train_size, ])
train_y_log_diff <- target[1:train_size]
test_x_log_diff <- as.matrix(log_diff_features[(train_size+1):length(target), ])
test_y_log_diff <- target[(train_size+1):length(target)]

forecast_12_raw <- forecast_rf(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 12)
forecast_12_log_diff <- forecast_rf(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 12)
cat("\n12-Month Forecast:\n")
print(forecast_12_raw)
cat("\n")
print(forecast_12_log_diff)

# 使用完整训练集训练模型
full_train_x_raw <- as.matrix(raw_features)
full_train_y_raw <- target
full_train_x_log_diff <- as.matrix(log_diff_features)
full_train_y_log_diff <- target

# 加载测试集数据
test_data <- final_test_dataset
test_x_raw <- as.matrix(test_data[, 1:202])
test_y_raw <- test_data$Inflation_Rate
test_x_log_diff <- as.matrix(test_data[, 203:405])
test_y_log_diff <- test_data$Inflation_Rate

# 使用完整训练集进行预测
full_forecast_1_raw <- forecast_rf(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 1)
full_forecast_1_log_diff <- forecast_rf(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 1)

cat("\nFull Dataset 1-Month Forecast:\n")
print(full_forecast_1_raw)
cat("\n")
print(full_forecast_1_log_diff)

full_forecast_3_raw <- forecast_rf(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 3)
full_forecast_3_log_diff <- forecast_rf(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 3)

cat("\nFull Dataset 3-Month Forecast:\n")
print(full_forecast_3_raw)
cat("\n")
print(full_forecast_3_log_diff)

full_forecast_12_raw <- forecast_rf(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 12)
full_forecast_12_log_diff <- forecast_rf(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 12)

cat("\nFull Dataset 12-Month Forecast:\n")
print(full_forecast_12_raw)
cat("\n")
print(full_forecast_12_log_diff)

full_forecast_49_raw <- forecast_rf(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 49)
full_forecast_49_log_diff <- forecast_rf(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 49)

cat("\nFull Dataset 49-Month Forecast:\n")
print(full_forecast_49_raw)
cat("\n")
print(full_forecast_49_log_diff)

#####################Gradient Boosting#################
library(gbm)
library(Metrics)

# 加载数据
data <- final_dataset
rownames(data) <- data$Month
data$Month <- NULL
raw_features <- data[, 1:202]
log_diff_features <- data[, 203:405]
target <- data$Inflation_Rate

# 定义预测函数
forecast_gbm <- function(train_x, train_y, test_x, test_y, horizon) {
  # 训练Gradient Boosting模型
  gbm_model <- train_gbm(train_x, train_y)
  
  # 进行预测
  predictions <- predict(gbm_model, newdata = test_x, n.trees = 100)
  
  # 计算指标
  actuals <- test_y[1:min(horizon, length(test_y))]
  mse <- mean((predictions[1:length(actuals)] - actuals)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(predictions[1:length(actuals)] - actuals))
  mape <- mape(actuals, predictions[1:length(actuals)])
  
  return(list(preds = predictions[1:length(actuals)], mse = mse, rmse = rmse, mae = mae, mape = mape))
}

# 训练Gradient Boosting模型
train_gbm <- function(x_train, y_train, n_trees = 500, shrinkage = 0.1, interaction_depth = 3) {
  gbm_model <- gbm(y_train ~ ., data = x_train,
                   distribution = "gaussian", n.trees = n_trees,
                   shrinkage = shrinkage, interaction.depth = interaction_depth)
  return(gbm_model)
}


# 划分训练集和测试集
test_size <- length(target) - 730
train_x_raw <- raw_features[1:730, ]
train_y_raw <- target[1:730]
test_x_raw <- raw_features[731:length(target), ]
test_y_raw <- target[731:length(target)]

train_x_log_diff <- log_diff_features[1:730, ]
train_y_log_diff <- target[1:730]
test_x_log_diff <- log_diff_features[731:length(target), ]
test_y_log_diff <- target[731:length(target)]

# 1个月预测
train_size <- 730
forecast_1_raw <- forecast_gbm(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 1)
forecast_1_log_diff <- forecast_gbm(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 1)
cat("1-Month Forecast:\n")
print(forecast_1_raw)
cat("\n")
print(forecast_1_log_diff)

# 3个月预测
train_size <- 728
train_x_raw <- raw_features[1:train_size, ]
train_y_raw <- target[1:train_size]
test_x_raw <- raw_features[(train_size+1):length(target), ]
test_y_raw <- target[(train_size+1):length(target)]

train_x_log_diff <- log_diff_features[1:train_size, ]
train_y_log_diff <- target[1:train_size]
test_x_log_diff <- log_diff_features[(train_size+1):length(target), ]
test_y_log_diff <- target[(train_size+1):length(target)]

forecast_3_raw <- forecast_gbm(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 3)
forecast_3_log_diff <- forecast_gbm(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 3)
cat("\n3-Month Forecast:\n")
print(forecast_3_raw)
cat("\n")
print(forecast_3_log_diff)

# 12个月预测
train_size <- 719
train_x_raw <- raw_features[1:train_size, ]
train_y_raw <- target[1:train_size]
test_x_raw <- raw_features[(train_size+1):length(target), ]
test_y_raw <- target[(train_size+1):length(target)]

train_x_log_diff <- log_diff_features[1:train_size, ]
train_y_log_diff <- target[1:train_size]
test_x_log_diff <- log_diff_features[(train_size+1):length(target), ]
test_y_log_diff <- target[(train_size+1):length(target)]

forecast_12_raw <- forecast_gbm(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 12)
forecast_12_log_diff <- forecast_gbm(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 12)
cat("\n12-Month Forecast:\n")
print(forecast_12_raw)
cat("\n")
print(forecast_12_log_diff)

# 使用完整训练集训练模型
full_train_x_raw <- raw_features
full_train_y_raw <- target
full_train_x_log_diff <- log_diff_features
full_train_y_log_diff <- target

# 加载测试集数据
test_data <- final_test_dataset
test_x_raw <- test_data[, 1:202]
test_y_raw <- test_data$Inflation_Rate
test_x_log_diff <- test_data[, 203:405]
test_y_log_diff <- test_data$Inflation_Rate

# 使用完整训练集进行预测
full_forecast_1_raw <- forecast_gbm(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 1)
full_forecast_1_log_diff <- forecast_gbm(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 1)

cat("\nFull Dataset 1-Month Forecast:\n")
print(full_forecast_1_raw)
cat("\n")
print(full_forecast_1_log_diff)

full_forecast_3_raw <- forecast_gbm(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 3)
full_forecast_3_log_diff <- forecast_gbm(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 3)

cat("\nFull Dataset 3-Month Forecast:\n")
print(full_forecast_3_raw)
cat("\n")
print(full_forecast_3_log_diff)

full_forecast_12_raw <- forecast_gbm(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 12)
full_forecast_12_log_diff <- forecast_gbm(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 12)

cat("\nFull Dataset 12-Month Forecast:\n")
print(full_forecast_12_raw)
cat("\n")
print(full_forecast_12_log_diff)

full_forecast_49_raw <- forecast_gbm(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 49)
full_forecast_49_log_diff <- forecast_gbm(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 49)

cat("\nFull Dataset 49-Month Forecast:\n")
print(full_forecast_49_raw)
cat("\n")
print(full_forecast_49_log_diff)

#####################SVM#################
library(e1071)
library(Metrics)
# 加载数据
data <- final_dataset
rownames(data) <- data$Month
data$Month <- NULL
raw_features <- data[, 1:202]
log_diff_features <- data[, 203:405]
target <- data$Inflation_Rate
# 定义预测函数
forecast_svm <- function(train_x, train_y, test_x, test_y, horizon) {
  # 训练SVM模型
  svm_model <- train_svm(train_x, train_y)
  # 进行预测
  predictions <- predict(svm_model, newdata = test_x)
  # 计算指标
  actuals <- test_y[1:min(horizon, length(test_y))]
  mse <- mean((predictions[1:length(actuals)] - actuals)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(predictions[1:length(actuals)] - actuals))
  mape <- mape(actuals, predictions[1:length(actuals)])
  return(list(preds = predictions[1:length(actuals)], mse = mse, rmse = rmse, mae = mae, mape = mape))
}
# 训练SVM模型
train_svm <- function(x_train, y_train) {
  svm_model <- svm(y_train ~ ., data = x_train)
  return(svm_model)
}
?svm
# 划分训练集和测试集
test_size <- length(target) - 730
train_x_raw <- raw_features[1:730, ]
train_y_raw <- target[1:730]
test_x_raw <- raw_features[731:length(target), ]
test_y_raw <- target[731:length(target)]
train_x_log_diff <- log_diff_features[1:730, ]
train_y_log_diff <- target[1:730]
test_x_log_diff <- log_diff_features[731:length(target), ]
test_y_log_diff <- target[731:length(target)]
# 1个月预测
train_size <- 730
forecast_1_raw <- forecast_svm(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 1)
forecast_1_log_diff <- forecast_svm(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 1)
cat("1-Month Forecast:\n")
print(forecast_1_raw)
cat("\n")
print(forecast_1_log_diff)
# 3个月预测
train_size <- 728
train_x_raw <- raw_features[1:train_size, ]
train_y_raw <- target[1:train_size]
test_x_raw <- raw_features[(train_size+1):length(target), ]
test_y_raw <- target[(train_size+1):length(target)]
train_x_log_diff <- log_diff_features[1:train_size, ]
train_y_log_diff <- target[1:train_size]
test_x_log_diff <- log_diff_features[(train_size+1):length(target), ]
test_y_log_diff <- target[(train_size+1):length(target)]
forecast_3_raw <- forecast_svm(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 3)
forecast_3_log_diff <- forecast_svm(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 3)
cat("\n3-Month Forecast:\n")
print(forecast_3_raw)
cat("\n")
print(forecast_3_log_diff)
# 12个月预测
train_size <- 719
train_x_raw <- raw_features[1:train_size, ]
train_y_raw <- target[1:train_size]
test_x_raw <- raw_features[(train_size+1):length(target), ]
test_y_raw <- target[(train_size+1):length(target)]
train_x_log_diff <- log_diff_features[1:train_size, ]
train_y_log_diff <- target[1:train_size]
test_x_log_diff <- log_diff_features[(train_size+1):length(target), ]
test_y_log_diff <- target[(train_size+1):length(target)]
forecast_12_raw <- forecast_svm(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 12)
forecast_12_log_diff <- forecast_svm(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 12)
cat("\n12-Month Forecast:\n")
print(forecast_12_raw)
cat("\n")
print(forecast_12_log_diff)

# 使用完整训练集训练模型
full_train_x_raw <- raw_features
full_train_y_raw <- target
full_train_x_log_diff <- log_diff_features
full_train_y_log_diff <- target

# 加载测试集数据
test_data <- final_test_dataset
test_x_raw <- test_data[, 1:202]
test_y_raw <- test_data$Inflation_Rate
test_x_log_diff <- test_data[, 203:405]
test_y_log_diff <- test_data$Inflation_Rate

# 使用完整训练集进行预测
full_forecast_1_raw <- forecast_svm(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 1)
full_forecast_1_log_diff <- forecast_svm(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 1)

cat("\nFull Dataset 1-Month Forecast:\n")
print(full_forecast_1_raw)
cat("\n")
print(full_forecast_1_log_diff)

full_forecast_3_raw <- forecast_svm(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 3)
full_forecast_3_log_diff <- forecast_svm(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 3)

cat("\nFull Dataset 3-Month Forecast:\n")
print(full_forecast_3_raw)
cat("\n")
print(full_forecast_3_log_diff)

full_forecast_12_raw <- forecast_svm(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 12)
full_forecast_12_log_diff <- forecast_svm(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 12)

cat("\nFull Dataset 12-Month Forecast:\n")
print(full_forecast_12_raw)
cat("\n")
print(full_forecast_12_log_diff)

full_forecast_49_raw <- forecast_svm(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 49)
full_forecast_49_log_diff <- forecast_svm(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 49)

cat("\nFull Dataset 49-Month Forecast:\n")
print(full_forecast_49_raw)
cat("\n")
print(full_forecast_49_log_diff)

#####################XGBOOST#################

library(xgboost)
library(Metrics)

# 加载数据
data <- final_dataset
rownames(data) <- data$Month
data$Month <- NULL
raw_features <- data[, 1:202]
log_diff_features <- data[, 203:405]
target <- data$Inflation_Rate

# 定义预测函数
forecast_xgb <- function(train_x, train_y, test_x, test_y, horizon) {
  # 训练XGBoost模型
  xgb_model <- train_xgb(train_x, train_y)
  
  # 进行预测
  predictions <- predict(xgb_model, newdata = as.matrix(test_x))
  
  # 计算指标
  actuals <- test_y[1:min(horizon, length(test_y))]
  mse <- mean((predictions[1:length(actuals)] - actuals)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(predictions[1:length(actuals)] - actuals))
  mape <- mape(actuals, predictions[1:length(actuals)])
  
  return(list(preds = predictions[1:length(actuals)], mse = mse, rmse = rmse, mae = mae, mape = mape))
}

# 训练XGBoost模型
train_xgb <- function(x_train, y_train) {
  dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
  
  params <- list(
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100)
  
  return(xgb_model)
}

# 划分训练集和测试集
test_size <- length(target) - 730
train_x_raw <- raw_features[1:730, ]
train_y_raw <- target[1:730]
test_x_raw <- raw_features[731:length(target), ]
test_y_raw <- target[731:length(target)]

train_x_log_diff <- log_diff_features[1:730, ]
train_y_log_diff <- target[1:730]
test_x_log_diff <- log_diff_features[731:length(target), ]
test_y_log_diff <- target[731:length(target)]

# 1个月预测
train_size <- 730
forecast_1_raw <- forecast_xgb(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 1)
forecast_1_log_diff <- forecast_xgb(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 1)

cat("1-Month Forecast:\n")
print(forecast_1_raw)
cat("\n")
print(forecast_1_log_diff)

# 3个月预测
train_size <- 728
train_x_raw <- raw_features[1:train_size, ]
train_y_raw <- target[1:train_size]
test_x_raw <- raw_features[(train_size+1):length(target), ]
test_y_raw <- target[(train_size+1):length(target)]

train_x_log_diff <- log_diff_features[1:train_size, ]
train_y_log_diff <- target[1:train_size]
test_x_log_diff <- log_diff_features[(train_size+1):length(target), ]
test_y_log_diff <- target[(train_size+1):length(target)]

forecast_3_raw <- forecast_xgb(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 3)
forecast_3_log_diff <- forecast_xgb(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 3)

cat("\n3-Month Forecast:\n")
print(forecast_3_raw)
cat("\n")
print(forecast_3_log_diff)

# 12个月预测
train_size <- 719
train_x_raw <- raw_features[1:train_size, ]
train_y_raw <- target[1:train_size]
test_x_raw <- raw_features[(train_size+1):length(target), ]
test_y_raw <- target[(train_size+1):length(target)]

train_x_log_diff <- log_diff_features[1:train_size, ]
train_y_log_diff <- target[1:train_size]
test_x_log_diff <- log_diff_features[(train_size+1):length(target), ]
test_y_log_diff <- target[(train_size+1):length(target)]

forecast_12_raw <- forecast_xgb(train_x_raw, train_y_raw, test_x_raw, test_y_raw, 12)
forecast_12_log_diff <- forecast_xgb(train_x_log_diff, train_y_log_diff, test_x_log_diff, test_y_log_diff, 12)

cat("\n12-Month Forecast:\n")
print(forecast_12_raw)
cat("\n")
print(forecast_12_log_diff)

# 使用完整训练集训练模型
full_train_x_raw <- as.matrix(raw_features)
full_train_y_raw <- target
full_train_x_log_diff <- as.matrix(log_diff_features)
full_train_y_log_diff <- target

# 加载测试集数据
test_data <- final_test_dataset
test_x_raw <- as.matrix(test_data[, 1:202])
test_y_raw <- test_data$Inflation_Rate
test_x_log_diff <- as.matrix(test_data[, 203:405])
test_y_log_diff <- test_data$Inflation_Rate

# 使用完整训练集进行预测
full_forecast_1_raw <- forecast_xgb(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 1)
full_forecast_1_log_diff <- forecast_xgb(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 1)

cat("\nFull Dataset 1-Month Forecast:\n")
print(full_forecast_1_raw)
cat("\n")
print(full_forecast_1_log_diff)

full_forecast_3_raw <- forecast_xgb(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 3)
full_forecast_3_log_diff <- forecast_xgb(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 3)

cat("\nFull Dataset 3-Month Forecast:\n")
print(full_forecast_3_raw)
cat("\n")
print(full_forecast_3_log_diff)

full_forecast_12_raw <- forecast_xgb(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 12)
full_forecast_12_log_diff <- forecast_xgb(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 12)

cat("\nFull Dataset 12-Month Forecast:\n")
print(full_forecast_12_raw)
cat("\n")
print(full_forecast_12_log_diff)

full_forecast_49_raw <- forecast_xgb(full_train_x_raw, full_train_y_raw, test_x_raw, test_y_raw, 49)
full_forecast_49_log_diff <- forecast_xgb(full_train_x_log_diff, full_train_y_log_diff, test_x_log_diff, test_y_log_diff, 49)

cat("\nFull Dataset 49-Month Forecast:\n")
print(full_forecast_49_raw)
cat("\n")
print(full_forecast_49_log_diff)