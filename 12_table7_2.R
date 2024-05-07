####### PACKAGES #######
########################

library("crypto2")
library("data.table")
library("tidyverse")
library("dplyr")
library("tidyr")
library(reshape2)
library(openxlsx)
library(psych)
library(readxl)
library(sandwich)
library(lmtest)
library(forecast)
library(MSwM)
library(fUnitRoots)

#install.packages("fUnitRoots")

setwd("C:/Users/psamu/OneDrive/Documents/corvinus/_szakdoga/code")

## reading data ----------------------------------------------------------------

df_wml <- read.csv("output_p3l1.csv", header = TRUE)
df_wml$weighting.date <- as.Date(df_wml$weighting.date)

MKT <- read.csv("mkt_ret_2w.csv", header = TRUE)

mkt <- MKT$x
names(mkt) <- as.Date(MKT$X)

mkt <- mkt[names(mkt) %in% df_wml$weighting.date]

### input dataframe ------------------------------------------------------------

input <- data.frame("WML" = df_wml$wml, "WML_long" = df_wml$T3 - mkt, "WML_short" = mkt - df_wml$T1, 
                    "MG" = df_wml$X75th - df_wml$X25th, "MGup" = df_wml$X75th - df_wml$X50th,
                    "MGdown" = df_wml$X50th - df_wml$X25th, "MGidr" = df_wml$X90th - df_wml$X10th)

rownames(input) <- df_wml$weighting.date

### MARKET VARIABLES #####

#### mkt ret: past 3 month market return
#### mkt vol: past 3 month weekly market volatility
#### mkt ill: mean weekly Amihud ill

mkt_ret2 <- read.csv("mkt_ret_3m.csv")
mkt_vol2 <- read.csv("mkt_vol_w.csv")
mkt_ill2 <- read.csv("mkt_ill_mean_2w.csv")

## formatting data -------------------------------------------------------------

#converting to vectors
mkt_ret <- mkt_ret2$x
names(mkt_ret) <- as.Date(mkt_ret2$X)
mkt_vol <- mkt_vol2$x
names(mkt_vol) <- as.Date(mkt_vol2$X)
mkt_ill <- mkt_ill2$x
names(mkt_ill) <- as.Date(mkt_ill2$X)

#dates of WML returns
dates <- seq(as.Date("2014-03-26"), to = as.Date("2023-11-22"), by = "14 days")

#selecting only given dates
mkt_ret <- mkt_ret[names(mkt_ret) %in% dates]
mkt_vol <- mkt_vol[names(mkt_vol) %in% dates]
mkt_ill <- mkt_ill[names(mkt_ill) %in% dates]

#excluding first dates as no volatility data is available
df <- input[-c(1:5), ]
df$MktRet <- mkt_ret
df$MktVol <- mkt_vol
df$MktIll <- mkt_ill

df$Date <- as.Date(rownames(df))

# ------------------------------------------------------------------------------

### predictions - storing predicted values to determine change in RMSE

ts_data <- ts(df[, c("WML", "MG")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)#$coefficients
y_MG <- fitted(tslm_model)

ts_data <- ts(df[, c("WML", "MktRet")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)#$coefficients
y_mret <- fitted(tslm_model)

ts_data <- ts(df[, c("WML", "MktVol")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)#$coefficients
y_mvol <- fitted(tslm_model)

ts_data <- ts(df[, c("WML", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)#$coefficients
y_mill <- fitted(tslm_model)

#### OOS tests ####

## MG ##

initial_window_size <- 30

predictions <- numeric()
actual_values <- numeric()
r_squared_values <- numeric()

for (i in initial_window_size:(nrow(df) - 1)) {
  # Subset data up to the current iteration for training
  train_data <- df[1:i, ]
  
  # Fit the linear regression model
  lm_model <- lm(WML ~ MG, data = train_data)
  
  # Forecast one step ahead using the last observation
  forecast <- predict(lm_model, newdata = df[i + 1, ])
  
  # Store the predicted value and the actual value for comparison
  predictions <- c(predictions, forecast)
  actual_values <- c(actual_values, df$WML[i + 1])
  
  # Calculate R-squared value for the current iteration
  r_squared <- summary(lm_model)$r.squared
  r_squared_values <- c(r_squared_values, r_squared)
}

calculate_MSEF_stats <- function(predictions, actual_values, k) {
  # Calculate residuals
  residuals <- actual_values - predictions
  
  # Calculate MSE-F statistic
  MSEF_stat <- MSEFstats(residuals, k = k)
  
  return(MSEF_stat)
}

# mean R squared
mean_r_squared <- mean(r_squared_values)

predictions_MG <- predictions
actual_MG <- actual_values
predictions_insample_MG <- y_MG[31:253]

# RMSE-s
performance_metrics_IS <- sqrt(mean((predictions_insample_MG - actual_MG)^2))
performance_metrics_OS <- sqrt(mean((predictions_MG - actual_MG)^2))


# DELTA RMSE
performance_metrics_OS - performance_metrics_IS

#MSEF_stats_MG <- calculate_MSEF_stats(predictions_MG, predictions_insample_MG, k = 0)


#MSEF_MG <- ((mean((predictions_insample_MG - actual_MG)^2) - mean((predictions_MG - actual_MG)^2)) / 30) / (mean((predictions_MG - actual_MG)^2)/(length(predictions_MG)-30-1))

#R2_MG <- 1-(1-mean_r_squared)*(length(predictions_MG)-30-1)/(length(predictions_MG)-30-2)




## MktRet ##

initial_window_size <- 30

# Initialize variables to store predictions and actual values
predictions <- numeric()
actual_values <- numeric()
r_squared_values <- numeric()

for (i in initial_window_size:(nrow(df) - 1)) {
  # Subset data up to the current iteration for training
  train_data <- df[1:i, ]
  
  # Fit the linear regression model
  lm_model <- lm(WML ~ MktRet, data = train_data)
  
  # Forecast one step ahead using the last observation
  forecast <- predict(lm_model, newdata = df[i + 1, ])
  
  # Store the predicted value and the actual value for comparison
  predictions <- c(predictions, forecast)
  actual_values <- c(actual_values, df$WML[i + 1])
  
  # Calculate R-squared value for the current iteration
  r_squared <- summary(lm_model)$r.squared
  r_squared_values <- c(r_squared_values, r_squared)
}

# Calculate the mean R-squared value
mean_r_squared <- mean(r_squared_values)

predictions_MR <- predictions
actual_MR <- actual_values
predictions_insample_MR <- y_mret[31:253]

# Calculate and print performance metrics (e.g., RMSE, MAE)
performance_metrics_IS <- sqrt(mean((predictions_insample_MR - actual_MR)^2))
performance_metrics_OS <- sqrt(mean((predictions_MR - actual_MR)^2))

performance_metrics_OS - performance_metrics_IS

#MSEF_MR <- ((mean((predictions_insample_MR - actual_MR)^2) - mean((predictions_MR - actual_MR)^2)) / 30) / (mean((predictions_MR - actual_MR)^2)/(length(predictions_MG)-30-1))
#R2_MR <- 1-(1-mean_r_squared)*(length(predictions_MG)-30-1)/(length(predictions_MG)-30-2)

## MktVol ##

initial_window_size <- 30

# Initialize variables to store predictions and actual values
predictions <- numeric()
actual_values <- numeric()
r_squared_values <- numeric()

for (i in initial_window_size:(nrow(df) - 1)) {
  # Subset data up to the current iteration for training
  train_data <- df[1:i, ]
  
  # Fit the linear regression model
  lm_model <- lm(WML ~ MktVol, data = train_data)
  
  # Forecast one step ahead using the last observation
  forecast <- predict(lm_model, newdata = df[i + 1, ])
  
  # Store the predicted value and the actual value for comparison
  predictions <- c(predictions, forecast)
  actual_values <- c(actual_values, df$WML[i + 1])
  
  # Calculate R-squared value for the current iteration
  r_squared <- summary(lm_model)$r.squared
  r_squared_values <- c(r_squared_values, r_squared)
}

# Calculate the mean R-squared value
mean_r_squared <- mean(r_squared_values)

predictions_MV <- predictions
actual_MV <- actual_values
predictions_insample_MV <- y_mvol[31:253]

# Calculate and print performance metrics (e.g., RMSE, MAE)
performance_metrics_IS <- sqrt(mean((predictions_insample_MV - actual_MV)^2))
performance_metrics_OS <- sqrt(mean((predictions_MV - actual_MV)^2))

performance_metrics_OS - performance_metrics_IS

#MSEF_MV <- ((mean((predictions_insample_MV - actual_MV)^2) - mean((predictions_MV - actual_MV)^2)) / 30) / (mean((predictions_MV - actual_MV)^2)/(length(predictions_MV)-30-1))
#R2_MV <- 1-(1-mean_r_squared)*(length(predictions_MG)-30-1)/(length(predictions_MG)-30-2)

## MktIll ##

initial_window_size <- 30

# Initialize variables to store predictions and actual values
predictions <- numeric()
actual_values <- numeric()
r_squared_values <- numeric()

for (i in initial_window_size:(nrow(df) - 1)) {
  # Subset data up to the current iteration for training
  train_data <- df[1:i, ]
  
  # Fit the linear regression model
  lm_model <- lm(WML ~ MktIll, data = train_data)
  
  # Forecast one step ahead using the last observation
  forecast <- predict(lm_model, newdata = df[i + 1, ])
  
  # Store the predicted value and the actual value for comparison
  predictions <- c(predictions, forecast)
  actual_values <- c(actual_values, df$WML[i + 1])
  
  # Calculate R-squared value for the current iteration
  r_squared <- summary(lm_model)$r.squared
  r_squared_values <- c(r_squared_values, r_squared)
}

# Calculate the mean R-squared value
mean_r_squared <- mean(r_squared_values)

predictions_MI <- predictions
actual_MI <- actual_values
predictions_insample_MI <- y_mvol[31:253]

# Calculate and print performance metrics (e.g., RMSE, MAE)
performance_metrics_IS <- sqrt(mean((predictions_insample_MI - actual_MI)^2))
performance_metrics_OS <- sqrt(mean((predictions_MI - actual_MI)^2))

performance_metrics_OS - performance_metrics_IS

#MSEF_MI <- ((mean((predictions_insample_MI - actual_MI)^2) - mean((predictions_MI - actual_MI)^2)) / 30) / (mean((predictions_MI - actual_MI)^2)/(length(predictions_MI)-30-1))
#R2_MI <- 1-(1-mean_r_squared)*(length(predictions_MG)-30-1)/(length(predictions_MG)-30-2)


##### MG idr ###

ts_data <- ts(df[, c("WML", "MGidr")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)#$coefficients

y_MGidr <- fitted(tslm_model)

initial_window_size <- 30

# Initialize variables to store predictions and actual values
predictions <- numeric()
actual_values <- numeric()
r_squared_values <- numeric()

for (i in initial_window_size:(nrow(df) - 1)) {
  # Subset data up to the current iteration for training
  train_data <- df[1:i, ]
  
  # Fit the linear regression model
  lm_model <- lm(WML ~ MGidr, data = train_data)
  
  # Forecast one step ahead using the last observation
  forecast <- predict(lm_model, newdata = df[i + 1, ])
  
  # Store the predicted value and the actual value for comparison
  predictions <- c(predictions, forecast)
  actual_values <- c(actual_values, df$WML[i + 1])
  
  # Calculate R-squared value for the current iteration
  r_squared <- summary(lm_model)$r.squared
  r_squared_values <- c(r_squared_values, r_squared)
}

# Calculate the mean R-squared value
mean_r_squared <- mean(r_squared_values)

predictions_MGidr <- predictions
actual_MGidr <- actual_values
predictions_insample_MGidr <- y_MGidr[31:253]

# Calculate and print performance metrics (e.g., RMSE, MAE)
performance_metrics_IS <- sqrt(mean((predictions_insample_MGidr - actual_MGidr)^2))
performance_metrics_OS <- sqrt(mean((predictions_MGidr - actual_MGidr)^2))

performance_metrics_OS - performance_metrics_IS












#### trial - quadratic #####



ts_data <- ts(df[, c("WML", "MG")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + I(ts_data[, 2]^2))
summary(tslm_model)#$coefficients

y_MG <- fitted(tslm_model)



initial_window_size <- 30

# Initialize variables to store predictions and actual values
predictions <- numeric()
actual_values <- numeric()
r_squared_values <- numeric()

for (i in initial_window_size:(nrow(df) - 1)) {
  # Subset data up to the current iteration for training
  train_data <- df[1:i, ]
  
  # Fit the quadratic regression model
  lm_model <- lm(WML ~ MG + I(MG^2), data = train_data)
  
  # Forecast one step ahead using the last observation
  forecast <- predict(lm_model, newdata = df[i + 1, ])
  
  # Store the predicted value and the actual value for comparison
  predictions <- c(predictions, forecast)
  actual_values <- c(actual_values, df$WML[i + 1])
  
  # Calculate R-squared value for the current iteration
  r_squared <- summary(lm_model)$r.squared
  r_squared_values <- c(r_squared_values, r_squared)
}

# Calculate the mean R-squared value
mean_r_squared <- mean(r_squared_values)

# Output the mean R-squared value
print(paste("Mean R-squared:", mean_r_squared))

predictions_MG <- predictions
actual_MG <- actual_values
predictions_insample_MG <- y_MG[30:252]

# Calculate and print performance metrics (e.g., RMSE, MAE)
performance_metrics_IS <- sqrt(mean((predictions_insample_MG - actual_MG)^2))
performance_metrics_OS <- sqrt(mean((predictions_MG - actual_MG)^2))

performance_metrics_OS - performance_metrics_IS
