########################
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
library(quantmod)
library(zoo)

#install.packages("zoo")

setwd("C:/Users/psamu/OneDrive/Documents/corvinus/_szakdoga/code")
date_sequence <- seq(as.Date("2014-01-29"), by = "7 days", length.out = 600)


######## reading data ##########

prices_df <- read.csv("coin_prices.csv", header = TRUE)
mcap_df <- read.csv("coin_mcap.csv", header = TRUE)
vol_df <- read.csv("coin_vol.csv", header = TRUE)

######## formatting data ###########

prices_df$timestamp <- as.Date(prices_df$timestamp)
mcap_df$timestamp <- as.Date(mcap_df$timestamp)
vol_df$timestamp <- as.Date(vol_df$timestamp)

stable <- read.csv("stablecoins.csv", header = FALSE)
stable_list <- unlist(stable[, 1], use.names = FALSE)

prices_df <- head(prices_df, -2)
mcap_df <- head(mcap_df, -2)
vol_df <- head(vol_df, -2)

rnames <- as.Date(prices_df$timestamp)
rownames(prices_df) <- rnames
rownames(mcap_df) <- rnames
rownames(vol_df) <- rnames

prices_df <- subset(prices_df, select = -timestamp)
mcap_df <- subset(mcap_df, select = -timestamp)
vol_df <- subset(vol_df, select = -timestamp)

#######################################

### daily log returns ###

log_returns <- log(prices_df) - log(dplyr::lag(prices_df))

str(log_returns)
log_returns_df <- log_returns

dates <- seq(as.Date("2014-01-08"), as.Date("2023-12-19"), by = "7 days")

# weekly returns
num_rows <- nrow(log_returns)
rolling_sum <- matrix(NA, nrow = num_rows - 6, ncol = ncol(log_returns_df))
rownames(rolling_sum) <- rownames(log_returns)[7:(num_rows)]
colnames(rolling_sum) <- colnames(log_returns)

for (i in 1:ncol(log_returns_df)) {
  rolling_sum[, i] <- rowSums(embed(log_returns_df[, i], 7), na.rm = TRUE)
}

rolling_sum <- as.data.frame(rolling_sum)
rolling_sum_weekly <- rolling_sum

# 2weekly returns
num_rows <- nrow(log_returns)
rolling_sum <- matrix(NA, nrow = num_rows - 13, ncol = ncol(log_returns_df))
rownames(rolling_sum) <- rownames(log_returns)[14:(num_rows)]
colnames(rolling_sum) <- colnames(log_returns)

for (i in 1:ncol(log_returns_df)) {
  rolling_sum[, i] <- rowSums(embed(log_returns_df[, i], 14), na.rm = TRUE)
}

rolling_sum <- as.data.frame(rolling_sum)
rolling_sum_2weekly <- rolling_sum

# monthly returns
num_rows <- nrow(log_returns)
rolling_sum <- matrix(NA, nrow = num_rows - 27, ncol = ncol(log_returns_df))
rownames(rolling_sum) <- rownames(log_returns)[28:(num_rows)]
colnames(rolling_sum) <- colnames(log_returns)

for (i in 1:ncol(log_returns_df)) {
  rolling_sum[, i] <- rowSums(embed(log_returns_df[, i], 28), na.rm = TRUE)
}

rolling_sum <- as.data.frame(rolling_sum)
rolling_sum_monthly <- rolling_sum

# 3 monthly returns
num_rows <- nrow(log_returns)
rolling_sum <- matrix(NA, nrow = num_rows - 83, ncol = ncol(log_returns_df))
rownames(rolling_sum) <- rownames(log_returns)[84:(num_rows)]
colnames(rolling_sum) <- colnames(log_returns)

for (i in 1:ncol(log_returns_df)) {
  rolling_sum[, i] <- rowSums(embed(log_returns_df[, i], 84), na.rm = TRUE)
}

rolling_sum <- as.data.frame(rolling_sum)
rolling_sum_3monthly <- rolling_sum

#mcap weights
row_sums <- rowSums(mcap_df, na.rm = TRUE)
mcap_weights_df <- as.data.frame(mcap_df / row_sums)

#getting weighting dates
#filtered_mcap_weights_df <- as.data.frame(mcap_weights_df[row.names(mcap_weights_df) %in% dates, ])
#filtered_rolling_sum_weekly <- as.data.frame(rolling_sum_weekly[row.names(rolling_sum_weekly) %in% dates, ])
#filtered_rolling_sum_monthly <- as.data.frame(rolling_sum_monthly[row.names(rolling_sum_monthly) %in% dates, ])

#market returns
mwt_w <- as.matrix(mcap_weights_df) 
w_ret <- as.matrix(rolling_sum_weekly)
w2_ret <- as.matrix(rolling_sum_2weekly)
m_ret <- as.matrix(rolling_sum_monthly) 
m3_ret <- as.matrix(rolling_sum_3monthly)

## excluding missing and infinite values

mwt_w[is.nan(mwt_w) | is.na(mwt_w)] <- 0
w_ret[is.nan(w_ret) | is.na(w_ret)] <- 0
w2_ret[is.nan(w2_ret) | is.na(w2_ret)] <- 0
m_ret[is.nan(m_ret) | is.na(m_ret)] <- 0
m3_ret[is.nan(m3_ret) | is.na(m3_ret)] <- 0

mwt_w <- replace(mwt_w, !is.finite(mwt_w), 0)
w_ret <- replace(w_ret, !is.finite(w_ret), 0)
w2_ret <- replace(w2_ret, !is.finite(w2_ret), 0)
m_ret <- replace(m_ret, !is.finite(m_ret), 0)
m3_ret <- replace(m3_ret, !is.finite(m3_ret), 0)



# FINAL market returns
mktret_w <- rowSums(mwt_w[7:nrow(mwt_w), ]*w_ret, na.rm = TRUE)
mktret_w2 <- rowSums(mwt_w[14:nrow(mwt_w), ]*w2_ret, na.rm = TRUE)
mktret_m <- rowSums(mwt_w[28:nrow(mwt_w), ]*m_ret, na.rm = TRUE)
mktret_3m <- rowSums(mwt_w[84:nrow(mwt_w), ]*m3_ret, na.rm = TRUE)


### market returns ####
print(mktret_w) #weekly

print(mktret_m) #monthly
mktret_w_selected <- mktret_w[names(mktret_w) %in% dates]




#market volatility - weekly volatility - 12 weeks
volatility <- rollapply(mktret_w_selected, width = 12, FUN = sd, align = "right", fill = NA)
volatility <- volatility[12:length(volatility)]
n_dat <- names(mktret_w_selected)[12:length(mktret_w_selected)]
names(volatility) <- n_dat
mkt_vol_w <- volatility

#### market vol #####
print(mkt_vol_w)


#amihud illiquidity
illiq <- abs(log_returns) / vol_df
df <- as.matrix(illiq)
df[is.na(df) | is.nan(df) | is.infinite(df)] <- 0
mkt_illiq <- as.data.frame(df)
mkt_ill <- rowSums(mkt_illiq*mwt_w, na.rm = TRUE)
# mkt_ill[is.na(mkt_ill) | is.nan(mkt_ill) | is.infinite(mkt_ill)] <- 0
form <- 14
mean_mkt_ill <- numeric(length(mkt_ill) - form + 1)

## 2-week mean of illiquidity

for (i in 1:length(mean_mkt_ill)) {
  mean_mkt_ill[i] <- mean(mkt_ill[i:(i + form - 1)])
}
nms <- names(mkt_ill)
names(mean_mkt_ill) <- nms[14:length(nms)]

#### market illiquidity ####
print(mean_mkt_ill)

#### saving data
write.csv(mktret_w, file = "mkt_ret_w.csv", row.names = TRUE)
write.csv(mktret_w2, file = "mkt_ret_2w.csv", row.names = TRUE)
write.csv(mktret_m, file = "mkt_ret_m.csv", row.names = TRUE)
write.csv(mktret_3m, file = "mkt_ret_3m.csv", row.names = TRUE)
write.csv(mkt_vol_w, file = "mkt_vol_w.csv", row.names = TRUE)
write.csv(mkt_ill, file = "mkt_ill_daily.csv", row.names = TRUE)
write.csv(mean_mkt_ill, file = "mkt_ill_mean_2w.csv", row.names = TRUE)
####


