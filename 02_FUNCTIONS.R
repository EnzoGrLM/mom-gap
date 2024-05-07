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

#install.packages("readxl")

setwd("C:/Users/psamu/OneDrive/Documents/corvinus/_szakdoga/code")

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

##################################################

############## F4H1 #########################

###### INPUT ##########
formation <- 28
lag <- 0
holding <- 7
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f4h1 <- output

write.csv(output_f4h1, file = "output_f4h1.csv", row.names = TRUE)

############## F3H1 #########################

###### INPUT ##########
formation <- 21
lag <- 0
holding <- 7
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f3h1 <- output

write.csv(output_f3h1, file = "output_f3h1.csv", row.names = TRUE)

############## F2H1 #########################

###### INPUT ##########
formation <- 14
lag <- 0
holding <- 7
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f2h1 <- output

write.csv(output_f2h1, file = "output_f2h1.csv", row.names = TRUE)

############## F1H1 #########################

###### INPUT ##########
formation <- 7
lag <- 0
holding <- 7
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f1h1 <- output

write.csv(output_f1h1, file = "output_f1h1.csv", row.names = TRUE)

############## F1H2 #########################

###### INPUT ##########
formation <- 7
lag <- 0
holding <- 14
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f1h2 <- output

write.csv(output_f1h2, file = "output_f1h2.csv", row.names = TRUE)

############## F2H2 #########################

###### INPUT ##########
formation <- 14
lag <- 0
holding <- 14
rebalancing <- 7
####################### 

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f2h2 <- output

write.csv(output_f2h2, file = "output_f2h2.csv", row.names = TRUE)

############## F3H2 #########################

###### INPUT ##########
formation <- 21
lag <- 0
holding <- 14
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f3h2 <- output

write.csv(output_f3h2, file = "output_f3h2.csv", row.names = TRUE)

############## F4H2 #########################

###### INPUT ##########
formation <- 28
lag <- 0
holding <- 14
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f4h2 <- output

write.csv(output_f4h2, file = "output_f4h2.csv", row.names = TRUE)

############## F1H3 #########################

###### INPUT ##########
formation <- 7
lag <- 0
holding <- 21
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f1h3 <- output

write.csv(output_f1h3, file = "output_f1h3.csv", row.names = TRUE)

############## F2H3 #########################

###### INPUT ##########
formation <- 14
lag <- 0
holding <- 21
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f2h3 <- output

write.csv(output_f2h3, file = "output_f2h3.csv", row.names = TRUE)

############## F3H3 #########################

###### INPUT ##########
formation <- 21
lag <- 0
holding <- 21
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f3h3 <- output

write.csv(output_f3h3, file = "output_f3h3.csv", row.names = TRUE)

############## F4H3 #########################

###### INPUT ##########
formation <- 28
lag <- 0
holding <- 21
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f4h3 <- output

write.csv(output_f4h3, file = "output_f4h3.csv", row.names = TRUE)

############## F1H4 #########################

###### INPUT ##########
formation <- 7
lag <- 0
holding <- 28
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f1h4 <- output

write.csv(output_f1h4, file = "output_f1h4.csv", row.names = TRUE)

############## F2H4 #########################

###### INPUT ##########
formation <- 14
lag <- 0
holding <- 28
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f2h4 <- output

write.csv(output_f2h4, file = "output_f2h4.csv", row.names = TRUE)

############## F3H4 #########################

###### INPUT ##########
formation <- 21
lag <- 0
holding <- 28
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f3h4 <- output

write.csv(output_f3h4, file = "output_f3h4.csv", row.names = TRUE)

############## F4H4 #########################

###### INPUT ##########
formation <- 28
lag <- 0
holding <- 28
rebalancing <- 7
#######################

start_of_last_formation <- as.Date("2023-12-31") - formation - holding
days_difference <- as.numeric(start_of_last_formation - as.Date("2014-01-01"))
num_formations <- floor(days_difference / (as.numeric(rebalancing)))
sample_start <- as.Date("2014-01-01")

output <- data.frame(matrix(ncol = 16, nrow = num_formations))
names(output) <- c("weighting date", "number of assets", "wml", "wml+", "wml-", "5th", "10th",
                   "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")

for (i in 1:num_formations) {
  
  #calculating dates
  form_start <- sample_start + (i-1) * rebalancing
  weighting_date <- form_start + formation - 1
  form_end <- form_start + formation - lag - 1
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + holding - 1
  
  #levalogatas
  prices <- levalogatas(prices_df, form_start, hpr_end)
  mcap <- levalogatas(mcap_df, form_start, hpr_end)
  vol <- levalogatas(vol_df, form_start, hpr_end)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, weighting_date, stable_list)
  
  #signal generalas
  signal <- signal_generalas(szurt_prices, form_start, form_end, formation)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights <- sorting(signal, mcap_df, weighting_date)
  
  #hpr
  hpr <- holding_returns(prices, weights, hpr_start, hpr_end, holding)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  #saving results
  output[i, 1] <- weighting_date
  output[i, 2] <- length(hpr)
  output[i, 3] <- wml
  output[i, 4] <- wml_long
  output[i, 5] <- wml_short
  output[i, 6:16] <- mg
  
  #how far in the loop?
  print(wml)
  print(paste(i, "out of", num_formations, "done"))
  
}

output_f4h4 <- output

write.csv(output_f4h4, file = "output_f4h4.csv", row.names = TRUE)











###### plots #############x

df <- data.frame(number = appended_values, dates = as.Date(appended_dates))

ggplot(df, aes(x = dates, y = as.numeric(number))) +
  geom_line() +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  scale_color_manual(values = "black") +
  labs(x = "Date", y = "Number of coins", title = "Number of cryptocurrencies in dataset") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Georgia"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


################################################
################# FUNCTIONS ####################
################################################

#levalogatas
levalogatas <- function(df, form_start, hpr_end) {
  filtered_df <- df %>%
    filter(as.Date(rownames(df)) >= as.Date(form_start) & 
             as.Date(rownames(df)) <= as.Date(hpr_end))
  return(filtered_df)
}
#adatszures
adatszures <- function(prices, mcaps, weighting_date, stable_list) {
  
  #parameters
  mcap_limit <- 1000000
  missing_limit <- 0
  stablecoins <- stable_list
  
  #stablecoins
  prices_og <- prices %>%
    select(-any_of(stablecoins))
  mcaps_og <- mcaps %>%
    select(-any_of(stablecoins))
  
  #storing og dataframes
  prices <- prices_og %>%
    filter(as.Date(rownames(prices_og)) <= weighting_date)
  mcaps <- mcaps_og %>%
    filter(as.Date(rownames(mcaps_og)) <= weighting_date)
  
  # missing values
  filtered_prices <- prices %>%
    select(-which(colSums(is.na(.)) > missing_limit))
  filtered_mcap <- mcaps %>%
    select(-which(colSums(is.na(.)) > missing_limit))
  
  #updating dataframes
  p_symb <- colnames(filtered_prices)
  m_symb <- colnames(filtered_mcap)
  symb_stays <- Reduce(intersect, list(p_symb, m_symb))
  prices <- prices %>%
    select(any_of(symb_stays))
  mcaps <- mcaps %>%
    select(any_of(symb_stays))
  
  #mcap limit
  mcap_abovelim <- mcaps %>%
    select_if(~all(. >= mcap_limit, na.rm = TRUE))
  
  #updating dataframes
  m_symb <- colnames(mcap_abovelim)
  symb_stays <- Reduce(intersect, list(m_symb))
  prices <- prices_og %>%
    select(any_of(symb_stays))
  mcaps <- mcaps_og %>%
    select(any_of(symb_stays))
  
  #prices$timestamp <- as.Date(prices_og$timestamp)
  #mcaps$timestamp <- as.Date(mcaps_og$timestamp)
  
  return(prices)
}
#sign generalas
signal_generalas <- function(prices, form_start, form_end, formation) {
  
  #parameters
  winsorize <- 0.01

  #form period prices
  prices <- prices %>%
    filter(as.Date(rownames(prices)) >= as.Date(form_start) & 
             as.Date(rownames(prices)) <= as.Date(form_end))
  
  #form period log prices
  cumret_first <- log(prices[1, ])
  cumret_last <- log(tail(prices, n = 1))
  
  #form period cum returns
  result <- cumret_last / cumret_first #### ez majd csak a HPR-hoz/ (formation / 7)
  
  #result_sorted <- data.frame(t(apply(result, 1, sort)))
  result_sorted <- (t(apply(result, 1, sort)))
  
  #winsorizing
  result_wins <- winsorizing(result_sorted, winsorize)
  
  return(result_wins)
}
#winsorizing
winsorizing_old <- function(mat, p) {

  lower_percentile <- apply(mat, 2, quantile, probs = p)
  upper_percentile <- apply(mat, 2, quantile, probs = 1 - p)
  
  winsorized_mat <- mat
  for (i in 1:ncol(mat)) {
    winsorized_mat[, i][winsorized_mat[, i] < lower_percentile[i]] <- lower_percentile[i]
    winsorized_mat[, i][winsorized_mat[, i] > upper_percentile[i]] <- upper_percentile[i]
  }
  
  return(winsorized_mat)
}
winsorizing <- function(mat, p) {
  
  handle_na <- function(mat) {
    is_na <- is.na(mat)
    mat_na_removed <- mat
    mat_na_removed[is.na(mat)] <- 0  # Replace NA with 0 temporarily
    return(list(mat_na_removed = mat_na_removed, is_na = is_na))
  }
  
  mat_info <- handle_na(mat)
  mat_na_removed <- mat_info$mat_na_removed
  is_na <- mat_info$is_na
  
  lower_percentile <- apply(mat_na_removed, 2, function(x) quantile(x[!is_na], probs = p, na.rm = TRUE))
  upper_percentile <- apply(mat_na_removed, 2, function(x) quantile(x[!is_na], probs = 1 - p, na.rm = TRUE))
  
  winsorized_mat <- mat_na_removed
  for (i in 1:ncol(mat_na_removed)) {
    winsorized_mat[!is_na[, i], i][winsorized_mat[!is_na[, i], i] < lower_percentile[i]] <- lower_percentile[i]
    winsorized_mat[!is_na[, i], i][winsorized_mat[!is_na[, i], i] > upper_percentile[i]] <- upper_percentile[i]
  }
  
  winsorized_mat[is_na] <- lower_percentile[is_na]
  
  return(winsorized_mat)
}
#percentiles
mg_calculation_1 <- function(signal) {
  
  percentiles <- quantile(signal, probs = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95))
  
  col_names <- c("5th", "10th", "15th", "20th", "25th", "50th", "75th", "80th", "85th", "90th", "95th")
  percentiles_named <- setNames(percentiles, col_names)
  
  return(percentiles_named)
}
#portfolio_help_limits
calculate_percentiles <- function(vector, k) {

  percentiles <- seq(0, 1, by = 1 / k)
  
  quantiles <- quantile(vector, probs = percentiles)
  
  return(quantiles)
}
#portfolio_help_mom_portfolio
mom_portf <- function(original_vector, lower_limit, upper_limit) {
  new_vector <- ifelse(original_vector <= lower_limit, -1,
                       ifelse(original_vector >= upper_limit, 1, 0))
  return(new_vector)
}
#portfolios
sorting <- function(signal, mcap, weighting_date) {
  
  #parameters
  val_weighted <- TRUE
  num_portfolios <- 5
  
  #portfolio breakpoints
  limits <- calculate_percentiles(signal, num_portfolios)
  limit_lower <- limits[2]
  limit_upper <- limits[num_portfolios]
  
  portfolios_mom <- mom_portf(signal, limit_lower, limit_upper)
  
  #getting mcap
  coins <- colnames(signal)
  mcaps <- mcap %>%
    filter(rownames(mcap) == weighting_date) %>%
    select(one_of(coins))
  
  #handling missing values
  mcaps[is.na(mcaps)] <- 1
  
  #mcap weights
  df <- data.frame(Market_Cap = t(mcaps), Value = t(portfolios_mom))
  colnames(df) <- c("Market_Cap", "Value")
  
  #weight vectors
  df <- df %>%
    group_by(Value) %>%
    mutate(Sum_Market_Cap = sum(Market_Cap))
  df <- df %>%
    group_by(Value) %>%
    mutate(Asset_Count = n())
  df <- df %>%
    mutate(Weighted_Market_Cap = Market_Cap / Sum_Market_Cap,
           One_By_N = 1 / Asset_Count)
  
  df <- as.data.frame(df)
  rownames(df) <- coins

  #getting weights
  val_weights <- as.matrix(df$Weighted_Market_Cap)
  eq_weights <- as.matrix(df$One_By_N)
  rownames(val_weights) <- coins
  rownames(eq_weights) <- coins
  
  value_weights <- as.vector(portfolios_mom) * as.vector(val_weights)
  names(value_weights) <- coins
  equal_weights <- as.vector(portfolios_mom) * as.vector(eq_weights)
  names(equal_weights) <- coins
  
  if (val_weighted) {
    return(value_weights)
  } else {
    return(equal_weights)
  }
  
}
#holding period returns
holding_returns <- function(prices, weights, hpr_start, hpr_end, holding) {
  
  coins <- names(weights)
  
  #hp prices
  prices <- prices %>%
    filter(as.Date(rownames(prices)) >= as.Date(hpr_start) & 
             as.Date(rownames(prices)) <= as.Date(hpr_end)) %>%
    select(one_of(coins))
  
  #winsorize
  #wins <- 0.01
  #prices <- winsorizing(prices, 0.01)
  
  #hp log prices
  cumret_first <- log(prices[1, ])
  cumret_last <- log(tail(prices, n = 1))
  
  #hpr
  result <- as.vector(t((cumret_last / cumret_first) / (holding / 7)))
  names(result) <- coins
  
  first_percentile <- quantile(result, probs = 0.01, na.rm = TRUE)
  result[is.na(result)] <- first_percentile
  ninety_ninth_percentile <- quantile(result, probs = 0.99, na.rm = TRUE)
  result[result > ninety_ninth_percentile] <- ninety_ninth_percentile
  
  return(result)
}
#wml return
wml_return <- function(weights, holding_period_return) {
  
  coins <- names(weights)
  
  result <- sum(weights*holding_period_return)
  
  return(result)
}
#wml_short return
wml_short_return <- function(weights, holding_period_return) {
  
  coins <- names(weights)
  
  result <- -1*sum(weights[weights<0]*holding_period_return[weights<0])
  
  return(result)
}
#wml_long return
wml_long_return <- function(weights, holding_period_return) {
  
  coins <- names(weights)
  
  result <- sum(weights[weights>0]*holding_period_return[weights>0])
  
  return(result)
}



