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

#install.packages("lmtest")

setwd("C:/Users/psamu/OneDrive/Documents/corvinus/_szakdoga/code")

## reading data ----------------------------------------------------------------

df_wml <- read.csv("output_p3l1.csv", header = TRUE)
df_wml$weighting.date <- as.Date(df_wml$weighting.date)

MKT <- read.csv("mkt_ret_2w.csv", header = TRUE)

mkt <- MKT$x
names(mkt) <- as.Date(MKT$X)

## selecting market return for given date --------------------------------------

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

#### CALCULATIONS #### ---------------------------------------------------------

### regressions of WML ####

ts_data <- ts(df[, c("WML", "MG")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)#$coefficients

y_MG <- fitted(tslm_model)

ts_data <- ts(df[, c("WML", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4])
summary(tslm_model)#$coefficients

ts_data <- ts(df[, c("WML", "MG", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4]+ ts_data[, 5])
summary(tslm_model)#$coefficients

### regressions of WML_long ####

ts_data <- ts(df[, c("WML_long", "MG")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)#$coefficients

ts_data <- ts(df[, c("WML_long", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4])
summary(tslm_model)#$coefficients

ts_data <- ts(df[, c("WML_long", "MG", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4]+ ts_data[, 5])
summary(tslm_model)#$coefficients

### regressions of WML_short ####

ts_data <- ts(df[, c("WML_short", "MG")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)

ts_data <- ts(df[, c("WML_short", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4])
summary(tslm_model)#$coefficients

ts_data <- ts(df[, c("WML_short", "MG", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4]+ ts_data[, 5])
summary(tslm_model)#$coefficients

## az output adatok nincsenek egy dataframebe kigyujtve, az adott summary fuggveny mindig kiirja

#######################################
##### MG idr ##########


### regressions of WML ####

ts_data <- ts(df[, c("WML", "MGidr")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)#$coefficients

y_MG <- fitted(tslm_model)

ts_data <- ts(df[, c("WML", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4])
summary(tslm_model)#$coefficients

ts_data <- ts(df[, c("WML", "MGidr", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4]+ ts_data[, 5])
summary(tslm_model)#$coefficients

### regressions of WML_long ####

ts_data <- ts(df[, c("WML_long", "MGidr")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)#$coefficients

ts_data <- ts(df[, c("WML_long", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4])
summary(tslm_model)#$coefficients

ts_data <- ts(df[, c("WML_long", "MGidr", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4]+ ts_data[, 5])
summary(tslm_model)#$coefficients

### regressions of WML_short ####

ts_data <- ts(df[, c("WML_short", "MGidr")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2])
summary(tslm_model)

ts_data <- ts(df[, c("WML_short", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4])
summary(tslm_model)#$coefficients

ts_data <- ts(df[, c("WML_short", "MGidr", "MktRet", "MktVol", "MktIll")], frequency = 1)
tslm_model <- tslm(ts_data[, 1] ~ ts_data[, 2] + ts_data[, 3] + ts_data[, 4]+ ts_data[, 5])
summary(tslm_model)#$coefficients
