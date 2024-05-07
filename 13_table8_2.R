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
library(sandwich)
library(lmtest)
library(forecast)
library(zoo)
library(DescTools)

#install.packages("DescTools")

setwd("C:/Users/psamu/OneDrive/Documents/corvinus/_szakdoga/code")

#### READING AND FORMATTING INPUT DATA ####### ---------------------------------

df_wml <- read.csv("output_p3l1.csv", header = TRUE)
df_wml$weighting.date <- as.Date(df_wml$weighting.date)

MKT <- read.csv("mkt_ret_2w.csv", header = TRUE)

mkt <- MKT$x
names(mkt) <- as.Date(MKT$X)

mkt <- mkt[names(mkt) %in% df_wml$weighting.date]

input <- data.frame("WML" = df_wml$wml, "WML_long" = df_wml$T3 - mkt, "WML_short" = mkt - df_wml$T1, 
                    "MG" = df_wml$X75th - df_wml$X25th, "MGup" = df_wml$X75th - df_wml$X50th,
                    "MGdown" = df_wml$X50th - df_wml$X25th, "MGidr" = df_wml$X90th - df_wml$X10th)

rownames(input) <- df_wml$weighting.date

#### mkt ret: past 3 month market return
#### mkt vol: past 3 month weekly market volatility
#### mkt ill: mean weekly Amihud ill

mkt_ret2 <- read.csv("mkt_ret_3m.csv")
mkt_vol2 <- read.csv("mkt_vol_w.csv")
mkt_ill2 <- read.csv("mkt_ill_mean_2w.csv")

mkt_ret <- mkt_ret2$x
names(mkt_ret) <- as.Date(mkt_ret2$X)
mkt_vol <- mkt_vol2$x
names(mkt_vol) <- as.Date(mkt_vol2$X)
mkt_ill <- mkt_ill2$x
names(mkt_ill) <- as.Date(mkt_ill2$X)

dates <- seq(as.Date("2014-03-26"), to = as.Date("2023-11-22"), by = "14 days")

mkt_ret <- mkt_ret[names(mkt_ret) %in% dates]
mkt_vol <- mkt_vol[names(mkt_vol) %in% dates]
mkt_ill <- mkt_ill[names(mkt_ill) %in% dates]

df <- input[-c(1:5), ]
df$MktRet <- mkt_ret
df$MktVol <- mkt_vol
df$MktIll <- mkt_ill

df$Date <- as.Date(rownames(df))

#### ----------------------------------------------------------------------------

##### CALCULATIONS #####

# function to count mom crashes below threshold
mom_crash_count <- function(input_vector, threshold) {
  
  output_vector <- numeric(length(input_vector))
  
  for (i in seq_along(input_vector)) {
    if (input_vector[i] <= threshold) {
      output_vector[i] <- 1
    } else {
      output_vector[i] <- 0
    }
  }
  
  names(output_vector) <- names(input_vector)
  
  return(output_vector)
}

### counting crashes
crash025 <- mom_crash_count(df$WML, -0.025) 
sum(crash025)

crash05 <- mom_crash_count(df$WML, -0.05) 
sum(crash05)

crash075 <- mom_crash_count(df$WML, -0.075) 
sum(crash075)

crash10 <- mom_crash_count(df$WML, -0.1) 
sum(crash10)

crash125 <- mom_crash_count(df$WML, -0.125) 
sum(crash125)

crash15 <- mom_crash_count(df$WML, -0.15) 
sum(crash15)

df$crash025 <- crash025
df$crash05 <- crash05
df$crash075 <- crash075
df$crash10 <- crash10
df$crash125 <- crash125
df$crash15 <- crash15

### MG ###

### logistic regressions

ts_data <- ts(df[, c("crash025", "MG")], frequency = 1)
logistic_model <- glm(crash025 ~ MG, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash05", "MG")], frequency = 1)
logistic_model <- glm(crash05 ~ MG, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash075", "MG")], frequency = 1)
logistic_model <- glm(crash075 ~ MG, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash10", "MG")], frequency = 1)
logistic_model <- glm(crash10 ~ MG, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash125", "MG")], frequency = 1)
logistic_model <- glm(crash125 ~ MG, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash15", "MG")], frequency = 1)
logistic_model <- glm(crash15 ~ MG, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)


### MG es tsa ###

### logistic regressions - multiple variables

ts_data <- ts(df[, c("crash025", "MG", "MktRet", "MktVol", "MktIll")], frequency = 1)
logistic_model <- glm(crash025 ~ MG + MktRet + MktVol + MktIll, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash05", "MG", "MktRet", "MktVol", "MktIll")], frequency = 1)
logistic_model <- glm(crash05 ~ MG + MktRet + MktVol + MktIll, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash075", "MG", "MktRet", "MktVol", "MktIll")], frequency = 1)
logistic_model <- glm(crash075 ~ MG + MktRet + MktVol + MktIll, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)

PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash10", "MG", "MktRet", "MktVol", "MktIll")], frequency = 1)
logistic_model <- glm(crash10 ~ MG + MktRet + MktVol + MktIll, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash125", "MG", "MktRet", "MktVol", "MktIll")], frequency = 1)
logistic_model <- glm(crash125 ~ MG + MktRet + MktVol + MktIll, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash15", "MG")], frequency = 1)
logistic_model <- glm(crash15 ~ MG, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

#### -------------------------------------------------------------------------------------

### SAME FOR MGidr ###########################

### MGidr ###

ts_data <- ts(df[, c("crash025", "MGidr")], frequency = 1)
logistic_model <- glm(crash025 ~ MGidr, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash05", "MGidr")], frequency = 1)
logistic_model <- glm(crash05 ~ MGidr, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash075", "MGidr")], frequency = 1)
logistic_model <- glm(crash075 ~ MGidr, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash10", "MGidr")], frequency = 1)
logistic_model <- glm(crash10 ~ MGidr, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash125", "MGidr")], frequency = 1)
logistic_model <- glm(crash125 ~ MGidr, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash15", "MGidr")], frequency = 1)
logistic_model <- glm(crash15 ~ MGidr, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)


### MGidr es tsa ###

ts_data <- ts(df[, c("crash025", "MGidr", "MktRet", "MktVol", "MktIll")], frequency = 1)
logistic_model <- glm(crash025 ~ MGidr + MktRet + MktVol + MktIll, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash05", "MGidr", "MktRet", "MktVol", "MktIll")], frequency = 1)
logistic_model <- glm(crash05 ~ MGidr + MktRet + MktVol + MktIll, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash075", "MGidr", "MktRet", "MktVol", "MktIll")], frequency = 1)
logistic_model <- glm(crash075 ~ MGidr + MktRet + MktVol + MktIll, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)

PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash10", "MGidr", "MktRet", "MktVol", "MktIll")], frequency = 1)
logistic_model <- glm(crash10 ~ MGidr + MktRet + MktVol + MktIll, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)

ts_data <- ts(df[, c("crash125", "MGidr", "MktRet", "MktVol", "MktIll")], frequency = 1)
logistic_model <- glm(crash125 ~ MGidr + MktRet + MktVol + MktIll, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)



ts_data <- ts(df[, c("crash15", "MGidr")], frequency = 1)
logistic_model <- glm(crash15 ~ MGidr, data = as.data.frame(ts_data), family = binomial(link = "logit"))
summary(logistic_model)
PseudoR2(logistic_model)
