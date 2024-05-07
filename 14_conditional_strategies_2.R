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

#install.packages("lmtest")

setwd("C:/Users/psamu/OneDrive/Documents/corvinus/_szakdoga/code")

### INPUT DATA ### -------------------------------------------------------------

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

### ----------------------------------------------------------------------------

#### Sharpe ratio #####

### RISK FREE RATE ### ---------------------------------------------------------

#daily t-bill returns
rf_rate <- read.csv("1m_TBill_14-24.csv", header = TRUE, sep = ";")
rf_rate$DATE <- as.Date(as.character(rf_rate$DATE), format = "%Y%m%d")
rf <- rf_rate$RF_RATE
names(rf) <- rf_rate$DATE
rf <- gsub(",", ".", rf)
rf <- as.numeric(rf)
names(rf) <- as.Date(rf_rate$DATE)

sum(rf)
## annual wml return
sum(df$WML) / 10

## annualized volatility
sd(df$WML) * sqrt(26)

# Sharpe 10 evre
Sharpe_alap <- (sum(df$WML) / 10 - sum(rf)) / (sd(df$WML) * sqrt(26))


### aranyositas - evek szama
wml_count <- length(df$WML)
wml_pred_count <- length(df$WML[31:length(df$WML)])
arany <- wml_pred_count / wml_count

evek_mintaban <- wml_pred_count / 26

rf2 <- rf
df$Date[30]

## keeping risk free rate only for investigated period
rf <- rf2[names(rf2) >= df$Date[30]]


#### calc MG quantiles  ###

## this function checks whether mom gap is ranked in the top quintile
calculate_groups <- function(vector, k) {
  groups <- NULL
  for (i in 30:length(vector)) {
    window <- vector[1:i-1]
    quantiles <- quantile(window, probs = seq(0, 1, by = 1/(k)))
    #print(quantiles)
    groups[i] <- findInterval(vector[i], quantiles[1:k]) } # Ensure values between 1 and k
  return(groups)
}

# jo
calculate_groups(df$MG, 5)

MG_investment <- ifelse(calculate_groups(df$MG, 5) == 5, 0, 1)

MG_investment[1:30] <- 0

wml_MGtop <- MG_investment * df$WML
wml_MGtop <- wml_MGtop[31:length(df$WML)]
wml_alap <- df$WML[31:length(df$WML)]

# sharpe ratak
Sharpe_alap <- (sum(wml_alap) / evek_mintaban - sum(rf)) / (sd(wml_alap) * sqrt(26))
Sharpe_MGtop <- (sum(wml_MGtop) / evek_mintaban - sum(rf)) / (sd(wml_MGtop) * sqrt(26))

### szignifikans e a kulonbseg?  ####

z1 = (Sharpe_MGtop - Sharpe_alap) / (sqrt(((sd(wml_alap) * sqrt(26))^2) / 223 + ((sd(wml_MGtop) * sqrt(26))^2) / 223))

### prediction ###

## MG ##

initial_window_size <- 30

# Initialize variables to store predictions and actual values
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


predictions_MG <- predictions

# vesztes predikciok
vesztes <- ifelse(predictions_MG < 0, 0, 1)
sum(vesztes)
length(vesztes)
df$WML[31:length(df$WML)]
df$Date[31:length(df$WML)]

wml_vesztes <- df$WML[31:length(df$WML)] * vesztes

Sharpe_alap <- (sum(df$WML[31:length(df$WML)]) / evek_mintaban - sum(rf)) / (sd(df$WML[31:length(df$WML)]) * sqrt(26))
Sharpe_MGvesztes <- (sum(wml_vesztes) / evek_mintaban - sum(rf)) / (sd(wml_vesztes) * sqrt(26))

z2 = (Sharpe_MGvesztes - Sharpe_alap) / (sqrt(((sd(wml_alap) * sqrt(26))^2) / 223 + ((sd(wml_vesztes) * sqrt(26))^2) / 223))
# ez nem szignifikans sharpe javulas


##### summary ####

print(Sharpe_alap)
print(Sharpe_MGtop)
print(Sharpe_MGvesztes)

skew(wml_alap)
skew(wml_MGtop)
skew(wml_vesztes)

z1
z2

sum(vesztes)
