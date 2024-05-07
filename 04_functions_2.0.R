#### packages and wd ###

load_packages()
setwd("C:/Users/psamu/OneDrive/Documents/corvinus/_szakdoga/code")

######## reading and format data ##########-------------------------------------

prices_df <- hist_read_format("coin_prices.csv")
mcap_df <- hist_read_format("coin_mcap.csv")
vol_df <- hist_read_format("coin_vol.csv")

stable_list <- stable_read("stablecoins.csv")

#### parameters ####------------------------------------------------------------

parameters <- f_parameters(mcaplimUSD = 1000000, missing_num = 0, 
                               stablecoins = stable_list, winsorize = 0.01,
                               value_weighted = TRUE, num_portfolios = 5)

periods <- period_lengths_days(form = 21, lag = 0, hold = 7, reb = 7)
num_form <- number_of_formations(start = "2014-01-01", end = "2023-12-31")
loop_output <- loop_output_df()

#### for loop ####--------------------------------------------------------------

for (i in 1:num_form$formations_count) {

  #calculating dates
  dates <- loop_dates(i)
  
  #levalogatas
  prices <- levalogatas(prices_df)
  mcap <- levalogatas(mcap_df)
  vol <- levalogatas(vol_df)
  
  #adatszures
  szurt_prices <- adatszures(prices, mcap, vol)
  prices_form <- szurt_prices$formation_prices
  prices_hold <- szurt_prices$holding_prices
  
  #signal generalas
  signal <- signal_generalas(prices_form)
  
  #percentiles for MG
  mg <- mg_calculation_1(signal)
  
  #weights
  weights_all <- sorting(signal, mcap_df)
  weights <- weights_all$wml_weights
  weights_quantiles <- weights_all$quantile_weights
  
  #hpr
  hpr <- holding_returns(prices_hold, weights)
  
  #wml
  wml <- wml_return(weights, hpr)
  wml_short <- wml_short_return(weights, hpr)
  wml_long <- wml_long_return(weights, hpr)
  
  ret_qntls <- quantile_ret(weights_quantiles, hpr)
  
  #saving results
  loop_output[i, 1] <- dates$weighting_date
  loop_output[i, 2] <- length(weights)
  loop_output[i, 3] <- wml
  loop_output[i, 4] <- ret_qntls[1]
  loop_output[i, 5] <- ret_qntls[2]
  loop_output[i, 6] <- ret_qntls[3]
  loop_output[i, 7] <- ret_qntls[4]
  loop_output[i, 8] <- ret_qntls[5]
  loop_output[i, 9:15] <- mg
  
  #how far in the loop?
  print(wml)
  #print(weights)
  print(paste(i, "out of", num_form$formations_count, "done"))
}


################################################--------------------------------
################# FUNCTIONS ####################
################################################--------------------------------

#packages
load_packages <-function(){
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
  library(sandwich)
  library(lmtest)
}
#beolvasas es formattalas
hist_read_format <- function(df_name){
  
  prices_df <- read.csv(df_name, header = TRUE)
  prices_df$timestamp <- as.Date(prices_df$timestamp)
  prices_df <- head(prices_df, -2)
  rnames <- as.Date(prices_df$timestamp)
  rownames(prices_df) <- rnames
  prices_df <- subset(prices_df, select = -timestamp)
  
  return(prices_df)
}
#stable list creation
stable_read <- function(stable_file_name){
  stable <- read.csv(stable_file_name, header = FALSE)
  stable_list <- unlist(stable[, 1], use.names = FALSE)
}
#parameters
f_parameters <- function(mcaplimUSD, missing_num, stablecoins, winsorize, value_weighted, num_portfolios){
  return(list(mcap_limit = mcaplimUSD, missing_limit = missing_num, 
              stablecoins = stablecoins, winsorize_level = winsorize,
              val_weighted = value_weighted, num_portfolios = num_portfolios))
}
#input lengths
period_lengths_days <- function(form, lag, hold, reb){
  return(list(formation = form, lag = lag, holding = hold, rebalancing = reb))
}
#number of formations and start of sample
number_of_formations <- function(start, end) {
  
  start_of_last_formation <- as.Date(end) - periods$formation - periods$holding
  days_difference <- as.numeric(start_of_last_formation - as.Date(start))
  num_formations <- floor(days_difference / (as.numeric(periods$rebalancing)))
  sample_start <- as.Date(start)
  
  return(list(formations_count = num_formations, sample_start = sample_start))
}
#loop output dataframe  #update if number of portfolios is other than 5 !!!!!!!
loop_output_df <- function(){
  output <- data.frame(matrix(ncol = 15, nrow = num_form$formations_count))
  names(output) <- c("weighting date", "number of assets", "wml", "1", "2", "3", "4", "5", "5th", "10th",
                     "25th", "50th", "75th", "90th", "95th")
  
  return(output)
}
#dates 
loop_dates <- function(i){
  form_start <- num_form$sample_start + (i-1) * periods$rebalancing
  weighting_date <- form_start + periods$formation
  form_end <- form_start + periods$formation - periods$lag
  hpr_start <- weighting_date + 1
  hpr_end <- hpr_start + periods$holding
  
  return(list(form_start = form_start, form_end = form_end, weighting_date = weighting_date, 
         hpr_start = hpr_start, hpr_end = hpr_end))
}
#levalogatas
levalogatas <- function(df) {
  filtered_df <- df %>%
    filter(as.Date(rownames(df)) >= as.Date(dates$form_start) & 
             as.Date(rownames(df)) <= as.Date(dates$hpr_end))
  return(filtered_df)
}
#adatszures
adatszures <- function(prices, mcaps, vols) {
  
  #parameters
  mcap_limit <- parameters$mcap_limit
  missing_limit <- parameters$missing_limit
  stablecoins <- parameters$stablecoins
  
  #stablecoins
  prices_og <- prices %>%
    select(-any_of(stablecoins))
  mcaps_og <- mcaps %>%
    select(-any_of(stablecoins))
  vols_og <- vols %>%
    select(-any_of(stablecoins))
  
  #storing og dataframes
  prices <- prices_og %>%
    filter(as.Date(rownames(prices_og)) <= dates$weighting_date)
  mcaps <- mcaps_og %>%
    filter(as.Date(rownames(mcaps_og)) <= dates$weighting_date)
  vols <- vols_og %>%
    filter(as.Date(rownames(vols_og)) <= dates$weighting_date)
  
  ### filtering only for formation period:
  
  # missing values in prices, mcap and volatility
  filtered_prices <- prices %>%
    select(-which(colSums(is.na(.)) > missing_limit))
  filtered_mcap <- mcaps %>%
    select(-which(colSums(is.na(.)) > missing_limit))
  filtered_vol <- vols %>%
    select(-which(colSums(is.na(.)) > missing_limit))
  
  #updating dataframes
  p_symb <- colnames(filtered_prices)
  m_symb <- colnames(filtered_mcap)
  v_symb <- colnames(filtered_vol)
  symb_stays <- Reduce(intersect, list(p_symb, m_symb, v_symb))
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
  
  #calculating output
  prices_form <- prices %>%
    filter(as.Date(rownames(prices)) >= as.Date(dates$form_start) & 
             as.Date(rownames(prices)) <= as.Date(dates$form_end))
  
  prices_holding <- prices %>%
    filter(as.Date(rownames(prices)) >= as.Date(dates$hpr_start) & 
             as.Date(rownames(prices)) <= as.Date(dates$hpr_end))
  
  return(list(formation_prices = prices_form, holding_prices = prices_holding))
}
#sign generalas
signal_generalas <- function(prices) {
  
  #parameters
  winsorize <- parameters$winsorize_level
  
  #daily log ret
  shifted_prices <- prices[-nrow(prices), ]
  lagged_prices <- prices[-1, ]
  log_returns <- log(lagged_prices) - log(shifted_prices)
  rownames(log_returns) <- rownames(shifted_prices)
  colnames(log_returns) <- colnames(prices)
  
  #winsorizing
  #log_returns_winsorized <- apply(log_returns, 2, winsorizing3, winsorize = 0.01)
  #log_returns_winsorized <- as.data.frame(log_returns_winsorized)
  #rownames(log_returns_winsorized) <- rownames(log_returns)
  #colnames(log_returns_winsorized) <- colnames(log_returns)
  
  #cum returns
  result <- colSums(log_returns)

  result <- winsorizing3(result, winsorize)
  
  #sorting returns
  #result_sorted <- data.frame(t(apply(result, 1, sort)))
  #result_sorted <- (t(apply(result, 1, sort)))
  
  result_sorted <- sort(result, decreasing = FALSE)
  
  return(result_sorted)
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
winsorizing3 <- function(x, winsorize) {
  # Replace missing values with the first percentile
  if(all(is.na(x) | is.nan(x))) {
    return(rep(quantile(x, probs = winsorize, na.rm = TRUE), length(x)))
  }
  
  # Replace missing values with the first percentile
  x_na <- x
  x_na[is.na(x_na) | is.nan(x_na)] <- quantile(x_na, probs = winsorize, na.rm = TRUE)
  
  # Compute winsorized values
  qntl <- quantile(x_na, probs = c(winsorize, 1 - winsorize), na.rm = TRUE)
  x_na[x_na < qntl[1]] <- qntl[1]
  x_na[x_na > qntl[2]] <- qntl[2]
  
  return(x_na)
}
#percentiles
mg_calculation_1 <- function(signal) {
  
  percentiles <- quantile(signal, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
  
  col_names <- c("5th", "10th", "25th", "50th", "75th", "90th", "95th")
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
# numbers for helps
classify_values <- function(input_vector, limits_vector) {
  
  output_vector <- integer(length(input_vector)) # Initialize output vector
  names(output_vector) <- names(input_vector)
  
  for (i in 1:length(input_vector)) {
    value <- input_vector[i]
    # Compare value with each limit
    for (j in 1:(length(limits_vector)-1)) {
      if (value <= limits_vector[1]) {
        output_vector[i] <- 1
        break
      } else if (value >= limits_vector[length(limits_vector)]) {
        output_vector[i] <- length(limits_vector) + 1
        break
      } else if (value > limits_vector[j] && value <= limits_vector[j+1]) {
        output_vector[i] <- j + 1
        break
      }
    }
  }
  return(output_vector)
}
#portfolios
sorting <- function(signal, mcap) {
  
  #parameters
  val_weighted <- parameters$val_weighted
  num_portfolios <- parameters$num_portfolios
  
  #portfolio breakpoints
  limits <- calculate_percentiles(signal, num_portfolios)
  
  if (num_portfolios == 3) {
    limit_lower <- quantile(signal, probs = 0.3)
    limit_upper <- quantile(signal, probs = 0.7)
  } else {
    limit_lower <- limits[2]
    limit_upper <- limits[num_portfolios]
  }
  
  portfolios_mom <- mom_portf(signal, limit_lower, limit_upper)
  
  limits[2] <- limit_lower
  limits[length(limits)-1]
  limits <- limits[-c(1, length(limits))]
  
  help_vector <- classify_values(signal, limits)
  
  #getting mcap
  coins <- names(signal)
  mcaps <- mcap %>%
    filter(rownames(mcap) == dates$weighting_date) %>%
    select(one_of(coins))
  
  #handling missing values
  mcaps[is.na(mcaps)] <- 1
  
  #### WML weights ####
  
  #mcap weights
  df <- data.frame(Market_Cap = t(mcaps), Value = (portfolios_mom))
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
  #
  value_weights <- as.vector(portfolios_mom) * as.vector(val_weights)
  names(value_weights) <- coins
  equal_weights <- as.vector(portfolios_mom) * as.vector(eq_weights)
  names(equal_weights) <- coins
  
  ##############
  
  #### quantile weights ####
  
  #mcap weights
  df <- data.frame(Market_Cap = t(mcaps), Value = (help_vector))
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
  
  output_ew <- as.data.frame(matrix(NA, nrow = length(coins), ncol = length(unique(df$Value))))
  output_vw <- as.data.frame(matrix(NA, nrow = length(coins), ncol = length(unique(df$Value))))
  colnames(output_ew) <- 1:length(unique(df$Value))
  colnames(output_vw) <- 1:length(unique(df$Value))
  rownames(output_ew) <- coins
  rownames(output_vw) <- coins
  
  for (i in 1:length(coins)) {
    for (j in 1:length(unique(df$Value)))
      if (df$Value[i] == j) {
        output_ew[i, j] = df$One_By_N[i]
      } else {
        output_ew[i, j] = 0
      }
  }
  
  for (i in 1:length(coins)) {
    for (j in 1:length(unique(df$Value)))
      if (df$Value[i] == j) {
        output_vw[i, j] = df$Weighted_Market_Cap[i]
      } else {
        output_vw[i, j] = 0
      }
  }
  
  #
  if (val_weighted) {
    return(list(wml_weights = value_weights, quantile_weights = output_vw))
  } else {
    return(list(wml_weights = equal_weights, quantile_weights = output_ew))
  }
  
  #return(df)
  
}
#holding period returns
holding_returns <- function(prices, weights) {
  
  coins <- names(weights)
  
  shifted_prices <- prices[-nrow(prices), ]
  lagged_prices <- prices[-1, ]
  log_returns <- log(lagged_prices) - log(shifted_prices)
  rownames(log_returns) <- rownames(shifted_prices)
  colnames(log_returns) <- colnames(prices)
  
  #winsorizing
  #log_returns_winsorized <- apply(log_returns, 2, winsorizing3, winsorize = 0.01)
  #log_returns_winsorized <- as.data.frame(log_returns_winsorized)
  #rownames(log_returns_winsorized) <- rownames(log_returns)
  #colnames(log_returns_winsorized) <- colnames(log_returns)
  
  #cum returns
  result <- colSums(log_returns) / (periods$holding / 7)
  #names(result) <- coins
  result <- winsorizing3(result, parameters$winsorize_level)
  
  first_percentile <- quantile(result, probs = 0.01, na.rm = TRUE)
  result[is.na(result)] <- first_percentile
  result[is.nan(result)] <- first_percentile
  result[result == -Inf] <- first_percentile
  ninety_ninth_percentile <- quantile(result, probs = 0.99, na.rm = TRUE)
  result[result == Inf] <- ninety_ninth_percentile
  
  result <- result[coins]
  
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
#quantile returns
quantile_ret <- function(weights, holding_period_return) {
  
  coins <- colnames(weights)
  
  result <- colSums(weights * holding_period_return)
  
  names(result) <- coins
  
  return(result)
}



