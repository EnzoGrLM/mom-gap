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

mean(input$WML)

### PANEL A ### moments

summary2  <- function(returns) {
  
  #valtozok szamlalasa
  valtozok <- ncol(returns)
  
  #output: n, mean, sd, skewness, kurtosis, min, max
  output <- data.frame(matrix(NA, nrow = 7, ncol = valtozok))
  rownames(output) <- c("n", "mean", "sd", "skew", "kurt", "min", "max")
  colnames(output) <- colnames(returns)
  
  #adding summary stats
  for (i in 1:valtozok) {
    output[1, i] <- nrow(returns)
    output[2, i] <- mean(returns[,i])
    output[3, i] <- sd(returns[,i])
    output[4, i] <- skew(returns[,i])
    output[5, i] <- describe(returns[,i])$kurt
    output[6, i] <- describe(returns[,i])$min
    #output[7, i] <- quantile(returns[,i], probs = 0.25)
    #output[8, i] <- quantile(returns[,i], probs = 0.50)
    #output[9, i] <- quantile(returns[,i], probs = 0.75)
    output[7, i] <- describe(returns[,i])$max
  }
  
  return(output)
}

summary2(input)

write.csv(summary2(input), file = "table4_PA.csv", row.names = TRUE)

### PANEL B ### correlations

cor(input)

write.csv(cor(input), file = "table4_PB_1.csv", row.names = TRUE)

display_pvalue_matrix <- function(input_df) {
  # Initialize an empty matrix to store p-values
  pvalue_matrix <- matrix(NA, nrow = ncol(input_df), ncol = ncol(input_df))
  
  # Compute the p-values for each pair of variables
  for (i in 1:(ncol(input_df) - 1)) {
    for (j in (i + 1):ncol(input_df)) {
      correlation_test <- cor.test(input_df[, i], input_df[, j])
      pvalue_matrix[i, j] <- pvalue_matrix[j, i] <- correlation_test$p.value
    }
  }
  
  # Set column and row names of the matrix
  colnames(pvalue_matrix) <- rownames(pvalue_matrix) <- colnames(input_df)
  
  # Return the matrix of p-values
  return(pvalue_matrix)
}

display_pvalue_matrix(input)

write.csv(display_pvalue_matrix(input), file = "table4_PB_2.csv", row.names = TRUE)

## sztem csillagozni fogom a szign szinteket

### PANEL C ### year-by-year averages

aggregate_by_year <- function(input_df) {
  # Extract unique years from row names
  unique_years <- unique(as.integer(format(as.Date(rownames(input_df)), "%Y")))
  
  # Initialize an empty dataframe to store the results
  result_df <- data.frame(matrix(NA, nrow = length(unique_years), ncol = ncol(input_df)))
  
  # Set row names of result dataframe as unique years
  rownames(result_df) <- unique_years
  
  # Iterate over each unique year
  for (year in unique_years) {
    # Subset the input dataframe for the current year
    subset_df <- input_df[format(as.Date(rownames(input_df)), "%Y") == year, ]
    
    # Calculate the mean for each column and store it in the result dataframe
    result_df[as.character(year), ] <- colMeans(subset_df, na.rm = TRUE)
  }
  
  colnames(result_df) <- colnames(input_df)
  
  return(result_df)
}

aggregate_by_year(input)

write.csv(aggregate_by_year(input), file = "table4_PC.csv", row.names = TRUE)
