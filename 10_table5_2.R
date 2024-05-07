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

### PANEL A ### - 3 groups

order_and_group <- function(input_df, k) {
  # Order rows by the 5th column
  ordered_df <- input_df[order(input_df[, 4]), ]
  
  # Calculate number of rows per subgroup
  rows_per_group <- nrow(ordered_df) %/% k  # Integer division
  remainder <- nrow(ordered_df) %% k  # Remainder after integer division
  
  # Initialize result dataframes
  result_df1 <- data.frame(matrix(NA, nrow = k, ncol = 5))
  colnames(result_df1) <- c("Count", "Mean_MG", "Mean_WML", "Mean_WML_long", "Mean_WML_short")
  
  result_df2 <- data.frame(matrix(NA, nrow = k, ncol = 3))
  colnames(result_df2) <- c("tstat_meanWML", "tstat_WMLlong", "tstat_WMLshort")
  
  # Populate result dataframes
  for (i in 1:k) {
    start <- (i - 1) * rows_per_group + 1 + min(i - 1, remainder)  # Adjust start index
    end <- i * rows_per_group + min(i, remainder)  # Adjust end index
    subgroup <- ordered_df[start:end, ]
    
    # Result dataframe 1
    result_df1[i, "Count"] <- nrow(subgroup)
    result_df1[i, "Mean_MG"] <- mean(subgroup[, 4])
    result_df1[i, c("Mean_WML", "Mean_WML_long", "Mean_WML_short")] <- colMeans(subgroup[, 1:3])
    
    # Result dataframe 2
    t_stats <- apply(subgroup[, 1:3], 2, function(x) t.test(x, mu = 0)$statistic)
    result_df2[i, ] <- t_stats
  }
  
  return(list(result_df1, result_df2))
}

order_and_group(input, k = 3)

write.csv(order_and_group(input, k = 3), file = "table5_PA.csv", row.names = TRUE)


#order_and_group_neweywest(df, k = 3)

### PANEL B ### - 5 groups

order_and_group(input, k = 5)

write.csv(order_and_group(input, k = 5), file = "table5_PB.csv", row.names = TRUE)


### PANEL C ### 10 groups

order_and_group(input, k = 10)

write.csv(order_and_group(input, k = 10), file = "table5_PC.csv", row.names = TRUE)




#### probalkozasok ####

order_and_group2 <- function(input_df, k) {
  # Order rows by the 5th column
  ordered_df <- input_df[order(input_df[, 4]), ]
  
  # Calculate number of rows per subgroup
  rows_per_group <- nrow(ordered_df) %/% k  # Integer division
  remainder <- nrow(ordered_df) %% k  # Remainder after integer division
  
  # Initialize result dataframes
  result_df1 <- data.frame(matrix(NA, nrow = k, ncol = 5))
  colnames(result_df1) <- c("Count", "Mean_MG", "Mean_WML", "Mean_WML_long", "Mean_WML_short")
  
  result_df2 <- data.frame(matrix(NA, nrow = k, ncol = 3))
  colnames(result_df2) <- c("tstat_meanWML", "tstat_WMLlong", "tstat_WMLshort")
  
  # Initialize list to store all subgroups
  subgroup_list <- list()
  
  # Populate result dataframes
  for (i in 1:k) {
    start <- (i - 1) * rows_per_group + 1 + min(i - 1, remainder)  # Adjust start index
    end <- i * rows_per_group + min(i, remainder)  # Adjust end index
    subgroup <- ordered_df[start:end, ]
    
    # Store subgroup
    subgroup_list[[i]] <- subgroup
    
    # Result dataframe 1
    result_df1[i, "Count"] <- nrow(subgroup)
    result_df1[i, "Mean_MG"] <- mean(subgroup[, 4])
    result_df1[i, c("Mean_WML", "Mean_WML_long", "Mean_WML_short")] <- colMeans(subgroup[, 1:3])
    
    # Result dataframe 2
    t_stats <- apply(subgroup[, 1:3], 2, function(x) t.test(x, mu = 0)$statistic)
    result_df2[i, ] <- t_stats
  }
  
  # Add subgroups to the result list
  result_list <- list(result_df1, result_df2, subgroup_list)
  
  return(result_list)
}

op <- order_and_group2(input, k = 3)

t_stats2 <- apply(op[[3]][[5]]$WML-op[[3]][[1]]$WML, 2, function(x) t.test(x, mu = 0)$statistic)

length(op[[3]][[1]]$WML)
op[[3]][[5]]$WML
valt1 <- op[[3]][[3]]$WML
#valt1[52] <- mean(valt1)
valt2 <- op[[3]][[1]]$WML
res <- valt2-valt1
t_stats2 <- apply(res, 2, function(x) t.test(x, mu = 0)$statistic)
t_test_result <- t.test(res)

### MG idr ###

order_and_group3 <- function(input_df, k) {
  # Order rows by the 7th column
  ordered_df <- input_df[order(input_df[, 7]), ]
  
  # Calculate number of rows per subgroup
  rows_per_group <- nrow(ordered_df) %/% k  # Integer division
  remainder <- nrow(ordered_df) %% k  # Remainder after integer division
  
  # Initialize result dataframes
  result_df1 <- data.frame(matrix(NA, nrow = k, ncol = 5))
  colnames(result_df1) <- c("Count", "Mean_MG", "Mean_WML", "Mean_WML_long", "Mean_WML_short")
  
  result_df2 <- data.frame(matrix(NA, nrow = k, ncol = 3))
  colnames(result_df2) <- c("tstat_meanWML", "tstat_WMLlong", "tstat_WMLshort")
  
  # Populate result dataframes
  for (i in 1:k) {
    start <- (i - 1) * rows_per_group + 1 + min(i - 1, remainder)  # Adjust start index
    end <- i * rows_per_group + min(i, remainder)  # Adjust end index
    subgroup <- ordered_df[start:end, ]
    
    # Result dataframe 1
    result_df1[i, "Count"] <- nrow(subgroup)
    result_df1[i, "Mean_MG"] <- mean(subgroup[, 7])
    result_df1[i, c("Mean_WML", "Mean_WML_long", "Mean_WML_short")] <- colMeans(subgroup[, 1:3])
    
    # Result dataframe 2
    t_stats <- apply(subgroup[, 1:3], 2, function(x) t.test(x, mu = 0)$statistic)
    result_df2[i, ] <- t_stats
  }
  
  return(list(result_df1, result_df2))
}
order_and_group3(input, k=3)
order_and_group3(input, k=5)
order_and_group3(input, k=10)


