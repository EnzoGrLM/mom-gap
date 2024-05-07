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


#install.packages("lmtest")

setwd("C:/Users/psamu/OneDrive/Documents/corvinus/_szakdoga/code/wml_futtatas_3")

######## reading data ##########

df_f1h1 <- read.csv("output_f1h1_3.csv", header = TRUE)
df_f1h2 <- read.csv("output_f1h2_3.csv", header = TRUE)
df_f1h3 <- read.csv("output_f1h3_3.csv", header = TRUE)
df_f1h4 <- read.csv("output_f1h4_3.csv", header = TRUE)
df_f2h1 <- read.csv("output_f2h1_3.csv", header = TRUE)
df_f2h2 <- read.csv("output_f2h2_3.csv", header = TRUE)
df_f2h3 <- read.csv("output_f2h3_3.csv", header = TRUE)
df_f2h4 <- read.csv("output_f2h4_3.csv", header = TRUE)
df_f3h1 <- read.csv("output_f3h1_3.csv", header = TRUE)
df_f3h2 <- read.csv("output_f3h2_3.csv", header = TRUE)
df_f3h3 <- read.csv("output_f3h3_3.csv", header = TRUE)
df_f3h4 <- read.csv("output_f3h4_3.csv", header = TRUE)
df_f4h1 <- read.csv("output_f4h1_3.csv", header = TRUE)
df_f4h2 <- read.csv("output_f4h2_3.csv", header = TRUE)
df_f4h3 <- read.csv("output_f4h3_3.csv", header = TRUE)
df_f4h4 <- read.csv("output_f4h4_3.csv", header = TRUE)

#### adjusting data #########

process_dataframe <- function(df) {
  df$weighting.date <- as.Date(df$weighting.date)
  rownames(df) <- df[, 1]
  df <- df[, -1]
  return(df)
}

df_f1h1 <- process_dataframe(df_f1h1)
df_f1h2 <- process_dataframe(df_f1h2)
df_f1h3 <- process_dataframe(df_f1h3)
df_f1h4 <- process_dataframe(df_f1h4)
df_f2h1 <- process_dataframe(df_f2h1)
df_f2h2 <- process_dataframe(df_f2h2)
df_f2h3 <- process_dataframe(df_f2h3)
df_f2h4 <- process_dataframe(df_f2h4)
df_f3h1 <- process_dataframe(df_f3h1)
df_f3h2 <- process_dataframe(df_f3h2)
df_f3h3 <- process_dataframe(df_f3h3)
df_f3h4 <- process_dataframe(df_f3h4)
df_f4h1 <- process_dataframe(df_f4h1)
df_f4h2 <- process_dataframe(df_f4h2)
df_f4h3 <- process_dataframe(df_f4h3)
df_f4h4 <- process_dataframe(df_f4h4)

#############################

#####

print(t.test(df_f4h2$wml, mu = 0)$statistic)
print(t.test(df_f4h2$wml, mu = 0)$estimate)

NW_tstat <- function(data) {
  
  lm_model <- lm(wml ~ 1, data = data)
  cov_matrix <- NeweyWest(lm_model)
  t_test_result <- coeftest(lm_model, vcov = cov_matrix)
  t_value <- t_test_result[, "t value"]
  
  return(t_value)
}

row1 <- c(mean(df_f1h1$wml), mean(df_f1h2$wml), mean(df_f1h3$wml), mean(df_f1h4$wml))
row2 <- c(NW_tstat(df_f1h1), NW_tstat(df_f1h2), NW_tstat(df_f1h3), NW_tstat(df_f1h4))
row3 <- c(mean(df_f2h1$wml), mean(df_f2h2$wml), mean(df_f2h3$wml), mean(df_f2h4$wml))
row4 <- c(NW_tstat(df_f2h1), NW_tstat(df_f2h2), NW_tstat(df_f2h3), NW_tstat(df_f2h4))
row5 <- c(mean(df_f3h1$wml), mean(df_f3h2$wml), mean(df_f3h3$wml), mean(df_f3h4$wml))
row6 <- c(NW_tstat(df_f3h1), NW_tstat(df_f3h2), NW_tstat(df_f3h3), NW_tstat(df_f3h4))
row7 <- c(mean(df_f4h1$wml), mean(df_f4h2$wml), mean(df_f4h3$wml), mean(df_f4h4$wml))
row8 <- c(NW_tstat(df_f4h1), NW_tstat(df_f4h2), NW_tstat(df_f4h3), NW_tstat(df_f4h4))

rnams <- c("F1", "t_F1", "F2", "t_F2", "F3", "t_F3", "F4", "t_F4")
cnams <- c("H1", "H2", "H3", "H4")

df <- t(data.frame(row1, row2, row3, row4, row5, row6, row7, row8))
rownames(df) <- rnams
colnames(df) <- cnams

write.csv(df, file = "table3_futtatas3.csv", row.names = TRUE)





# t stat possibilities

# Perform Newey-West corrected t-test
lm_model <- lm(wml ~ 1, data = df_f2h2)
cov_matrix <- NeweyWest(lm_model)
t_test_result <- coeftest(lm_model, vcov = cov_matrix)

#White standard error test
model <- lm(wml ~ 1, data = df_f2h2)  # Creating a simple model
white_se <- sqrt(diag(vcovHC(model, type = "HC1")))  # Computing White standard errors
t_stat <- coef(model) / white_se  # Computing t-statistic


