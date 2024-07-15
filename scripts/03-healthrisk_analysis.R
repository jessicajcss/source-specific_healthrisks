
#############################################################################
#############################################################################
# # The Health Risk Analysis

### Date: 09/11/2020 | Last update: 07/2024
### By: Jéssica C. dos Santos-Silva

# # Associated paper:  A fingerprint of source-specific health risk of PM2.5-bound components over a coastal industrial city
#############################################################################
#############################################################################


## References ---- 
# Adapted from:
# Requia, W. J.; Amini, H.; Mukherjee, R.; Gold, D. R.; Schwartz, J. D.
# Nat Commun 2021, https://doi.org/10.1038/s41467-021-26822-7.


##############################################
############### LOAD PACKAGES ################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(survival)

##############################################
################## The data ##################
##############################################

source("./scripts/00-basic_functions.R")

#source("./scripts/02-preparing_healthrisk_analysis.R") # OR #

final_data_3 <- read_csv("./output/03-data_crossanalysis.csv")

summary(final_data_3)


### Part 6 - Statistical analyses ----
####################################


# Health outcome - Select the options below
#########################################################################
# RUN ONCE !!!

results.temp <- data.frame() # To Save results - run once before start


#!!! SEE Line 460 for: Set the strata id, then rename output file

############################################################################
############################################################################
############################################################################


for (portion in 1:63) {
  if (portion >= 1 & portion <= 7) {
    # Not grouped [Primary model]
    outcome = "Cardiorespiratory"
    sex = "All"
    analysis = 'Primary model'
    data_site <- final_data_3
    
  } else if (portion >= 8 & portion <= 14) {
    # Grouped by sex [Sensitivity model]
    outcome = "Cardiorespiratory"
    sex = "Male"
    analysis = 'Sensitivity model'
    data_site <- final_data_3 %>%
      filter(SEX == sex)
  } else if (portion >= 15 & portion <= 21) {
    outcome = "Cardiorespiratory"
    sex = "Female"
    analysis = 'Sensitivity model'
    data_site <- final_data_3 %>%
      filter(SEX == sex)
  } else if (portion >= 22 & portion <= 28) {
    # Grouped only by CID [Primary model]
    outcome = "Respiratory"
    sex = "All"
    analysis = 'Primary model'
    data_site <- final_data_3 %>%
      filter(CID == outcome)
  } else if (portion >= 29 & portion <= 35) {
    # Grouped only by CID and SEX [Sensitivity model]
    outcome = "Respiratory"
    sex = "Female"
    analysis = 'Sensitivity model'
    data_site <- final_data_3 %>%
      filter(CID == outcome,
             SEX == sex) 
  } else if (portion >= 36 & portion <= 42) {
    outcome = "Respiratory"
    sex = "Male"
    analysis = 'Sensitivity model'
    data_site <- final_data_3 %>%
      filter(CID == outcome,  
             SEX == sex) 
  } else if (portion >= 43 & portion <= 49) {
    # Grouped only by CID [Primary model]
    outcome = "Circulatory"
    sex = "All"
    analysis = 'Primary model'
    data_site <- final_data_3 %>%
      filter(CID == outcome) 
  } else if (portion >= 50 & portion <= 56) {
    # Grouped only by CID and SEX [Sensitivity model]
    outcome = "Circulatory"
    sex = "Female"
    analysis = 'Sensitivity model'
    data_site <- final_data_3 %>%
      filter(CID == outcome,  
             SEX == sex)   
  } else if (portion >= 57 & portion <= 63) {
    outcome = "Circulatory"
    sex = "Male"
    analysis = 'Sensitivity model'
    data_site <- final_data_3 %>%
      filter(CID == outcome,  
             SEX == sex) 
  }
  
  
  
## Moving averages ----
for (j in 0:4) {
  if (j == 0) {
    coluna = "_avg_1day"
    lag = "1-day moving average"
  } else if (j == 1) {
    coluna = "_avg_2days"
    lag = "2-day moving average" 
  } else if (j == 2) {
    coluna = "_avg_3days"
    lag = "3-day moving average" 
  } else if (j == 3) {
    coluna = "_avg_4days"
    lag = "4-day moving average" 
  } else if (j == 4) {
    coluna = "_avg_5days"
    lag = "5-day moving average" 
  }
  

  # PM25 (1 day)
  vars <- c("PM2.5", "Lag1_PM25")
  data_site$PM25_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # PM25 (2 days)
  vars <- c("PM2.5", "Lag1_PM25", "Lag2_PM25")
  data_site$PM25_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # PM25 (3 days)
  vars <- c("PM2.5", "Lag1_PM25", "Lag2_PM25", "Lag3_PM25")
  data_site$PM25_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # PM25 (4 days)
  vars <- c("PM2.5", "Lag1_PM25", "Lag2_PM25", "Lag3_PM25", "Lag4_PM25")
  data_site$PM25_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # PM25 (5 days)
  vars <- c("PM2.5", "Lag1_PM25", "Lag2_PM25", "Lag3_PM25", "Lag4_PM25", "Lag5_PM25")
  data_site$PM25_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  
  # F1 (1 day)
  vars <- c("Factor.1", "Lag1_F1")
  data_site$F1_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F1 (2 days)
  vars <- c("Factor.1", "Lag1_F1", "Lag2_F1")
  data_site$F1_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F1 (3 days)
  vars <- c("Factor.1", "Lag1_F1", "Lag2_F1", "Lag3_F1")
  data_site$F1_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F1 (4 days)
  vars <- c("Factor.1", "Lag1_F1", "Lag2_F1", "Lag3_F1", "Lag4_F1")
  data_site$F1_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F1 (5 days)
  vars <- c("Factor.1", "Lag1_F1", "Lag2_F1", "Lag3_F1", "Lag4_F1", "Lag5_F1")
  data_site$F1_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  
  # F2 (1 day)
  vars <- c("Factor.2", "Lag1_F2")
  data_site$F2_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F2 (2 days)
  vars <- c("Factor.2", "Lag1_F2", "Lag2_F2")
  data_site$F2_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F2 (3 days)
  vars <- c("Factor.2", "Lag1_F2", "Lag2_F2", "Lag3_F2")
  data_site$F2_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F2 (4 days)
  vars <- c("Factor.2", "Lag1_F2", "Lag2_F2", "Lag3_F2", "Lag4_F2")
  data_site$F2_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F2 (5 days)
  vars <- c("Factor.2", "Lag1_F2", "Lag2_F2", "Lag3_F2", "Lag4_F2", "Lag5_F2")
  data_site$F2_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  # F3 (1 day)
  vars <- c("Factor.3", "Lag1_F3")
  data_site$F3_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F3 (2 days)
  vars <- c("Factor.3", "Lag1_F3", "Lag2_F3")
  data_site$F3_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F3 (3 days)
  vars <- c("Factor.3", "Lag1_F3", "Lag2_F3", "Lag3_F3")
  data_site$F3_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F3 (4 days)
  vars <- c("Factor.3", "Lag1_F3", "Lag2_F3", "Lag3_F3", "Lag4_F3")
  data_site$F3_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F3 (5 days)
  vars <- c("Factor.3", "Lag1_F3", "Lag2_F3", "Lag3_F3", "Lag4_F3", "Lag5_F3")
  data_site$F3_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  
  # F4 (1 day)
  vars <- c("Factor.4", "Lag1_F4")
  data_site$F4_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F4 (2 days)
  vars <- c("Factor.4", "Lag1_F4", "Lag2_F4")
  data_site$F4_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F4 (3 days)
  vars <- c("Factor.4", "Lag1_F4", "Lag2_F4", "Lag3_F4")
  data_site$F4_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F4 (4 days)
  vars <- c("Factor.4", "Lag1_F4", "Lag2_F4", "Lag3_F4", "Lag4_F4")
  data_site$F4_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F4 (5 days)
  vars <- c("Factor.4", "Lag1_F4", "Lag2_F4", "Lag3_F4", "Lag4_F4", "Lag5_F4")
  data_site$F4_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  
  
  # F5 (1 day)
  vars <- c("Factor.5", "Lag1_F5")
  data_site$F5_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F5 (2 days)
  vars <- c("Factor.5", "Lag1_F5", "Lag2_F5")
  data_site$F5_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F5 (3 days)
  vars <- c("Factor.5", "Lag1_F5", "Lag2_F5", "Lag3_F5")
  data_site$F5_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F5 (4 days)
  vars <- c("Factor.5", "Lag1_F5", "Lag2_F5", "Lag3_F5", "Lag4_F5")
  data_site$F5_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F5 (5 days)
  vars <- c("Factor.5", "Lag1_F5", "Lag2_F5", "Lag3_F5", "Lag4_F5", "Lag5_F5")
  data_site$F5_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  
  # F6 (1 day)
  vars <- c("Factor.6", "Lag1_F6")
  data_site$F6_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F6 (2 days)
  vars <- c("Factor.6", "Lag1_F6", "Lag2_F6")
  data_site$F6_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F6 (3 days)
  vars <- c("Factor.6", "Lag1_F6", "Lag2_F6", "Lag3_F6")
  data_site$F6_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F6 (4 days)
  vars <- c("Factor.6", "Lag1_F6", "Lag2_F6", "Lag3_F6", "Lag4_F6")
  data_site$F6_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F6 (5 days)
  vars <- c("Factor.6", "Lag1_F6", "Lag2_F6", "Lag3_F6", "Lag4_F6", "Lag5_F6")
  data_site$F6_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  
  
  # F7 (1 day)
  vars <- c("Factor.7", "Lag1_F7")
  data_site$F7_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F7 (2 days)
  vars <- c("Factor.7", "Lag1_F7", "Lag2_F7")
  data_site$F7_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F7 (3 days)
  vars <- c("Factor.7", "Lag1_F7", "Lag2_F7", "Lag3_F7")
  data_site$F7_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F7 (4 days)
  vars <- c("Factor.7", "Lag1_F7", "Lag2_F7", "Lag3_F7", "Lag4_F7")
  data_site$F7_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # F7 (5 days)
  vars <- c("Factor.7", "Lag1_F7", "Lag2_F7", "Lag3_F7", "Lag4_F7", "Lag5_F7")
  data_site$F7_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  # RH (1 day)
  vars <- c("umid", "Lag1_RH")
  data_site$RH_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # RH (2 days)
  vars <- c("umid", "Lag1_RH", "Lag2_RH")
  data_site$RH_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # RH (3 days)
  vars <- c("umid", "Lag1_RH", "Lag2_RH", "Lag3_RH")
  data_site$RH_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # RH (4 days)
  vars <- c("umid", "Lag1_RH", "Lag2_RH", "Lag3_RH", "Lag4_RH")
  data_site$RH_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # RH (5 days)
  vars <- c("umid", "Lag1_RH", "Lag2_RH", "Lag3_RH", "Lag4_RH", "Lag5_RH")
  data_site$RH_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  
  # TEMP (1 day)
  vars <- c("temp", "Lag1_TEMP")
  data_site$TEMP_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # TEMP (2 days)
  vars <- c("temp", "Lag1_TEMP", "Lag2_TEMP")
  data_site$TEMP_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # TEMP (3 days)
  vars <- c("temp", "Lag1_TEMP", "Lag2_TEMP", "Lag3_TEMP")
  data_site$TEMP_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # TEMP (4 days)
  vars <- c("temp", "Lag1_TEMP", "Lag2_TEMP", "Lag3_TEMP", "Lag4_TEMP")
  data_site$TEMP_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # TEMP (5 days)
  vars <- c("temp", "Lag1_TEMP", "Lag2_TEMP", "Lag3_TEMP", "Lag4_TEMP", "Lag5_TEMP")
  data_site$TEMP_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  
  # PREC (1 day)
  vars <- c("prec", "Lag1_PREC")
  data_site$PREC_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # PREC (2 days)
  vars <- c("prec", "Lag1_PREC", "Lag2_PREC")
  data_site$PREC_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # PREC (3 days)
  vars <- c("prec", "Lag1_PREC", "Lag2_PREC", "Lag3_PREC")
  data_site$PREC_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # PREC (4 days)
  vars <- c("prec", "Lag1_PREC", "Lag2_PREC", "Lag3_PREC", "Lag4_PREC")
  data_site$PREC_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # PREC (5 days)
  vars <- c("prec", "Lag1_PREC", "Lag2_PREC", "Lag3_PREC", "Lag4_PREC", "Lag5_PREC")
  data_site$PREC_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  
 
  # WS (1 day)
  vars <- c("ws", "Lag1_WS")
  data_site$WS_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # WS (2 days)
  vars <- c("ws", "Lag1_WS", "Lag2_WS")
  data_site$WS_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # WS (3 days)
  vars <- c("ws", "Lag1_WS", "Lag2_WS", "Lag3_WS")
  data_site$WS_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # WS (4 days)
  vars <- c("ws", "Lag1_WS", "Lag2_WS", "Lag3_WS", "Lag4_WS")
  data_site$WS_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  # WS (5 days)
  vars <- c("ws", "Lag1_WS", "Lag2_WS", "Lag3_WS", "Lag4_WS", "Lag5_WS")
  data_site$WS_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  
  
  
  # WD (1 day)

  data_site$uwind <- u.wind(data_site$ws, data_site$wd)
  data_site$vwind <- v.wind(data_site$ws, data_site$wd)
  
  data_site$Lag1_uwind <- u.wind(data_site$Lag1_WS, data_site$Lag1_WD)
  data_site$Lag1_vwind <- v.wind(data_site$Lag1_WS, data_site$Lag1_WD)
  
  vars <- c("uwind", "Lag1_uwind")
  data_site$u_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  vars <- c("vwind", "Lag1_vwind")
  data_site$v_avg_1day <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  data_site$WD_avg_1day <- wind_direction(data_site$u_avg_1day, data_site$v_avg_1day)
  
  
  # WD (2 days)

  data_site$Lag2_uwind <- u.wind(data_site$Lag2_WS, data_site$Lag2_WD)
  data_site$Lag2_vwind <- v.wind(data_site$Lag2_WS, data_site$Lag2_WD)
  
  vars <- c("uwind", "Lag1_uwind", "Lag2_uwind")
  data_site$u_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  vars <- c("vwind", "Lag1_vwind", "Lag2_vwind")
  data_site$v_avg_2days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  data_site$WD_avg_2days <- wind_direction(data_site$u_avg_2days, data_site$v_avg_2days)
  
  
  # WD (3 days)
  
  data_site$Lag3_uwind <- u.wind(data_site$Lag3_WS, data_site$Lag3_WD)
  data_site$Lag3_vwind <- v.wind(data_site$Lag3_WS, data_site$Lag3_WD)
  
  vars <- c("uwind", "Lag1_uwind", "Lag2_uwind", "Lag3_uwind")
  data_site$u_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  vars <- c("vwind", "Lag1_vwind", "Lag2_vwind", "Lag3_vwind")
  data_site$v_avg_3days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  data_site$WD_avg_3days <- wind_direction(data_site$u_avg_3days, data_site$v_avg_3days)
  
  
  
  # WD (4 days)
  
  data_site$Lag4_uwind <- u.wind(data_site$Lag4_WS, data_site$Lag4_WD)
  data_site$Lag4_vwind <- v.wind(data_site$Lag4_WS, data_site$Lag4_WD)
  
  vars <- c("uwind", "Lag1_uwind", "Lag2_uwind", "Lag3_uwind", "Lag4_uwind")
  data_site$u_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  vars <- c("vwind", "Lag1_vwind", "Lag2_vwind", "Lag3_vwind", "Lag4_vwind")
  data_site$v_avg_4days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  data_site$WD_avg_4days <- wind_direction(data_site$u_avg_4days, data_site$v_avg_4days)
  
  
  
  # WD (5 days)
  
  data_site$Lag5_uwind <- u.wind(data_site$Lag5_WS, data_site$Lag5_WD)
  data_site$Lag5_vwind <- v.wind(data_site$Lag5_WS, data_site$Lag5_WD)
  
  vars <- c("uwind", "Lag1_uwind", "Lag2_uwind", "Lag3_uwind", "Lag4_uwind", "Lag5_uwind")
  data_site$u_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  vars <- c("vwind", "Lag1_vwind", "Lag2_vwind", "Lag3_vwind", "Lag4_vwind", "Lag5_vwind")
  data_site$v_avg_5days <- rowMeans(data_site[ , vars], na.rm = TRUE)
  
  data_site$WD_avg_5days <- wind_direction(data_site$u_avg_5days, data_site$v_avg_5days)
  
  
  
  
  
  # For the "factor"s variable, replace NaN values With NAs
  
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  
  data_site[is.nan(data_site)] <- NA
  
  
  
  # Health outcome - Set the strata id ----
  #########################################################################
  

  data_site <- data_site[order(data_site$date),]
  
  data_site$variable_indice_for_strata_id <- paste(data_site$site,"_",
                                                   #data_site$HOLIDAY,"_",
                                                   data_site$DOW,"_",
                                                   data_site$Month,"_",
                                                   #data_site$season,"_",
                                                   data_site$Year)
  # set config and file name
  strata_config = "site_DOW_MONTH_Year"
                                        
  data_site$strata_id <- data_site %>% 
    group_indices(variable_indice_for_strata_id)   # creating unique group IDs by day of week, month, and year.
  
  data_site <- data_site[order(data_site$strata_id),]
  
  
  
  # Set case (defined as "1")/control (defined as "0")
  ##################################################################
  data <- data_site #only to simplify

  for (i in portion) {
    
    if (i == 1 | i == 8 | i == 15 | i == 22 | i == 29 | i == 36 | i == 43 | i == 50 | i == 57) {
      source = "Mineral dust and soil ressuspension"
      quantile_F1 = as.numeric(quantile(data[[paste0("F1", coluna)]], c(.75), na.rm = T))
      data$case <- ifelse(data[[paste0("F1", coluna)]] >= quantile_F1, 1, 0)
      
    } else if (i == 2 | i == 9 | i == 16 | i == 23 | i == 30 | i == 37 | i == 44 | i == 51 | i == 58) {
      source = "Aged sea salt"
      quantile_F2 = as.numeric(quantile(data[[paste0("F2", coluna)]], c(.75), na.rm = T))
      data$case <- ifelse(data[[paste0("F2", coluna)]] >= quantile_F2, 1, 0)

    } else if (i == 3 | i == 10 | i == 17 | i == 24 | i == 31 | i == 38 | i == 45 | i == 52 | i == 59) {
      source = "Secondary nitrate"
      quantile_F3 = as.numeric(quantile(data[[paste0("F3", coluna)]], c(.75), na.rm = T))
      data$case <- ifelse(data[[paste0("F3", coluna)]] >= quantile_F3, 1, 0)
      
    } else if (i == 4 | i == 11 | i == 18 | i == 25 | i == 32 | i == 39 | i == 46 | i == 53 | i == 60) {
      source = "Fresh sea salt mixed with aged emissions"
      quantile_F4 = as.numeric(quantile(data[[paste0("F4", coluna)]], c(.75), na.rm = T))
      data$case <- ifelse(data[[paste0("F4", coluna)]] >= quantile_F4, 1, 0)
      
    } else if (i == 5 | i == 12 | i == 19 | i == 26 | i == 33 | i == 40 | i == 47 | i == 54 | i == 61) {
      source = "Industrial emissions"
      quantile_F5 = as.numeric(quantile(data[[paste0("F5", coluna)]], c(.75), na.rm = T))
      data$case <- ifelse(data[[paste0("F5", coluna)]] >= quantile_F5, 1, 0)
      
    } else if (i == 6 | i == 13 | i == 20 | i == 27 | i == 34 | i == 41 | i == 48 | i == 55 | i == 62) {
      source = "Secondary sulfate"
      quantile_F6 = as.numeric(quantile(data[[paste0("F6", coluna)]], c(.75), na.rm = T))
      data$case <- ifelse(data[[paste0("F6", coluna)]] >= quantile_F6, 1, 0)

    } else if (i == 7 | i == 14 | i == 21 | i == 28 | i == 35 | i == 42 | i == 49 | i == 56 | i == 63) {
      source = "Combustion of biomass, coal and fossil fuels"
      quantile_F7 = as.numeric(quantile(data[[paste0("F7", coluna)]], c(.75), na.rm = T))
      data$case <- ifelse(data[[paste0("F7", coluna)]] >= quantile_F7, 1, 0)
    } 
    
    case_control = as.data.frame(table(data$case))
    
    
    
    # Run the clogit model
    ##################################################################
    
    
    library(survival)
    model_clogit <- clogit(case ~ data[[paste0("PM25", coluna)]] +
                             data[[paste0("TEMP", coluna)]] + data[[paste0("RH", coluna)]] + data[[paste0("PREC", coluna)]] + 
                             data[[paste0("WD", coluna)]] + data[[paste0("WS", coluna)]] + 
                             QT_DIARIAS + strata(strata_id),
                           data)
    

    # Extract the OR
    beta <- summary(model_clogit)$coefficients[1,2]
    # Extract percentage increase associated with source-related air pollution events
    beta_perc <- (beta-1)*100
    # Extract SE
    se <- summary(model_clogit)$coefficients[1,3]
    # Extract OR 95%CI
    lo95ci=((beta-1.96*se))
    hi95ci=((beta+1.96*se))
    # Extract 95%CI percentage increase associated with source-related air pollution events
    lo95ci_perc= (lo95ci-1)*100
    hi95ci_perc= (hi95ci-1)*100
    AIC <-broom::glance(model_clogit)$AIC
    BIC <-broom::glance(model_clogit)$BIC
    
    
    # Save results
    results.temp[i+j*63,1]<- strata_config
    results.temp[i+j*63,2]<- outcome
    results.temp[i+j*63,3]<- sex
    results.temp[i+j*63,4]<- source
    results.temp[i+j*63,5]<- analysis
    results.temp[i+j*63,6]<- lag
    results.temp[i+j*63,7]<- case_control[1,2]
    results.temp[i+j*63,8]<- case_control[2,2]
    results.temp[i+j*63,9]<- beta
    results.temp[i+j*63,10]<- lo95ci
    results.temp[i+j*63,11]<- hi95ci 
    results.temp[i+j*63,12]<- beta_perc
    results.temp[i+j*63,13]<- lo95ci_perc
    results.temp[i+j*63,14]<- hi95ci_perc
    results.temp[i+j*63,15]<- AIC
    results.temp[i+j*63,16]<- BIC
    
    

    ### Save the results in a Table
    ################################################
    colnames(results.temp)<-c("Strata", "Outcome", "Group", "Source",
                              "Model", "Moving Average", "Controls(n)", "Cases(n)", 
                              "Odds Ratio", "Lower 95%CI", "Upper 95%CI", 
                              "% increase", "% increase Lower 95%CI", "% increase Upper 95%CI",
                              "AIC", "BIC")
    }
}  
}



#### Save the results in a Table

write_csv(results.temp, 
          paste0("./output/03-results_cross_",strata_config,".csv"))  


### Part 7 - Visualizing the results ----
##########################################################################################################
results_temp_cols <- results.temp

colnames(results_temp_cols)<-c("Region", "Outcome", "Group", "Source", "Model", "Moving_Average", "Controls(n)",
                               "Cases(n)", "Odds Ratio", "Lower_95CI", "Upper_95CI", 
                               "increase", "increaseLower95CI", "increaseUpper95CI", "AIC", "BIC")




results_temp_cols %>%
  filter(Outcome == "Circulatory" | Outcome == "Respiratory") %>%
  mutate(Moving_Average = recode((Moving_Average),
                                 "1-day moving average" = "1-day",
                                 "2-day moving average" = "2-day",
                                 "3-day moving average" = "3-day",
                                 "4-day moving average" = "4-day",
                                 "5-day moving average" = "5-day"),
         Source = recode((Source),
                         "Mineral dust and soil ressuspension" = "Soil and Dust",
                         "Aged sea salt" = "Aged sea salt",
                         "Secondary nitrate" = "Secondary nitrate",
                         "Fresh sea salt mixed with aged emissions" = "Fresh sea salt",
                         "Industrial emissions" = "Industrial",
                         "Secondary sulfate" = "Secondary sulphate",
                         "Combustion of biomass, coal and fossil fuels" = "Combustion")) %>%
  ggplot(aes(y = increase, x = Moving_Average, 
             ymin = increaseLower95CI,
             ymax = increaseUpper95CI)) +
  geom_point(aes(color = Outcome), position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(min(results_temp_cols$increaseLower95CI, na.rm = T)-10, 100),
                     breaks = seq(0, 100, by = 20)) + 
  geom_errorbar(width =.2, aes(color = Outcome), position = position_dodge(0.5)) +
  labs(title = NULL, x = 'Moving average - Lag', y = 'Percentage change (95%CI)') +
  geom_hline(yintercept = 0, color='black', linetype='dashed', alpha=.5) +
  scale_color_manual(values = c("black", "gray"))  +
  theme_classic() + theme(axis.title.y = element_text(vjust=2),
                          legend.position = "bottom", text = element_text(size = 11)) +
  facet_grid(Group ~ Source)

