
#############################################################################
#############################################################################
# # Preparing COMPLETE DATASET to Health Risk Analysis

### Date: 09/11/2020 | Last update: 07/2024
### By: Jéssica C. dos Santos-Silva

# # Associated paper:  A fingerprint of source-specific health risk of PM2.5-bound components over a coastal industrial city
#############################################################################
#############################################################################


## References ---- 

# Requia, W. J.; Amini, H.; Mukherjee, R.; Gold, D. R.; Schwartz, J. D.
# Nat Commun 2021, https://doi.org/10.1038/s41467-021-26822-7.


##############################################
############### LOAD PACKAGES ################

library(tidyverse)
library(reshape2)
library(survival)

##############################################
############### Preparing data ###############
##############################################


### Part 1 - Organize and clean the health data ----
################################################

#source("./scripts/00-hospital_admissions_datasus.R")

health_data <- read_csv("./output/01-health_data.csv")




### Part 2 - Organize and clean the meteo data ----
##########################################################################################################

## Dataset: daily data ----

meteo_daily <- read_csv("./data/00-meteo_daily.csv") 


### Part 3 - Organize and clean the PM2.5 sampling/source data ----
##########################################################################################################


# Sampling/chemical data ----
avg_concentrations <-  read_csv("./data/00-meanPM2p5_concentrations.csv")



# Factors output PMF
factors <- read_csv("./output/02-contribution_factor.csv") %>%
  melt(id = c("site", "date")) %>%
  rename("factor" = "variable",
         "total" = "value") %>%
  rowwise() %>%                   
  mutate(source = case_when(factor == 'Factor.1' & site == 'MIC' ~ "mineral",
                            factor == 'Factor.2' & site == 'MIC' ~ "agedss",
                            factor == 'Factor.3' & site == 'MIC' ~ "secnitrate",
                            factor == 'Factor.4' & site == 'MIC' ~ "freshss",
                            factor == 'Factor.5' & site == 'MIC' ~ "industrial",
                            factor == 'Factor.6' & site == 'MIC' ~ "secsulphate",
                            factor == 'Factor.7' & site == 'MIC' ~ "combustion",
                            factor == 'Factor.1' & site == 'NID' ~ "freshss",
                            factor == 'Factor.2' & site == 'NID' ~ "secsulphate",
                            factor == 'Factor.3' & site == 'NID' ~ "agedss",
                            factor == 'Factor.4' & site == 'NID' ~ "industrial",
                            factor == 'Factor.5' & site == 'NID' ~ "combustion",
                            factor == 'Factor.6' & site == 'NID' ~ "secnitrate",
                            factor == 'Factor.7' & site == 'NID' ~ "mineral",
                            TRUE ~ NA)) %>%
  select(site, date, source, total) %>%
  mutate(source = recode((source),
                         "mineral" = "Factor.1",
                         "agedss" = "Factor.2",
                         "secnitrate" = "Factor.3",
                         "freshss" = "Factor.4",
                         "industrial" = "Factor.5",
                         "secsulphate" = "Factor.6",
                         "combustion" = "Factor.7")) %>%
  reshape2::dcast(site + date ~ source, value.var = 'total')


factors <- factors %>%
  rowwise() %>%
  mutate(total = sum(c_across(Factor.1:Factor.7), na.rm = T)) %>%
  mutate_at(vars(Factor.1:total), ~replace(., total == 0, NA))

tz(factors$date)




# From 1025 samples, 346 were chemically analysed


# Merging sample analysis with source apportionment factors

source_samples <- left_join(avg_concentrations,
                            factors,
                            by = c('site', 'date')) 
tz(factors$date)


# PM2.5 samples do not represent a full day, so data needs to be standardized by day to match the other datasets

df <- source_samples %>%
  select(site, date, date.end)

summary(source_samples)


# to convert partial sample time to the day they most represent
daily <- seq(as.POSIXct("2018-08-15"), 
             as.POSIXct("2020-10-08"), "days") %>% as.data.frame()
colnames(daily) <- "date"

# partial datasets according to samples timeframe coverage
out0 <- vector("list", length(df))
out1 <- vector("list", length(df))
out2 <- vector("list", length(df))
out3 <- vector("list", length(df))



for (i in 1:nrow(df)) {
  if ((format(as.POSIXct(df$date), format = "%H:%M")  <= "12:00")[i] && 
      (format(as.POSIXct(df$date.end), format = "%H:%M")  <= "12:00")[i]) {
    start <- as.Date(df$date)[i]
    end <- as.Date(df$date.end)[i]
    subperiodo <- subset(daily, date < as.Date(end) & date >= as.Date(start))
    out0[[i]] <- subperiodo %>%
      mutate(date.start = df$date[i],
             date.end = df$date.end[i])
  } else if ((format(as.POSIXct(df$date), format = "%H:%M")  > "12:00")[i] && 
             (format(as.POSIXct(df$date.end), format = "%H:%M")  >= "12:00")[i]) {
    start <- as.Date(df$date)[i]
    end <- as.Date(df$date.end)[i]
    subperiodo <- subset(daily, date <= as.Date(end) & date > as.Date(start))
    out1[[i]] <- subperiodo %>%
      mutate(date.start = df$date[i],
             date.end = df$date.end[i])
  } else if ((format(as.POSIXct(df$date), format = "%H:%M")  <= "12:00")[i] && 
             (format(as.POSIXct(df$date.end), format = "%H:%M")  > "12:00")[i]) {
    start <- as.Date(df$date)[i]
    end <- as.Date(df$date.end)[i]
    subperiodo <- subset(daily, date <= as.Date(end) & date >= as.Date(start))
    out2[[i]] <- subperiodo %>%
      mutate(date.start = df$date[i],
             date.end = df$date.end[i])
  } else { 
    start <- as.Date(df$date)[i]
    end <- as.Date(df$date.end)[i]
    subperiodo <- subset(daily, date <= as.Date(end) & date >= as.Date(start))
    
    out3[[i]] <- subperiodo %>%
      mutate(date.start = df$date[i],
             date.end = df$date.end[i])
  }
}  

output <- dplyr::bind_rows(out0, out1, out2, out3) %>% # combine the output into a single data frame
  distinct() 
head(output)


# Aggregate by Date (daily average)
source_samples <- source_samples %>%
  rename(date.start = date)



daily_source_samples <- left_join(output, 
                                  source_samples, 
                                  by = c("date.start", "date.end"))

data_covariates_daily <- plyr::ddply(daily_source_samples, 
                                     c("date", "site"), summarise,
                                     PM2.5 = mean(PM2.5, na.rm=TRUE),
                                     Factor.1 = mean(Factor.1),
                                     Factor.2 = mean(Factor.2),
                                     Factor.3 = mean(Factor.3),
                                     Factor.4 = mean(Factor.4),
                                     Factor.5 = mean(Factor.5),
                                     Factor.6 = mean(Factor.6),
                                     Factor.7 = mean(Factor.7))


data_covariates_daily <- left_join(data_covariates_daily, 
                                   meteo_daily,
                                   by = c("site", "date"))



# Create lags (1-5 days).

data_lags_covariates <- data_covariates_daily %>% 
  arrange(as.Date(date)) %>% 
  group_by(site) %>% 
  mutate(Lag1_PM25 = lag(PM2.5, 1)) %>%
  mutate(Lag2_PM25 = lag(PM2.5, 2)) %>%
  mutate(Lag3_PM25 = lag(PM2.5, 3)) %>%
  mutate(Lag4_PM25 = lag(PM2.5, 4)) %>%
  mutate(Lag5_PM25 = lag(PM2.5, 5)) %>% 
  
  mutate(Lag1_F1 = lag(Factor.1, 1)) %>%
  mutate(Lag2_F1 = lag(Factor.1, 2)) %>%
  mutate(Lag3_F1 = lag(Factor.1, 3)) %>%
  mutate(Lag4_F1 = lag(Factor.1, 4)) %>%
  mutate(Lag5_F1 = lag(Factor.1, 5)) %>%
  
  mutate(Lag1_F2 = lag(Factor.2, 1)) %>%
  mutate(Lag2_F2 = lag(Factor.2, 2)) %>%
  mutate(Lag3_F2 = lag(Factor.2, 3)) %>%
  mutate(Lag4_F2 = lag(Factor.2, 4)) %>%
  mutate(Lag5_F2 = lag(Factor.2, 5)) %>%
  
  mutate(Lag1_F3 = lag(Factor.3, 1)) %>%
  mutate(Lag2_F3 = lag(Factor.3, 2)) %>%
  mutate(Lag3_F3 = lag(Factor.3, 3)) %>%
  mutate(Lag4_F3 = lag(Factor.3, 4)) %>%
  mutate(Lag5_F3 = lag(Factor.3, 5)) %>%
  
  mutate(Lag1_F4 = lag(Factor.4, 1)) %>%
  mutate(Lag2_F4 = lag(Factor.4, 2)) %>%
  mutate(Lag3_F4 = lag(Factor.4, 3)) %>%
  mutate(Lag4_F4 = lag(Factor.4, 4)) %>%
  mutate(Lag5_F4 = lag(Factor.4, 5)) %>%
  
  mutate(Lag1_F5 = lag(Factor.5, 1)) %>%
  mutate(Lag2_F5 = lag(Factor.5, 2)) %>%
  mutate(Lag3_F5 = lag(Factor.5, 3)) %>%
  mutate(Lag4_F5 = lag(Factor.5, 4)) %>%
  mutate(Lag5_F5 = lag(Factor.5, 5)) %>%
  
  mutate(Lag1_F6 = lag(Factor.6, 1)) %>%
  mutate(Lag2_F6 = lag(Factor.6, 2)) %>%
  mutate(Lag3_F6 = lag(Factor.6, 3)) %>%
  mutate(Lag4_F6 = lag(Factor.6, 4)) %>%
  mutate(Lag5_F6 = lag(Factor.6, 5)) %>%
  
  mutate(Lag1_F7 = lag(Factor.7, 1)) %>%
  mutate(Lag2_F7 = lag(Factor.7, 2)) %>%
  mutate(Lag3_F7 = lag(Factor.7, 3)) %>%
  mutate(Lag4_F7 = lag(Factor.7, 4)) %>%
  mutate(Lag5_F7 = lag(Factor.7, 5)) %>%
  
  mutate(Lag1_TEMP = lag(temp, 1)) %>%
  mutate(Lag2_TEMP = lag(temp, 2)) %>%
  mutate(Lag3_TEMP = lag(temp, 3)) %>%
  mutate(Lag4_TEMP = lag(temp, 4)) %>%
  mutate(Lag5_TEMP = lag(temp, 5)) %>%
  
  mutate(Lag1_RH = lag(umid, 1)) %>%
  mutate(Lag2_RH = lag(umid, 2)) %>%
  mutate(Lag3_RH = lag(umid, 3)) %>%
  mutate(Lag4_RH = lag(umid, 4)) %>%
  mutate(Lag5_RH = lag(umid, 5)) %>%
  
  mutate(Lag1_PREC = lag(prec, 1)) %>%
  mutate(Lag2_PREC = lag(prec, 2)) %>%
  mutate(Lag3_PREC = lag(prec, 3)) %>%
  mutate(Lag4_PREC = lag(prec, 4)) %>%
  mutate(Lag5_PREC = lag(prec, 5)) %>%
  
  mutate(Lag1_WD = lag(wd, 1)) %>%
  mutate(Lag2_WD = lag(wd, 2)) %>%
  mutate(Lag3_WD = lag(wd, 3)) %>%
  mutate(Lag4_WD = lag(wd, 4)) %>%
  mutate(Lag5_WD = lag(wd, 5)) %>%
  
  mutate(Lag1_WS = lag(ws, 1)) %>%
  mutate(Lag2_WS = lag(ws, 2)) %>%
  mutate(Lag3_WS = lag(ws, 3)) %>%
  mutate(Lag4_WS = lag(ws, 4)) %>%
  mutate(Lag5_WS = lag(ws, 5)) 




### Part 4 - Merge health data with the PM2.5 and covariates ----
################################################################


health_data <- health_data %>%
  rename(date = DT_INTER)


final_data <- left_join(health_data, data_lags_covariates, 
                        by = "date")

# ADD SEASON

library(openair)
final_data <- final_data %>%
  cutData(type = "season", hemisphere = "southern")




### Remove missing values in the predictors. This will remove observations that did not merge 
#library(tidyr)
final_data_2 <- final_data %>% drop_na(PM2.5, temp, umid,
                                       prec, ws, wd)





### Part 5 - Finalize the data before statistical model - select predictors, remove missing values, and set additional variables
##################################################################################################################################
# Select variables for statistical analyses
myvars <- c("site",                                                          # Location variables
            "DIAG_PRINC", "QT_DIARIAS", "CID",                               # Health outcome 
            "date", "Year", "season", "Month", "Day", "DOW", "HOLIDAY",      # Temporal variables   
            "SEX", "IDADE",                                                  # SES variables  
            "PM2.5",                                                         # Air pollution variables
            "Factor.1", "Factor.2", "Factor.3", "Factor.4",
            "Factor.5", "Factor.6", "Factor.7",                              # Source contributions
            "temp", "umid", "prec", "wd", "ws",                              # Weather variables
            "Lag1_PM25", "Lag2_PM25", "Lag3_PM25", "Lag4_PM25", "Lag5_PM25", # Lag variables
            "Lag1_F1", "Lag2_F1", "Lag3_F1", "Lag4_F1", "Lag5_F1",           # Lag variables
            "Lag1_F2", "Lag2_F2", "Lag3_F2", "Lag4_F2", "Lag5_F2",           # Lag variables
            "Lag1_F3", "Lag2_F3", "Lag3_F3", "Lag4_F3", "Lag5_F3",           # Lag variables
            "Lag1_F4", "Lag2_F4", "Lag3_F4", "Lag4_F4", "Lag5_F4",           # Lag variables
            "Lag1_F5", "Lag2_F5", "Lag3_F5", "Lag4_F5", "Lag5_F5",           # Lag variables
            "Lag1_F6", "Lag2_F6", "Lag3_F6", "Lag4_F6", "Lag5_F6",           # Lag variables
            "Lag1_F7", "Lag2_F7", "Lag3_F7", "Lag4_F7", "Lag5_F7",           # Lag variables
            "Lag1_TEMP", "Lag2_TEMP", "Lag3_TEMP", "Lag4_TEMP", "Lag5_TEMP", # Lag variables
            "Lag1_RH", "Lag2_RH", "Lag3_RH", "Lag4_RH", "Lag5_RH",           # Lag variables
            "Lag1_PREC", "Lag2_PREC", "Lag3_PREC", "Lag4_PREC", "Lag5_PREC", # Lag variables
            "Lag1_WS", "Lag2_WS", "Lag3_WS", "Lag4_WS", "Lag5_WS",           # Lag variables
            "Lag1_WD", "Lag2_WD", "Lag3_WD", "Lag4_WD", "Lag5_WD")           # Lag variables


final_data_3 <- final_data_2[myvars]

final_data_3 %>% summary()

write_csv(final_data_3, 
          "./output/03-data_crossanalysis.csv")





### Part 6 - Statistical analyses 
####################################

# Go to '03-healthrisk_analysis.R'
# source("./scripts/02-preparing_healthrisk_analysis.R")