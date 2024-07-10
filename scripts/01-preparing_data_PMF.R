#############################################################################
#############################################################################
# # Preparing data to PMF Analysis
### Associated paper: A fingerprint of source-specific health risk of PM2.5-bound components over a coastal industrial city


### Date: 05-05-2022 | Last updated: 09/2024
### Elaborated by: Jéssica C. dos Santos-Silva

#############################################################################
#############################################################################

## Considerations ----

#! Raw data is used, without blank ou LQ corrections
#! Limit of detection (LD) used are given by the instruments*
#! MDL (Minimum detection Limit) are calculated from the 'average of blanks measurements + 3*SD' (Scerri & EPA, 2016 - MDL)
#! Missing values were set as '-999'
#! No censoring of data below detection or substitution of missing data was done.(Brown et al., 2015)

## SECTION 1 - Uncertainties are calculated using MDL values
### > [MDL in ug, line 35:120]
### > [No treatment of data, line 125:305]
### > [INPUT PMF means - without any treatment, line 315:384]
### > [% Valid data - criteria to select variables, line 400:440]
### > [INPUT PMF uncertainties, line 450:550]

##############################################
############### LOAD PACKAGES ################


library(tidyverse)
library(reshape2) #Reorganize


#########################################################################

# RAW DATASET: PM2.5 chemical analysis
rawdata <- read_csv("./data/00-rawdata_concentrations.csv")



# Preparing dataset of mean concentrations -------------------------------------------------

# long/molten format
data_melt <- melt(rawdata, 
                  id=c("date", "site", "rep"))

# mean concentrations - to use as INPUT in PMF5.0 analysis
concentracao_mean <- dcast(data_melt, 
                           date + site ~ variable, 
                           mean)
summary(concentracao_mean)


########################################
################ OUTPUT ################


# INPUT PMF-concentration ----------------------------------------
# mising values = -999
concentracao_mean %>%
  select(-flow, -time) %>%
  mutate_at(vars(PM2.5:calcio), ~replace(., is.na(.), -999)) %>% #summary() #verificar NAs
  write_csv("./output/02-data_PMF_mean.csv")




# INPUT PMF, uncertainty  ----------------------------------------
detection <- read.csv("./data/01-MDLug.csv")


# dataframe MDL
MDL <- detection %>% 
  select(variable, MDL) %>% 
  t() %>% as.data.frame()
names(MDL) <- lapply(MDL[1, ], as.character)
MDL <- MDL[-1,]


# Uncertainty calculation considering equations and criteria discussed in Norris, 2014.

# dataframe to use as base in the formula
data_PMF_unc <- concentracao_mean
MDL_data <- concentracao_mean

# a part of the equation:
fstterm <- function(x, na.rm = FALSE) ((x*0.1)^2)


for (j in 5:ncol(data_PMF_unc)) {
  MDLug <- as.numeric(MDL[1, ][as.character(colnames(data_PMF_unc[j]))])
  MDL_data <- MDL_data %>% # MDL^2
    mutate_at(vars(colnames(MDL_data[j])), ~replace(., !is.na(PM2.5), 
                                                    ((MDLug/(flow*time))/2)^2)) # MDL adjustment to ug/m³
  data_PMF_unc <- data_PMF_unc %>% # part - (x*p) ^2 - of the uncertainty equation
    mutate_at(vars(colnames(data_PMF_unc[j])), fstterm) 
}



# Uncertainty DATAFRAME - final calculations
uncertainty <- MDL_data

uncertainty[c(5:ncol(uncertainty))] <- as.data.frame(as.matrix(MDL_data[c(5:ncol(MDL_data))]) + as.matrix(data_PMF_unc[c(5:ncol(data_PMF_unc))]))
uncertainty[c(5:ncol(uncertainty))] <- sqrt(uncertainty[c(5:ncol(uncertainty))])



# Last criteria: IF concentration < MDL, UNCERTAINTY will be 5*MDL/6, since PMF does not accept zero or negative value 

concentracao_mean <- concentracao_mean %>%
  mutate_at(vars(PM2.5:calcio), ~replace(., is.na(.), -999))


for (i in 1:nrow(uncertainty)) {
  for (j in 5:ncol(uncertainty)) {
    if (is.na(concentracao_mean[i, j])) {
      MDLugm3 <- as.numeric(MDL[1, ][as.character(colnames(concentracao_mean[j]))])/(concentracao_mean$flow[i]*concentracao_mean$time[i])
      uncertainty[i, j] <- 5*MDLugm3/6  
    } else if ((concentracao_mean[i, j]) >= (as.numeric(MDL[1, ][as.character(colnames(concentracao_mean[j]))])/(concentracao_mean$flow[i]*concentracao_mean$time[i]))) {
      uncertainty[i, j] <- uncertainty[i, j] 
      } else {
      MDLugm3 <- as.numeric(MDL[1, ][as.character(colnames(concentracao_mean[j]))])/(concentracao_mean$flow[i]*concentracao_mean$time[i])
      uncertainty[i, j] <- 5*MDLugm3/6 
    }
  }
}


# Check for NA values 

########################################
################ OUTPUT ################

# INPUT PMF-uncertainty
# mising values = -999
uncertainty %>%
  select(-flow, -time) %>%
  mutate_at(vars(PM2.5:calcio), ~replace(., is.na(.), -999)) %>% #summary() #verificar NAs
  write_csv("./output/02-data_PMF_unc.csv")







#########################################################################

# LAST: Checking data validation
### Checking which species should be selected according to LD values


LD_ug <- read_csv("./data/01-LDug_instrument.csv") # LD em ug 

non_na <- concentracao_mean # using LD criteria: % of samples with concentration above LD
for (i in 1:nrow(non_na)) {
  for(j in 5:ncol(non_na)){
    LD_ugm3 <- as.numeric(LD_ug[1, ][as.character(colnames(non_na[j]))])/(non_na$flow[i]*non_na$time[i]) #LD adjustment to ug/m³
    if (non_na[i, j] <= LD_ugm3 | is.na(non_na[i, j])) {
      non_na[i, j] <- NA
    }
  }
}

### % data validation
its_na <- non_na %>%
  select(-date, -flow, -time) %>%
  melt(by = 'site') %>%
  group_by(site, variable) %>%
  summarize(n = n(),
            value_non_na = sum(!is.na(value))/n()*100) %>%
  filter(value_non_na < 75)

