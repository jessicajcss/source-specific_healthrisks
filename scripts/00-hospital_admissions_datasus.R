
#############################################################################
#############################################################################
# # Downloading Public Health Data - Number of hospital admission

### Date: 09/11/2019 | Last update: 07/2024
### By: Jéssica C. dos Santos-Silva

# # Associated paper:  A fingerprint of source-specific health risk of PM2.5-bound components over a coastal industrial city
#############################################################################
#############################################################################


## References ---- 

#SALDANHA - https://github.com/rfsaldanha/microdatasus
#SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pre-processamento de microdados do Departamento de Informatica do SUS (DATASUS). Cad. Saude Publica, Rio de Janeiro , v. 35, n. 9, e00032419, 2019 . Available from http://ref.scielo.org/dhcq3y.
#ROCHA et al., 2018. Geolocalizacao de internacoes cadastradas no Sistema de Informacoes Hospitalares do Sistema unico de Saude:uma solucao baseada no programa estatistico R. doi: 10.5123/S1679-49742018000400016
# HOLIDAYS JOINVILLE - https://www.joinville.sc.gov.br/tag-tipo-publicacao/calendario/

##############################################
############### LOAD PACKAGES ################

library(readxl)
library(lubridate) # gerenciar dados data/hora 
library(tidyverse)

library(read.dbc)
library(usethis)
library(devtools)
#install.packages("devtools") # instala o pacote devtools


#devtools::install_github("rfsaldanha/microdatasus")
library(microdatasus)


##############################################
############### Download data ################
##############################################

## Hospital admissions in Joinville (Datasus, using microdatasus package)

### From datasus by year
dados2018 <- fetch_datasus(year_start = 2018, month_start = 1, year_end = 2018, month_end = 12, uf = "SC", information_system = "SIH-RD")
dados2019 <- fetch_datasus(year_start = 2019, month_start = 1, year_end = 2019, month_end = 12, uf = "SC", information_system = "SIH-RD")
dados2020 <- fetch_datasus(year_start = 2020, month_start = 1, year_end = 2020, month_end = 12, uf = "SC", information_system = "SIH-RD")
dados2021 <- fetch_datasus(year_start = 2021, month_start = 1, year_end = 2021, month_end = 12, uf = "SC", information_system = "SIH-RD")
process_sih(dados)

### Binding
dados <- rbind(dados2018,dados2019, dados2020, dados2021)
rm(dados2018,dados2019, dados2020, dados2021)


### Selecting data of Joinville residents
process_dados <- process_sih(dados)
datasus.JOI <- subset(process_dados, 
                      process_dados$MUNIC_RES == '420910') # Joinville code
rm(process_dados)




##############################################
############### Preparing data ################
##############################################


### Part 1 - Organize and clean the health data ----
################################################


# Subset the dataset for the diseases considered in the study (Respiratory = J00-J99; Circulatory = I00-I99)

health_data <- datasus.JOI %>%
  select(CEP, IDADE, SEXO, DT_INTER, DIAG_PRINC, QT_DIARIAS) %>%
  rename("SEX" = "SEXO") %>%
  filter(str_detect(DIAG_PRINC, paste(c("I+", "J+"), collapse = '|'))) %>%
  filter(DT_INTER < ymd('2020-03-17') & DT_INTER >= ymd('2018-08-15')) %>% # LIMITED BY THE FIRST COVID CASE
  mutate(DT_INTER = as.Date(DT_INTER),
         CID = case_when(str_detect(DIAG_PRINC, "I+") ~ "Circulatory",
                         TRUE ~ "Respiratory"),
         # To complete the dataset: year, month, day, weekday
         Year = lubridate::year(DT_INTER),
         Month = lubridate::month(DT_INTER),
         Day = lubridate::day(DT_INTER),
         DOW = wday(DT_INTER),
         SEX = recode((SEX),
                      "Masculino" = "Male",
                      "Feminino" = "Female"))


## Holidays (PREFEITURA DE JOINVILLE) ----
feriados <- read.csv("./draft/00-feriadosjoinville.csv",
                     na = NA,
                     sep = ";",
                     header = TRUE,
                     check.names = F) %>%
  select(date) %>%
  mutate(HOLIDAY = 1, # if holiday, = 1
         date = dmy(date)) %>%
  rename("DT_INTER" = "date")

head(feriados)


health_data <- left_join(health_data, feriados, by = ("DT_INTER"))
health_data$HOLIDAY[is.na(health_data$HOLIDAY)] = 0  # if not a holiday, = 0


write_csv(health_data, 
          "./output/01-health_data.csv")

