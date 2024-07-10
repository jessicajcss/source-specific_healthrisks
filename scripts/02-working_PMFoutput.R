#############################################################################
# # Preparing PMF output data to Health Risk Analysis

### Date: 09/11/2020 | Last update: 07/2024
### By: Jéssica C. dos Santos-Silva

# # Associated paper:  A fingerprint of source-specific health risk of PM2.5-bound components over a coastal industrial city
#############################################################################
#############################################################################



##############################################
############### LOAD PACKAGES ################

library(tidyverse)



##############################################
############### Preparing data ###############
##############################################


#############################
# PMF Results ----
#############################


###########
## MIC ----

matrizF <- read.csv(
  "./output/PMF/02-factor_profiles_conc_matrizF.csv")  %>%
  filter(site == "MIC")
matrizG <- read.csv(
  "./output/PMF/02-factor_contribution_matrizG.csv") %>%
  filter(site == "MIC")

result <- list()

for (i in 3:ncol(matrizG)) {
  mG <- as.matrix(matrizG[c(i)])
  mF <- matrizF[c(2,i)]
  rownames(mF) <- mF$Species
  row_namesmF <- rownames(mF)
  mF <- mF[,-1]
  mF <- as.matrix(t(mF))
  result[[i]] <-  (mG %*% mF) %>% as.data.frame()
  colnames(result[[i]]) <- row_namesmF
}

result[[3]]$fator <- "Fator.1"
result[[4]]$fator <- "Fator.2"
result[[5]]$fator <- "Fator.3"
result[[6]]$fator <- "Fator.4"
result[[7]]$fator <- "Fator.5"
result[[8]]$fator <- "Fator.6"
result[[9]]$fator <- "Fator.7"

result[[3]]$date <- matrizG$date
result[[4]]$date <- matrizG$date
result[[5]]$date <- matrizG$date
result[[6]]$date <- matrizG$date
result[[7]]$date <- matrizG$date
result[[8]]$date <- matrizG$date
result[[9]]$date <- matrizG$date

results_factor_site <- dplyr::bind_rows(result)
results_factor_site$site <- "MIC"

results_factor_site1 <- results_factor_site %>%
  select(site, date, fator, BC:calcio)


results_factor_site1 <- results_factor_site %>%
  mutate(Br = NA) %>%
  relocate(Br, .before = Zn) %>%
  select(site, date, fator, BC:calcio)




###########
## NID ----

matrizF <- read.csv(
  "./output/PMF/02-factor_profiles_conc_matrizF.csv")  %>%
  filter(site == "NID")
matrizG <- read.csv(
  "./output/PMF/02-factor_contribution_matrizG.csv") %>%
  filter(site == "NID")

result <- list()


for (i in 3:ncol(matrizG)) {
  mG <- as.matrix(matrizG[c(i)])
  mF <- matrizF[c(2,i)]
  rownames(mF) <- mF$Species
  row_namesmF <- rownames(mF)
  mF <- mF[,-1]
  mF <- as.matrix(t(mF))
  result[[i]] <-  (mG %*% mF) %>% as.data.frame()
  colnames(result[[i]]) <- row_namesmF
}


result[[3]]$fator <- "Fator.1"
result[[4]]$fator <- "Fator.2"
result[[5]]$fator <- "Fator.3"
result[[6]]$fator <- "Fator.4"
result[[7]]$fator <- "Fator.5"
result[[8]]$fator <- "Fator.6"
result[[9]]$fator <- "Fator.7"

result[[3]]$date <- matrizG$date
result[[4]]$date <- matrizG$date
result[[5]]$date <- matrizG$date
result[[6]]$date <- matrizG$date
result[[7]]$date <- matrizG$date
result[[8]]$date <- matrizG$date
result[[9]]$date <- matrizG$date

results_factor_site <- dplyr::bind_rows(result)
results_factor_site$site <- "NID"


results_factor_site2 <- results_factor_site %>%
  select(site, date, fator, BC:calcio) 




###########
## Binding final dataset: both sites ----

results_factor_sites <- rbind(results_factor_site1,
                              results_factor_site2)



tz(results_factor_sites$date)
results_factor_sites$date <- mdy_hm(results_factor_sites$date)
results_factor_sites$date <- results_factor_sites$date + hours(3)


# Adjusting Time Zone
matrizG <- read.csv("./output/PMF/02-factor_contribution_matrizG.csv")
matrizG$date <- mdy_hm(matrizG$date)
matrizG$date <- matrizG$date + hours(3)


write_csv(matrizG,
          "./output/02-contribution_factor.csv")

write_csv(results_factor_sites,
          "./output/02-concentration_species.csv")


WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
###########
## Calculating concentration contribution by factor
df <- results_factor_sites %>%
  as.data.frame() %>%
  rowwise() %>%
  mutate(total = sum(c_across(BC:calcio), na.rm = T)) %>%
  filter(total != 0) %>%
  select(site, date, fator, total) %>%
  dcast(site + date ~ fator, value.var = "total")



write_csv(df,
          "./output/02-concentration_factor.csv")

