---
title: "Step_by_Step"
author: "Jessica C. dos Santos-Silva"
date: "07/10/2024"
---

*Supplementary material:*

** A fingerprint of source-specific health risk of PM2.5-bound components over a coastal industrial city **. Dataset associated with this material can be found at DOI: XXXXX


Authors: 
Jéssica Caroline dos Santos Silva^1^, Sanja Potgieter-Vermaak^2,3^, Sandra Helena Westrupp Medeiros^4^, Luiz Vitor da Silva^4^, Danielli Ventura Ferreira^4^, Carlos Itsuo Yamamoto^5^, Ricardo Henrique Moreton Godoi^1,5,6*^


<p style="font-family: times, serif; font-size:8pt; font-style:italic">
1-	Postgraduate Program in Water Resources and Environmental Engineering, Federal University of Paraná, Curitiba, Paraná, Brazil;
2-	Ecology & Environment Research Centre, Department of Natural Science, Manchester Metropolitan University, Manchester M1 5GD, United Kingdom;
3-	Molecular Science Institute, University of the Witwatersrand, Johannesburg, South Africa;
4-	Department of Environmental and Sanitary Engineering, University of the Region of Joinville, Joinville, Santa Catarina, Brazil;
5-	Department of Chemical Engineering, Federal University of Paraná, Curitiba, Paraná, Brazil;
6-	Department of Environmental Engineering, Federal University of Paraná, Curitiba, Paraná, Brazil.
</p>



First, run the script **00-basic_functions.R**.

To column descriptions and variable units, see **README_Column_description.txt**.



# Data wrangling: Preparing data to PMF Analysis ---

## Chemical analysis data

*Dataset*: 00-rawdata_concentrations.csv - PM2.5 chemical analysis: concentrations were calculated without blank of LQ correction
*Dataset*: 00-MDLug.csv - MDL calculates from blank values

*Script*: 01-preparing_data_PMF.R - The normal input files for EPA-PMF are two matrices: one with the concentration data values and the other with the associated sample-by-sample uncertainties.

*Output*: 01-data_PMF_mean.csv
*Output*: 01-data_PMF_unc.csv

*Dataset*: 00-meanPM2p5_concentrations.csv - PM2.5 sampling concentrations - to be used in Health Risk Analysis
*Dataset*: 00-meteo_sampling_climatol.csv - sampling data and period-specific meteo data for several analysis



## Health and population data

*Script*: 00-hospital_admissions_datasus.R - download hospital admissions data from Datasus database, filtering Joinville data and preparing data for health risk analysis (this includes the following variables: day of the week, sex, hospitalization date, ICD, hospitalization duration)

*Output*: 01-health_data.csv



## Meteo data

*Dataset*: 00-meteo_sampling_climatol.csv - Meteorological and Sampling data, by sampling period.

*Dataset*: 00-meteo_daily.csv - Daily meteotological data





# Data analysis ---

## Source apportionment

*Dataset*: 02-factor_profiles_conc_matrizF.csv
*Dataset*: 02-factor_contribution_matrizG.csv

*Script*: 02-working_PMFoutput.R - calculating each factor sample  concentration.

*Output*: 02-contribution_factor.csv - Matriz G, adjusted to use in the case-crossover analysis.



## Source-specific and health risk assessment of PM2.5

*Dataset*: 00-meteo_daily.csv - Daily meteorological data
*Dataset*: 00-meanPM2p5_concentrations.csv - PM2.5 sampling data
*Dataset*: 01-health_data.csv - Datasus: hospital admission data
*Dataset*: 02-contribution_fator.csv - PMF: Matriz G - source contribution

*Script*: 01-preparing_healthrisk_analysis.R - Preparing data to case-crossover analysis
*Output*: 03-data_crossanalysis.csv 

*Script*: 03-healthrisk_analysis.R - Health risk analysis - case-crossover design
*Output*: 03-results_cross_sourceqtil.csv - Results










*More info*


For missing data, homogenization and interpolation were done using the **Climatol package in R** (Azorin-Molina et al., 2019; Guijarro, 2019). 

See script **01-meteo_data.R** in:

dos Santos-Silva, J. C.; Potgieter-Vermaak, S.; Medeiros, S. H. W.; da Silva, L. V.; Ferreira, D. V.; Moreira, C. A. B.; de Souza Zorzenão, P. C.; Pauliquevis, T.; Godoi, A. F. L.; de Souza, R. A. F.; Yamamoto, C. I.; Godoi, R. H. M. A New Strategy for Risk Assessment of PM<inf>2.5</Inf>-Bound Elements by Considering the Influence of Wind Regimes. Science of the Total Environment 2023, 872. https://doi.org/10.1016/j.scitotenv.2023.162131.






**REFERENCES:**

Azorin‐Molina, C.; Guijarro, J. A.; McVicar, T. R.; Trewin, B. C.; Frost, A. J.; Chen, D. An Approach to Homogenize Daily Peak Wind Gusts: An Application to the Australian Series. International Journal of Climatology 2019, 39 (4), 2260–2277. https://doi.org/10.1002/joc.5949.

Guijarro, J. A. Climatol: Climate Tools (Series Homogenization and Derived Products). R Package Version 3.1.2. 2019. https://CRAN.R-project.org/package=climatol (accessed 2020-10-12).

NIOSH. CAS Number Listing - No CAS number - NMAM 4th Edition | NIOSH | CDC. NIOSH 0500 Manual of Analytical Methods 881 (NMAM), Fourth Edition. https://www.cdc.gov/niosh/docs/2003-154/method-cas0.html (accessed 2023-12-12).

Norris, G. EPA Positive Matrix Factorization 5.0 Fundamentals and User Guide | US EPA. Prepared for the US Environmental Protection Agency Office of Research and Development. https://www.epa.gov/air-research/epa-positive-matrix-factorization-50-fundamentals-and-user-guide (accessed 2024-01-24).

Requia, W. J.; Amini, H.; Mukherjee, R.; Gold, D. R.; Schwartz, J. D. Health Impacts of Wildfire-Related Air Pollution in Brazil: A Nationwide Study of More than 2 Million Hospital Admissions between 2008 and 2018. Nat Commun 2021, 12 (1), 6555. https://doi.org/10.1038/s41467-021-26822-7.




