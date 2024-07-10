*Supplementary material:* 
 A fingerprint of source-specific health risk of PM2.5-bound components over a coastal industrial city

dos Santos Silva, Jessica Caroline; Westrupp Medeiros, Sandra Helena; da Silva, Luiz Vitor; Ventura Ferreira, Danielli; Yamamoto, Carlos Itsuo; Godoi, Ricardo Henrique Moreton. doi: XXXX



#######################
COLUMN DESCRIPTION FILE
#######################




DATASET: meteorological data
FILES: 
COLUMNS:
  date (or date.meteo) - date_time (UTC) of record
  temp - Temperature (ºC)
  umid - Relative Humidity (%)
  ws - Wind speed (m/s)
  wd - Wind direction (º)
  prec - Accumulated precipitation (mm)
  u - u.wind component (m/s)
  v - v.wind component (m/s)
Source: Original meteorological data was provisioned by Santa Catarina Civil Defense (2021) or [SBJV Airport data] is available on the MESONET website.

REFERENCE:
MESONET, n.d. ASOS-AWOS-METAR Data Download [WWW Document]. URL https://mesonet.agron.iastate.edu/request/download.phtml?network=BR__ASOS (accessed 3.2.22). 


DATASET: sampling and laboratory results data
FILES: 01-concentration_data.csv; concentracao_meteo_windflow.csv
COLUMNS:
  Filter.ID - sample identification
  rep - laboratory replicate measurement
  site - sampling site (MIC - Metallurgical Industrial Complex, 
                        NID - North Industrial District)
  date - start time of sampling
  date.end - end time of sampling
  flow - sampling flow rate (lpm)
  time - sampling duration (min)
  PM2.5 - PM2.5 mass concentration*
  BC - black carbon concentration*
  pBC - percentual of black carbon in the sample
  Al:Hg - EDXRF results - elemental concentration*
  fluoride: calcium - IC results - ionic concentration*
  * all concentrations are in ug/m³




