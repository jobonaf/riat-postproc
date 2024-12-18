# data extracted with following query
# https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.ebd.countries_and_nuts&UrbanisationDegree=All Areas (incl.unclassified)&CountryOrTerritory=Italy&Year=2013,2017&DataAggregation=P1Y
library(readr)
library(tidyr)
library(glue)
dat <- read_csv("out/eea/DataExtract.csv")
dat %>%
  distinct(
    `NUTS Code`, 
    `NUTS Name`, 
    Year, 
    `Air Pollutant`, 
    `Air Pollution Average [ug/m3]`) %>%
  pivot_wider(
    names_from = Year, 
    values_from = `Air Pollution Average [ug/m3]`) -> dd
for (pp in c("PM2.5","NO2")) {
  dd %>% 
    filter(nchar(`NUTS Code`)==5, `Air Pollutant`==pp) %>% 
    write_csv(., glue("out/eea/{pp}_PROVINCIA_dati_EEA.csv"))
}
