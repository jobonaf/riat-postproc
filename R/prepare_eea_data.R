# data extracted with following queries
# DataExtract_NUTS:   https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.ebd.countries_and_nuts&UrbanisationDegree=All Areas (incl.unclassified)&CountryOrTerritory=Italy&Year=2013,2017&DataAggregation=P1Y
# DataExtract_cities: https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.ebd.cities&ScenarioDescription=Baseline%20from%20WHO%202021%20AQG&Year=2022&Sex=Total#

# packages
library(readr)
library(tidyr)
library(glue)

# NUTS
dat <- read_csv("out/eea/DataExtract_NUTS.csv")
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
  dd %>% 
    filter(nchar(`NUTS Code`)==4, `Air Pollutant`==pp) %>% 
    write_csv(., glue("out/eea/{pp}_REGIONE_dati_EEA.csv"))
}

# cities
dat <- read_csv("out/eea/DataExtract_cities.csv")
dat %>%
  distinct(
    `City Code`, 
    `City Or Territory`, 
    Year, 
    `Air Pollutant`, 
    `Air Pollution Average [ug/m3]`) %>%
  pivot_wider(
    names_from = Year, 
    values_from = `Air Pollution Average [ug/m3]`) -> dd
for (pp in c("PM2.5","NO2")) {
  dd %>% 
    filter(`Air Pollutant`==pp) %>% 
    write_csv(., glue("out/eea/{pp}_CITTA_dati_EEA.csv"))
}
