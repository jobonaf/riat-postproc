library(sf)
library(readr)
library(glue)
library(futile.logger)
library(dplyr)
library(tidyr)

sf_use_s2(FALSE) # faster and easier

# read administrative borders
flog.info("Reading borders")
cc <- st_read("/atlas/arpa/bonafeg/data/geo/LimitiAmministrativi/Limiti_2024_WGS84_UTM32N/Com01012024_WGS84.shp")

# filter regions
cc %>% filter(COD_REG %in% c(1,3,5,6,8)) -> cc

# reproject
cc_ll <- st_transform(cc, crs=4326)

# arrange borders data
flog.info("Rearranging borders")
cc_ll %>%
  filter(!is.na(COD_PROV)) %>%
  select(-COD_RIP,-COD_CM:-PRO_COM_T) %>%
  mutate(REG=case_match(COD_REG,1~"PIE",3~"LOM",5~"VEN",6~"FVG",8~"EMR")) %>%
  mutate(CAPOLUOGO=if_else(CC_UTS==1,COMUNE,NA_character_)) %>%
  group_by(COD_PROV) %>%
  mutate(PROVINCIA=first(CAPOLUOGO, na_rm = T)) -> spdat

# function for spatial average
spat_mean <- function(dat, colname, ndigits=2) {
  dat %>% 
    mutate(LevelType=colname, Level=!!sym(colname)) %>%
    select(value,Level,LevelType) %>%
    st_drop_geometry() %>%
    group_by(LevelType,Level) %>%
    summarize(value=mean(value,na.rm=T)) %>%
    ungroup() %>%
    filter(!is.na(value), !is.na(Level)) %>%
    mutate(value=round(value,ndigits)) 
}

# calculate averages
spat_synthesis <- function(
    run, model, poll,
    spdat,
    filein=glue("out/aqi/{run}_{model}_{poll}.csv"),
    levels=c("PROVINCIA","CAPOLUOGO","REG")) {
  
  # read AQI in CSV as written by write_aqi.R (x,y,Value)
  flog.info(glue("Reading AQI in {filein}"))
  pp <- read_csv(filein)
  pp <- st_as_sf(pp, coords = c("x", "y"), crs = 4326)
  
  # spatial join
  st_join(pp, spdat) -> aqdat
  
  # averaging
  out <- NULL
  for(level in levels) {
    flog.info(glue("Averaging on level {level}"))
    spat_mean(aqdat, level) %>%
      bind_rows(out) %>%
      mutate(Run=run, Model=model, Poll=poll)-> out
  }
  return(out)
}

# iterate calculations over all models, runs, pollutants
Dat <- bind_rows(
  spat_synthesis(model="ninfa_er", poll="PM10", run="pniec2030a", spdat=spdat),
  spat_synthesis(model="ninfa_er", poll="NO2",  run="pniec2030a", spdat=spdat),
  spat_synthesis(model="farm_pi",  poll="PM10", run="pniec2030a", spdat=spdat),
  spat_synthesis(model="farm_pi",  poll="NO2",  run="pniec2030a", spdat=spdat),
  spat_synthesis(model="ninfa_er", poll="PM10", run="pniec2030b", spdat=spdat),
  spat_synthesis(model="ninfa_er", poll="NO2",  run="pniec2030b", spdat=spdat),
  spat_synthesis(model="farm_pi",  poll="PM10", run="pniec2030b", spdat=spdat),
  spat_synthesis(model="farm_pi",  poll="NO2",  run="pniec2030b", spdat=spdat)
)

# for each pollutant and each spatial aggregation write CSV
for(pp in unique(Dat$Poll)) {
  for(ll in unique(Dat$LevelType)) {
    fileout <- glue("out/aqi/{pp}_{ll}.csv")
    Dat %>% 
      filter(Poll==pp, LevelType==ll) %>%
      pivot_wider(names_from = c(Run,Model), values_from = value) %>%
      write_csv(., file = fileout)
    flog.info(glue("Written file {fileout}"))
  }
}
