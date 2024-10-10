
unbias_aqi <- function(baseyear=2015,
                        run="bpa4fvg_pm10popavg_tech_nontech_2025",
                        model="farm_pi",
                        point=3,
                        pollutant="PM10") {
  
  # read kriging
  library(maptools)
  library(glue)
  library(futile.logger)
  source("~/src/carminio/R/read_kriging.R")
  flog.info(glue("Reading kriging {pollutant} {baseyear}"))
  read_kriging_year(path = glue("/lustre/arpa/operative/data/ariareg/databases/airq_reg/1001F0B0D0_{baseyear}/kriging_CivilYear/{baseyear}0101/data/output/"), 
                    year = baseyear, pollutant = pollutant, content=ifelse(pollutant=="PM10","ukriging_wet","ukriging")) -> dk
  
  # read RIAT delta
  source("R/read_riat.R")
  flog.info(glue("Reading RIAT {pollutant} maps, model {model}, point {point}"))
  deltap <- read_maps_aqi(filein = glue("data/data_prepair_{model}/run/{run}/post_proc_output_dir/maps_aqi/{pollutant}TP1_point{point}_delp.csv"))
  deltap <- spTransform(deltap, dk@proj4string)
  
  # interpolate delta to kriging grid
  library(akima)
  flog.info("Interpolating delta")
  dp <- interp(x = coordinates(deltap)[,"Lon"],
               y = coordinates(deltap)[,"Lat"],
               z = deltap@data$Value, 
               xo = unique(coordinates(dk)[,1]), 
               yo = unique(coordinates(dk)[,2]), 
               linear = F, extrap = T)
  flog.info("Unbiasing")
  dat <- dk
  data.frame(x=coordinates(dk)[,1], y=coordinates(dk)[,2],
             Before=unname(dk@data), DeltaP=c(dp$z)) %>% 
    mutate(After=Before*(1+DeltaP*0.01), DeltaP=NULL) -> out
  return(out)
}
  

calc_popexp <- function(dat,
                        files.eqthr=c("PM10.mean-vs-rank36.2009-2018.rds",
                                      "PM10.mean-vs-rank4.2009-2018.rds"),
                        yave_thresholds=c(70,50,40,30,20,15)) {
  # read population
  flog.info("Reading population")
  pop <- readRDS("/atlas/arpa/bonafeg/data/geo/Popolazione/Pop_FARMFVG_2km.rds")
  
  # annual average thresholds
  flog.info("Annual average thresholds")
  library(dplyr)
  library(glue)
  eqthr <- NULL
  for (yat in yave_thresholds) {
    eqthr <- bind_rows(eqthr, data.frame(Index="annual average", threshold=yat, thr.eq.yave=yat))
  }
  
  # read equivalent thresholds
  flog.info("Reading equivalent thresholds")
  for (ff in files.eqthr) {
    m2r <- readRDS(glue("/atlas/arpa/bonafeg/etc/stat-aq-obs/out/mean-vs-rank/{ff}"))
    eqthr <- bind_rows(eqthr, data.frame(Index=glue("{m2r$N}th highest"), threshold=m2r$thresholds_Nth, thr.eq.yave=m2r$thresholds_yave))
  }
  
  # calculate exposed population
  flog.info("Calculating exposed population")
  library(raster)
  library(tidyr)
  coo.pop <- coordinates(pop)*1000
  colnames(coo.pop) <- c("x","y")
  Dat <- dat %>% 
    gather(When,Value,-x,-y) %>%
    left_join(data.frame(coo.pop, Pop=values(pop$population), id=1), by=c("x","y")) %>%
    left_join(eqthr%>%mutate(id=1), by="id") %>%
    dplyr::select(-id) %>%
    mutate(Exposed=Value>thr.eq.yave,
           ExpPop=Pop*as.numeric(Exposed)) -> out
  return(out)
}


stat_popexp <- function(dat) {
  dat %>%
    group_by(When,Nth,thr.Nth) %>%
    summarize(ExpPop=round(sum(ExpPop, na.rm=T),-3)) %>%
    spread(When,ExpPop) -> stat
  return(stat)
}




