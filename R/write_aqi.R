library("glue")
library(readr)
library(dplyr)

write_aqi <- function(
    model="ninfa_er", run="pniec2030a", poll="PM10",
    filebase = glue("data/data_prepair_{model}/run/{run}/post_proc_output_dir/maps_aqi/{poll}TP1_point1.csv"),
    filedelta = glue("data/data_prepair_{model}/run/{run}/post_proc_output_dir/maps_aqi/{poll}TP1_point2_del.csv"),
    fileout = glue("{run}_{model}_{poll}.csv")) {
  bdat <- read.csv(filebase)
  ddat <- read.csv(filedelta)
  dat <- bind_cols(bdat,ddat)
  dat$value <- dat[,3]+dat[,6]
  dat$value[dat$value < 0] <- NA
  dat %>% 
    transmute(x=xutm.km., y=yutm.km., value=round(value,2)) %>%
    filter(!is.na(value)) %>%
    write_csv(., file=fileout)
}

write_aqi(model="ninfa_er", poll="PM10", run="pniec2030a")
write_aqi(model="ninfa_er", poll="NO2",  run="pniec2030a")
write_aqi(model="farm_pi",  poll="PM10", run="pniec2030a")
write_aqi(model="farm_pi",  poll="NO2",  run="pniec2030a")
write_aqi(model="ninfa_er", poll="PM10", run="pniec2030b")
write_aqi(model="ninfa_er", poll="NO2",  run="pniec2030b")
write_aqi(model="farm_pi",  poll="PM10", run="pniec2030b")
write_aqi(model="farm_pi",  poll="NO2",  run="pniec2030b")
