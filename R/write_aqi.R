library(glue)
library(readr)
library(dplyr)

write_aqi <- function(
    model="ninfa_er", run="pniec2030a", poll="PM10",
    filebase = glue("data/data_prepair_{model}/run/{run}/post_proc_output_dir/maps_aqi/{poll}TP1_point1.csv"),
    filedelta = glue("data/data_prepair_{model}/run/{run}/post_proc_output_dir/maps_aqi/{poll}TP1_point2_del.csv"),
    fileout = glue("{run}_{model}_{poll}.csv")) {
  bdat <- read.csv(filebase)
  colnames(bdat) <- c("x","y","aqi_base")
  ddat <- read.csv(filedelta)
  colnames(ddat) <- c("x","y","aqi_delta")
  dat <- bind_cols(
    bdat,
    ddat %>% select(aqi_delta)
  ) %>%
    mutate(aqi_scenario=ifelse(aqi_base<0,NA,round(aqi_base+aqi_delta,2)),
           aqi_base=round(aqi_base,2),
           aqi_delta=round(aqi_delta,2)) %>%
    filter(!is.na(aqi_scenario)) %>%
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

write_aqi(model="farm_pi",  poll="PM25", run="pniec2030a")
write_aqi(model="farm_pi",  poll="PM25", run="pniec2030b")

write_aqi(model="ninfa_er", poll="NO2",  run="pniec2030e")
write_aqi(model="ninfa_er", poll="PM10",  run="pniec2030e")
write_aqi(model="ninfa_er", poll="PM25",  run="pniec2030e")

