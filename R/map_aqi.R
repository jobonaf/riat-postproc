library("glue")
library("ggplot2")
library(dplyr)

map_aqi <- function(
    model="ninfa_er", run="pniec2030a", poll="PM10", rundesc="PNIEC2030 vs PREPAIR2013",
    filebase = glue("data/data_prepair_{model}/run/{run}/post_proc_output_dir/maps_aqi/{poll}TP1_point1.csv"),
    filedelta = glue("data/data_prepair_{model}/run/{run}/post_proc_output_dir/maps_aqi/{poll}TP1_point2_del.csv"),
    fileout = glue("{run}_{model}_{poll}.pdf"),
    limits=c(NA,NA)) {
  bdat <- read.csv(filebase)
  ddat <- read.csv(filedelta)
  dat <- bind_cols(bdat,ddat)
  dat$value <- dat[,3]+dat[,6]
  dat$value[dat$value < 0] <- NA
  ggplot(dat, aes(x=xutm.km., y=yutm.km., fill=value))+
    geom_tile() +
    scale_fill_continuous(type = "viridis", name=expression(mu*g/m^3), limits=limits) +
    labs(title = glue("{poll} annual mean"), subtitle = glue("RIAT+ {model} ({rundesc})")) +
    theme_bw() + xlab("longitude") +ylab("latitude") -> p
  if(!is.na(fileout)) ggsave(p, filename = fileout, width=8, height=7)
  return(p)
}

# map_aqi(model="ninfa_er", poll="PM10", run="pniec2030a", rundesc="PNIEC2030 vs PREPAIR2013", limits=c(0,35))
# map_aqi(model="ninfa_er", poll="NO2",  run="pniec2030a", rundesc="PNIEC2030 vs PREPAIR2013", limits=c(0,45))
# map_aqi(model="farm_pi",  poll="PM10", run="pniec2030a", rundesc="PNIEC2030 vs PREPAIR2013", limits=c(0,35))
# map_aqi(model="farm_pi",  poll="NO2",  run="pniec2030a", rundesc="PNIEC2030 vs PREPAIR2013", limits=c(0,45))
# map_aqi(model="ninfa_er", poll="PM10", run="pniec2030b", rundesc="PNIEC2030 vs ENEA2010"   , limits=c(0,35))
# map_aqi(model="ninfa_er", poll="NO2",  run="pniec2030b", rundesc="PNIEC2030 vs ENEA2010"   , limits=c(0,45))
# map_aqi(model="farm_pi",  poll="PM10", run="pniec2030b", rundesc="PNIEC2030 vs ENEA2010"   , limits=c(0,35))
# map_aqi(model="farm_pi",  poll="NO2",  run="pniec2030b", rundesc="PNIEC2030 vs ENEA2010"   , limits=c(0,45))

# map_aqi(model="farm_pi",  poll="PM25", run="pniec2030a", rundesc="PNIEC2030 vs PREPAIR2013", limits=c(0,35))
# map_aqi(model="farm_pi",  poll="PM25", run="pniec2030b", rundesc="PNIEC2030 vs ENEA2010"   , limits=c(0,35))

map_aqi(model="ninfa_er", poll="NO2",  run="pniec2030e", rundesc="PNIEC2030 vs PREPAIR2017"   , limits=c(0,45))
map_aqi(model="ninfa_er", poll="PM10",  run="pniec2030e", rundesc="PNIEC2030 vs PREPAIR2017"   , limits=c(0,35))
map_aqi(model="ninfa_er", poll="PM25",  run="pniec2030e", rundesc="PNIEC2030 vs PREPAIR2017"   , limits=c(0,35))
