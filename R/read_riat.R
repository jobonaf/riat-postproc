library(dplyr)
snap_ms_descr <- function(code) {
  recode(factor(code,levels=1:11,ordered=T),
         `1`="energy",
         `2`="non-industrial combustion",
         `3`="industrial combustion",
         `4`="industr. production",
         `5`="fuels",
         `6`="solvent",
         `7`="road transport",
         `8`="other mobile sources",
         `9`="waste",
         `10`="agriculture",
         .default="[other]")
}

read_activity_details_xls <- function (filein="data/data_prepair_ninfa_er/run/fvg4fvg_pm10popavg_tech_2013/ActivityDetails_point3.xls",
                                       split_region=T) {
  library(readxl)
  dat <- read_excel(filein, sheet = 1, 
                    col_types = c("numeric",rep("text",3),
                                  "numeric","skip",
                                  rep("numeric",11)))
  if(split_region) dat %>% mutate(Region=substr(Sector,2,4), Sector=substr(Sector,7,nchar(Sector)), Macrosector=as.character(snap_ms_descr(as.numeric(`SNAP 1`)))) -> dat
  return(dat)
}

read_maps_aqi <- function(filein="data/data_prepair_ninfa_er/run/fvg4fvg_pm10popavg_tech_2013/post_proc_output_dir/maps_aqi/PM10TP1_point1.csv") {
  read.csv(filein) -> dd
  colnames(dd) <- c("Lon","Lat","Value")
  dd %>% filter(Value!=-999) -> dd
  library(sp)
  coordinates(dd) <- ~Lon+Lat
  proj4string(dd)<-CRS("+init=epsg:4326")
  return(dd)
}

# read_activity_details_csv <- function(path="data/data_prepair_ninfa_er/",
# 				      run="bpa4fvg_pm10popavg_tech_nontech_2025",
# 				      point=3,
# 				      dbgains="DBGains2025.xlsx")  {
# 	filedata <- paste0(path,"/run/",run,"/post_proc_output_dir/emi_cost/msat/msat_point,",
# 			  point,".csv")
# 	fileinfo <- paste0(path,"/db/",dbgains)
# 	dat <- read.csv(filedata)
# 	library(readxl)
# 	tec <- read_xls(fileinfo, sheet = 5)  ## PROBLEMI LETTURA
