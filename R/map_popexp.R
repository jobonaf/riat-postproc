library(sp,lib.loc="/u/arpa/bonafeg/R/x86_64-pc-linux-gnu-library/3.5/")
library(raster)

map_popexp <- function(run = "fvg4fvg_pm10popavg_tech_nontech_2025",
                       pollutant = "PM10",
                       models = c("farm_pi","ninfa_er"),
                       refyears = 2015:2017) {
  
  source("R/unbias_riat.R")
  source("R/read_riat.R")
  library(dplyr)
  library(futile.logger)
  library(glue)
  
  flog.info(glue("Processing run {run}, {pollutant}:"))
  Dat <- NULL
  for (yy in refyears) {
    for (mm in models) {
      for (pp in 1:5) {
        flog.info(glue("year {yy}, model {mm}, point {pp}"))
        cost <- read_activity_details_xls(filein=glue("data/data_prepair_{mm}/run/{run}/AD{pp}.xls")) %>% 
          filter(`CostOverCle[M€]`>0) %>% 
          summarize(Cost=sum(`CostOverCle[M€]`)) 
        cost <- cost$Cost
        unbias_aqi(baseyear=yy,
                   run=run,
                   model=mm,
                   point=pp,
                   pollutant=pollutant) %>%
          calc_popexp(.) %>%
          mutate(Model=mm, BaseYear=yy, Point=pp, Run=run, Pollutant=pollutant) %>%
          mutate(Cost=cost) %>%
          bind_rows(Dat) -> Dat
      }
    }
  }
  
  # manage interannual variability
  flog.info("Analyzing interannual variability")
  ny <- length(unique(Dat$BaseYear))*length(unique(Dat$Model))
  Dat %>% 
    filter(When=="After",Pop>0) %>%
    mutate(Index=paste0(Index," > ",threshold)) %>%
    group_by(x,y,Index,Point) %>%
    summarize(Pop=first(Pop),
              ProbExp=sum(Exposed)/ny*100,
              minCost=min(Cost), 
              rangeCost=paste(paste(unique(range(Cost)),collapse=" - "),"MEUR"), 
              .groups="drop") -> pDat
  library(forcats)
  pDat$rangeCost <- fct_reorder(pDat$rangeCost, pDat$minCost, min)
  
  # read region
  flog.info("Preparing for plotting")
  library(maptools)
  library(ggplot2)
  library(rgeos)
  fvg <- readShapePoly("/atlas/arpa/bonafeg/data/geo/LimitiAmministrativi/FVG_UTM33/FVG_conSappada_UTM33")
  fvg <- gSimplify(fvg,tol = 1000,topologyPreserve = T)
  fvg <- fortify(fvg)
  
  # plot
  flog.info("Plotting the maps")
  source("~/src/rutilante/R/gg_themes.R")
  fileout <- glue("scenarios_{pollutant}_{run}.pdf")
  pdf(fileout, width=10,height=8)
  for(ii in unique(pDat$Index)) {
    ggplot() +
      geom_polygon(data=fvg,aes(x=long,y=lat,group=group),fill="grey90")+
      geom_point(data=pDat%>%filter(Index==ii), 
                 aes(x=x,y=y,size=Pop,col=ProbExp)) +
      scale_size(range = c(0.2,3), guide = "none") +
      scale_color_viridis_c(direction=1, option="plasma",name="probability\n(%)") +
      labs(subtitle=glue("run: {run}",
                         "\nmodel{ifelse(length(models)>1,'s','')}: {paste(models,collapse=', ')}",
                         "\nreference year{ifelse(length(refyears)>1,'s','')}: {paste(refyears,collapse=', ')}"),
           title=bquote(.(pollutant)~.(ii)~mu*g/m^3))+
      facet_wrap("rangeCost") +
      theme_bw(base_family = "Decima WE") +
      theme(
        text = element_text(family = "Decima WE", color = "grey20"),
        strip.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks.length = unit(0, "pt"))  -> p
    print(p)
    # }
  }
  dev.off()
  embed_fonts(fileout)
}


map_popexp(run = "fvg4fvg_pm10popavg_tech_nontech_2025")
map_popexp(run = "fvg4fvg_pm10popavg_tech_2025")
map_popexp(run = "bpa4fvg_pm10popavg_tech_nontech_2025")
map_popexp(run = "bpa4fvg_pm10popavg_tech_2025")

