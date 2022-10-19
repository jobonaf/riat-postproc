source("R/unbias_riat.R")
source("R/read_riat.R")
library(dplyr)
library(futile.logger)
runs <- c( "fvg4fvg_pm10popavg_tech_2025",
           "fvg4fvg_pm10popavg_tech_nontech_2025",
           "bpa4fvg_pm10popavg_tech_2025",
           "bpa4fvg_pm10popavg_tech_nontech_2025")
codes <- c( "fvg4fvg_tech", 
            "fvg4fvg_tech+nontech",
            "bpa4fvg_tech", 
            "bpa4fvg_tech+nontech")
pollutant <- "PM10"
models <- c("farm_pi","ninfa_er")
refyears <- 2015:2017



Dat <- NULL
for (yy in refyears) {
  for (mm in models) {
    for (pp in 1:5) {
      for (rr in runs) {
        unbias_aqi(baseyear=yy,
                   run=rr,
                   model=mm,
                   point=pp,
                   pollutant=pollutant) %>%
          calc_popexp(.) %>%
          mutate(Model=mm, BaseYear=yy, Point=pp, Run=rr, Pollutant=pollutant) %>%
          mutate(Cost=read_activity_details_xls(filein=glue("data/data_prepair_{mm}/run/{rr}/AD{pp}.xls")) %>% 
                   filter(`CostOverCle[M€]`>0) %>% 
                   summarize(Cost=sum(`CostOverCle[M€]`)) %>%
                   unlist %>% unname %>% c) %>%
          bind_rows(Dat) -> Dat
      }
    }
  }
}
fileout <- glue("scenarios_{pollutant}_summary.rds")
saveRDS(Dat,fileout)

# manage interannual variability
Dat %>% 
  filter(When=="After",Pop>0,Index=="36th highest") %>%
  mutate(Index=paste0(Index," > ",threshold),
         Run=factor(Run, levels = runs, labels = codes)) %>%
  group_by(Index,Point,Run,Model,BaseYear) %>%
  summarize(ExpPop=sum(ExpPop),Cost=mean(Cost),.groups="drop") %>%
  group_by(Index,Point,Run) %>%
  summarize(minCost=min(Cost), 
            maxCost=max(Cost), 
            medCost=median(Cost), 
            minExpPop=min(ExpPop), 
            maxExpPop=max(ExpPop), 
            medExpPop=median(ExpPop), 
            .groups="drop") -> pDat


# plot
library(ggplot2)
source("~/src/rutilante/R/gg_themes.R")
ggplot(data=pDat, mapping = aes(x=medCost, y=medExpPop, col=Run)) +
  geom_pointrange(mapping = aes(ymin=minExpPop, ymax=maxExpPop),alpha=0.8, fatten=2) +
  geom_errorbarh(mapping = aes(xmin=minCost, xmax=maxCost, height=0),alpha=0.8) +
  geom_line(mapping = aes(group=Run),alpha=0.8,size=0.4) +
  facet_wrap("Index") +
  scale_x_sqrt(breaks=c(0,10,20,50,100,200,500), name="cost (MEUR)") +
  scale_y_continuous(breaks=c(0,10,20,50,100,200,500)*10^3, name="exposed population", 
               labels=scales::number) +
  scale_color_discrete(name=NULL)+
  guides(color=guide_legend(ncol=1))+
  theme_fvg()+
  theme(panel.grid.major = element_line(colour="grey80",size=0.15),
        legend.position=c(1,1),
        legend.justification = c(1,1),
        legend.background = element_rect(colour = "transparent", fill = "transparent"))-> p
fileout <- glue("scenarios_{pollutant}_summary.pdf")
ggsave_fvg(p, filename = fileout, width=6, height=4)
