# read the data
source("R/read_riat.R")
library(dplyr)
library(glue)
library(futile.logger)
runs <- c( "bpa4fvg_pm10popavg_tech_nontech_2025",
           "fvg4fvg_pm10popavg_tech_nontech_2025",
           "bpa4fvg_pm10popavg_tech_2025",
           "fvg4fvg_pm10popavg_tech_2025"
)
models <- c("farm_pi",
            "ninfa_er")
pollutant <- "PM10"
Dat <- NULL
for (mm in models) {
  for (pp in 1:5) {
    for (rr in runs) {
      filein <- glue("data/data_prepair_{mm}/run/{rr}/AD{pp}.xls")
      flog.info(glue("Reading {filein}"))
      read_activity_details_xls(filein=glue(filein)) %>%
        mutate(Model=mm, Point=pp, Run=rr, Pollutant=pollutant) %>%
        bind_rows(Dat) -> Dat
    }
  }
}

# prepare for plotting
library(tidyr)
source("~/src/rutilante/R/gg_themes.R")
library(stringr)
library(forcats)
library(ggrepel)
library(scales)
library(RColorBrewer)
Dat %>%
  mutate(Cost=`CostOverCle[M€]`) %>%
  group_by(Macrosector,Point,Model,Run) %>%
  summarize(Cost=sum(Cost,na.rm=T),.groups="drop") %>%
  group_by(Macrosector,Point,Run) %>%
  summarize(Cost=mean(Cost),.groups="drop") %>%
  group_by(Point,Run) %>%
  mutate(TotalCost=round(sum(Cost))) %>%
  ungroup()%>%
  filter(TotalCost>0,
         Cost>=0.01*TotalCost) %>%
  mutate(TotalCost=as.factor(TotalCost),
         Macrosector=as.factor(Macrosector)) -> dd
ms <- sort(unique(Dat$Macrosector))
cols <- brewer.pal(n = length(ms), name = "Set3")
names(cols) <- ms

# costs synthetic plot
fileout <- glue("scenariosTotCosts_{pollutant}_synthetic.pdf")
pdf(fileout, width=7,height=3)
for (rr in unique(dd$Run)) {
  ggplot(data=dd%>%filter(Run==rr)) +
  geom_col(aes(x=TotalCost,y=Cost,fill=Macrosector),color="grey40") +
  labs(title="",
       subtitle=glue("run: {rr}",
                     "\nmodel{ifelse(length(models)>1,'s','')}: {paste(models,collapse=', ')}")) +
  xlab("total cost of the scenario (MEUR)")+
  ylab(glue("cost (MEUR)")) +
  coord_flip()+
  scale_fill_manual(values=cols)+
  theme_fvg()+
  theme(panel.grid.major.x = element_line(colour="grey90",size=0.35),
        panel.grid.major.y = element_blank(),
        aspect.ratio = 0.4,
        legend.position="right") -> p
  print(p)
}
dev.off()
embed_fonts(fileout)

