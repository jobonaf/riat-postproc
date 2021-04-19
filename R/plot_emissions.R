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
Dat %>%
  group_by(Point,Model,Run) %>%
  mutate(TotalCost=sum(`CostOverCle[M€]`,na.rm=T)) %>%
  ungroup()%>%
  group_by(Point,Run) %>%
  mutate(TotalCost=round(mean(TotalCost,na.rm=T),0)) %>%
  ungroup()%>%
  gather(Specie,EmiRed,`EmiRedNox[ton]`:`EmiRedSO2[ton]`) %>%
  dplyr::select(Sector:Technology,`CostOverCle[M€]`:EmiRed,TotalCost) %>%
  filter(EmiRed!=0) %>%
  mutate(Specie=recode(Specie,
                       `EmiRedNox[ton]` ="NOx",
                       `EmiRedVoc[ton]` ="VOC",
                       `EmiRedNh3[ton]` ="NH3",
                       `EmiRedPm10[ton]`="PM10",
                       `EmiRedPm25[ton]`="PM2.5",
                       `EmiRedSO2[ton]` ="SO2")) -> dd

# emissions extensive plot
source("~/src/rutilante/R/gg_themes.R")
library(stringr)
library(forcats)
library(ggrepel)
library(scales)
fileout <- glue("scenariosEmisRedu_{pollutant}.pdf")
pdf(fileout, width=10,height=8)
for (ss in unique(dd$Specie)) {
  for (rr in unique(dd$Run)) {
    dd %>%
      filter(Specie==ss,Run==rr) %>%
      mutate(SectorActivity=paste(Sector,Activity,sep=": "),
             RegionMacrosector=paste(Region,Macrosector,sep=": ")) %>%
      mutate(TotalEmiRed=sum(EmiRed)) %>%
      group_by(RegionMacrosector) %>%
      filter(sum(EmiRed)>0.01*mean(TotalEmiRed)) %>%
      ungroup() %>%
      group_by(RegionMacrosector,SectorActivity,Model,Point,TotalCost) %>%
      summarize(EmiRed=sum(EmiRed)) %>%
      ungroup()%>%
      group_by(RegionMacrosector,SectorActivity,Point,TotalCost) %>%
      summarize(EmiRed=mean(EmiRed))%>%
      ungroup()%>%
      group_by(RegionMacrosector,TotalCost) %>%
      arrange(desc(EmiRed),.by_group=T) %>%
      mutate(maxSA=first(SectorActivity)) %>%
      ungroup() %>% 
      mutate(maxEmiRed=if_else(maxSA==SectorActivity,EmiRed,0)) %>%
      group_by(RegionMacrosector,Point,TotalCost) %>%
      summarise(EmiRed=sum(EmiRed),Label=first(maxSA),
                EmiRedMaxSA=sum(maxEmiRed)) %>%
      ungroup() %>%
      mutate(Label=if_else(Point==5,paste(str_wrap(Label, width = 20), sep="\n"),""),
             TotalCost=fct_reorder(as.factor(TotalCost),Point,sum))%>%
      arrange(desc(Point)) -> dp
    ggplot(data=dp) +
      geom_col(aes(x=TotalCost,y=EmiRed),fill="grey90",col="grey70") +
      geom_line(aes(x=TotalCost,y=EmiRedMaxSA,group=RegionMacrosector), col="orange") +
      geom_point(aes(x=TotalCost,y=EmiRedMaxSA), col="orange") +
      # geom_text(aes(x=Point,y=EmiRedMaxSA,label=Label),vjust=0, hjust=1, 
      #           nudge_y = mean(dp$EmiRed)*0.05, col="steelblue",
      #           family="Decima WE", size=2.7, lineheight = .8)+
      geom_text_repel(aes(x=TotalCost,y=EmiRedMaxSA,label=Label), col=muted("orange"),
                      family="Decima WE", size=2.7, lineheight = .8, box.padding = 0.5)+
      #scale_x_continuous(breaks=2:5,limits=c(1.5,5.5))+
      labs(title=glue("{ss} emissions reduction"),subtitle=glue("run: {rr}")) +
      #xlab("point along the Pareto curve")+
      xlab("total cost of the scenario (MEUR)")+
      ylab(glue("{ss} emissions reduction (Mg/y)")) +
      facet_wrap("RegionMacrosector", nrow = 2)+
      theme_fvg()+
      theme(panel.grid.major.y = element_line(colour="grey90",size=0.35),
            panel.grid.major.x = element_blank(),
            aspect.ratio = 1) -> p
    print(p)
  }    
}
dev.off()
embed_fonts(fileout)

# emissions synthetic plot
library(RColorBrewer)
ms <- sort(unique(Dat$Macrosector))
cols <- brewer.pal(n = length(ms), name = "Set3")
names(cols) <- ms
fileout <- glue("scenariosEmisRedu_{pollutant}_synthetic.pdf")
pdf(fileout, width=10,height=6)
for (rr in unique(dd$Run)) {
  dd %>%
    filter(Run==rr) %>%
    group_by(Specie,Macrosector,Model,Point,TotalCost) %>%
    summarize(EmiRed=sum(EmiRed)) %>%
    ungroup()%>%
    group_by(Specie,Macrosector,Point,TotalCost) %>%
    summarize(EmiRed=mean(EmiRed))%>%
    ungroup()%>%
    mutate(TotalCost=as.factor(TotalCost),
           Macrosector=as.factor(Macrosector)) -> dp
  ggplot(data=dp) +
    geom_col(aes(x=TotalCost,y=EmiRed,fill=Macrosector),color="grey40") +
    labs(title="",
         subtitle=glue("run: {rr}",
                       "\nmodel{ifelse(length(models)>1,'s','')}: {paste(models,collapse=', ')}")) +
    xlab("total cost of the scenario (MEUR)")+
    ylab(glue("emissions reduction (Mg/y)")) +
    facet_wrap("Specie", nrow = 2, scales = "free_y")+
    #guides(linetype = guide_legend(override.aes = list(fill = "white")))+
    scale_fill_manual(values=cols)+
    theme_fvg()+
    theme(panel.grid.major.y = element_line(colour="grey90",size=0.35),
          panel.grid.major.x = element_blank(),
          aspect.ratio = 1,
          legend.position="right") -> p
  print(p)
}    
dev.off()
embed_fonts(fileout)
