plot_measures <- function(run = "fvg4fvg_pm10popavg_tech_nontech_2025",
                          models = "ninfa_er",
                          pollutant = "PM10") {
  
  
  # ONLY ONE RUN, ONE TARGET POLLUTANT, ONE MODEL
  source("R/read_riat.R")
  library(dplyr)
  library(futile.logger)
  library(glue)
  
  # read the data
  Dat <- NULL
  for (mm in models) {
    for (pp in 1:5) {
      filein <- glue("data/data_prepair_{mm}/run/{run}/AD{pp}.xls")
      flog.info(glue("Reading {filein}"))
      read_activity_details_xls(filein=glue(filein)) %>%
        mutate(Model=mm, Point=pp) %>%
        bind_rows(Dat) -> Dat
    }
  }
  
  # prepare for plotting
  source("R/recode_tech.R")
  library(tidyr)
  library(forcats)
  Dat %>%
    group_by(Point) %>% 
    mutate(TotalCost=sum(`CostOverCle[M€]`)) %>%
    ungroup() %>%
    gather(Specie,EmiRed,`EmiRedNox[ton]`:`EmiRedSO2[ton]`) %>%
    dplyr::select(Sector:Technology,`CostOverCle[M€]`:EmiRed) %>%
    filter(EmiRed>0) %>%
    mutate(Specie=recode(Specie,
                         `EmiRedNox[ton]` ="NOx",
                         `EmiRedVoc[ton]` ="VOC",
                         `EmiRedNh3[ton]` ="NH3",
                         `EmiRedPm10[ton]`="PM10",
                         `EmiRedPm25[ton]`="PM2.5",
                         `EmiRedSO2[ton]` ="SO2"),
           Technology=recode_tech(Technology))%>% 
    group_by(SectTech=glue("[{Region}, {Macrosector}: {Sector}] {Technology}"),
             Model,Point,Specie) %>%
    summarize(Cost=sum(`CostOverCle[M€]`),
              TotalCost=mean(TotalCost),
              EmiRed=sum(EmiRed)) %>%
    ungroup() %>%
    group_by(Specie) %>%
    filter(EmiRed>0.05*max(EmiRed)) %>%
    ungroup() %>%
    mutate(Point=as.factor(Point),
           ZeroCost=factor(Cost==0),
           strCost=fct_reorder(glue("{round(TotalCost)} MEUR"),TotalCost,sum))-> dd
  
  
  source("~/src/rutilante/R/gg_themes.R")
  library(stringr)
  library(ggrepel)
  library(scales)
  library(ggplot2)
  fileout <- glue("MainMeasures_{run}_{paste(models,collapse='-')}.pdf")
  pdf(fileout,height=4,width=14)
  for(ss in unique(dd$Specie)) {
    ggplot(dd%>% 
             filter(Specie==ss) %>%
             mutate(SectTech=fct_reorder(SectTech,EmiRed,sum)))+
      geom_pointrange(aes(x=EmiRed, y=SectTech, xmin=0, xmax=EmiRed), 
                      shape=21, col="grey10", size=0.01, fatten=100) +
      geom_point(aes(x=EmiRed, y=SectTech, col=ZeroCost, size=Cost)) +
      facet_wrap("strCost", nrow = 1,drop=FALSE) +
      theme_fvg() +
      ggtitle("",subtitle = glue("{ss} emission reduction\n",
                                 "model{ifelse(length(models)>1,'s','')}: {paste(models,collapse=', ')}\n",
                                 "run: {run}"))+
      xlab("emission reduction (Mg/y)")+
      scale_size_binned_area(limits=range(dd$Cost), trans = "sqrt",
                             breaks=c(0.1,1,10),
                             name="cost (MEUR)")+
      scale_color_manual(values=c(`TRUE`="orange",`FALSE`="grey10"),
                         name="zero cost measure", drop = FALSE)+
      scale_y_discrete(position = "right", name="") +
      theme(legend.position="left", 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.major.x = element_line(colour="grey90",size=0.15),
            panel.grid.major.y = element_blank())+
      scale_x_reverse(limits=c(NA,0))-> p
    print(p)
  }
  dev.off()
  embed_fonts(fileout)
  
}

plot_measures(run = "fvg4fvg_pm10popavg_tech_nontech_2025", models = "ninfa_er")
plot_measures(run = "fvg4fvg_pm10popavg_tech_2025", models = "ninfa_er")
plot_measures(run = "bpa4fvg_pm10popavg_tech_nontech_2025", models = "ninfa_er")
plot_measures(run = "bpa4fvg_pm10popavg_tech_2025", models = "ninfa_er")
plot_measures(run = "fvg4fvg_pm10popavg_tech_nontech_2025", models = "farm_pi")
plot_measures(run = "fvg4fvg_pm10popavg_tech_2025", models = "farm_pi")
plot_measures(run = "bpa4fvg_pm10popavg_tech_nontech_2025", models = "farm_pi")
plot_measures(run = "bpa4fvg_pm10popavg_tech_2025", models = "farm_pi")

