summary_measures <- function(run = "fvg4fvg_pm10popavg_tech_nontech_2025",
                          model = "ninfa_er",
                          pollutant = "PM10",
                          point = 2,
                          thr_technology = 0.01,
                          thr_activity = 0.05) {
  
  
  source("R/read_riat.R")
  library(dplyr)
  library(futile.logger)
  library(glue)
  
  # read the data
  filein <- glue("data/data_prepair_{model}/run/{run}/AD{point}.xls")
  flog.info(glue("Reading {filein}"))
  read_activity_details_xls(filein=glue(filein)) %>%
    mutate(Model=model, Point=point) -> Dat

  # aggregate and keep only most important technologies
  source("R/recode_tech.R")
  library(tidyr)
  library(forcats)
  library(knitr)
  Dat %>%
    gather(Specie,EmiRed,`EmiRedNox[ton]`:`EmiRedSO2[ton]`) %>%
    dplyr::select(Sector:Technology,`CostOverCle[M€]`:EmiRed,`OPT AR`,LowHigh) %>%
    mutate(Specie=recode(Specie,
                         `EmiRedNox[ton]` ="NOx"  ,
                         `EmiRedVoc[ton]` ="VOC"  ,
                         `EmiRedNh3[ton]` ="NH3"  ,
                         `EmiRedPm10[ton]`="PM10" ,
                         `EmiRedPm25[ton]`="PM2.5",
                         `EmiRedSO2[ton]` ="SO2"  ),
           Technology=recode_tech(Technology,expand=T))%>% 
    mutate(Cost=`CostOverCle[M€]`,
           AR=`OPT AR`) %>%
    group_by(Specie) %>%
    mutate(PointSpecie_EmiRed=sum(EmiRed*as.numeric(EmiRed>0)),
           PointSpecie_EmiBal=-sum(EmiRed),
           Point_Cost=sum(Cost)) %>% 
    ungroup() %>%
    filter(EmiRed/PointSpecie_EmiRed>thr_technology) %>%
    group_by(Specie,Region,Macrosector,Sector,Activity,Technology) %>%
    mutate(nLH=n()) %>% 
    ungroup() %>%
    mutate(Technology=ifelse(nLH>1,
                             glue("{Technology}{c(' - areal sources',' - punctual sources')[LowHigh]}"),
                             Technology)) %>%
    group_by(PointSpecie_EmiRed,PointSpecie_EmiBal,Point_Cost,
             Specie,Region,Macrosector,Sector,Activity) %>%
    arrange(desc(EmiRed)) %>% 
    filter(EmiRed>0,AR>0) %>%
    summarise(Technologies=paste(unique(glue("{Technology}","{ifelse(round(AR)>0,glue(' ({round(AR)}%)'),'')}")),collapse = " + "),
              nTech=n(),
              EmiRed=sum(EmiRed),
              Cost = sum(Cost),
              .groups="drop") %>% 
    filter(EmiRed/PointSpecie_EmiRed>thr_activity) %>%
    arrange(Specie,desc(EmiRed)) %>%
    transmute(Run=run,Model=model,
              Point_Cost=signif(Point_Cost,3),Specie,
              PointSpecie_EmiRed=signif(PointSpecie_EmiRed,3),
              PointSpecie_EmiBal=signif(PointSpecie_EmiBal,3),
              EmiRed=signif(EmiRed,2),Cost=signif(Cost,2),
              Region,Macrosector,Sector,Activity,Technologies)-> out
  return(out)
} 

 
latex_measures <- function(tt,specie="PM10") {
  library(knitr)
  library(glue)
  library(dplyr)
  tt %>% filter(Specie==specie) -> tt
  rrun <- gsub("_","\\\\_",unique(tt$Run))
  mmodel <- gsub("_","\\\\_",unique(tt$Model))
  caption <- glue("Main selected measures for [specie] emission reduction in run \\texttt{[rrun]}. ",
                  "Model: \\texttt{[mmodel]}; total cost: [unique(tt$Point_Cost)] MEUR; ",
                  "total reduction of [specie] over CLE: [-unique(tt$PointSpecie_EmiBal)] Mg/y; ",
                  "reduction achieved by these measures: [signif(sum(tt$EmiRed),3)] Mg/y.",
                  .open = "[",.close="]")
  tt %>%
    select(-Run:-PointSpecie_EmiBal) %>% 
    kable(format="latex",caption = caption, 
          align = c("p{0.08\\linewidth}","p{0.08\\linewidth}","l","p{0.1\\linewidth}",
                    "p{0.12\\linewidth}","p{0.1\\linewidth}","p{0.4\\linewidth}"),
          col.names = c("emission reduction (Mg/y)","cost (MEUR)","region","macrosector",
                        "sector","activity","selected measures (and application rates)")) %>%
    gsub("\\\\hline\n","",.)%>%
    gsub("\\|","@{\\\\hspace{0.1cm}}",.)
  
} 


summary_measures("fvg4fvg_pm10popavg_tech_nontech_2025")->tt
tt %>% latex_measures(.,specie="PM10")
tt %>% latex_measures(.,specie="NOx")
tt %>% latex_measures(.,specie="NH3")
tt %>% latex_measures(.,specie="SO2")
tt %>% latex_measures(.,specie="VOC")
