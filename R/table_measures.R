summary_measures <- function(run = "fvg4fvg_pm10popavg_tech_nontech_2025",
                          model = "ninfa_er",
                          pollutant = "PM10",
                          point,
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
    dplyr::select(Sector:Technology,`CostOverCle[M€]`:EmiRed,`CLE AR`,`OPT AR`,LowHigh) %>%
    mutate(Specie=recode(Specie,
                         `EmiRedNox[ton]` ="NOx"  ,
                         `EmiRedVoc[ton]` ="VOC"  ,
                         `EmiRedNh3[ton]` ="NH3"  ,
                         `EmiRedPm10[ton]`="PM10" ,
                         `EmiRedPm25[ton]`="PM2.5",
                         `EmiRedSO2[ton]` ="SO2"  ),
           Technology=recode_tech(Technology,expand=T))%>% 
    mutate(Cost=`CostOverCle[M€]`,
           ARcle=`CLE AR`,
           ARopt=`OPT AR`) %>%
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
    filter(EmiRed>0,ARopt>0) %>%
    summarise(Technologies=paste(unique(glue("{Technology}",
                                             "{ifelse(round(ARopt)>0,glue(' ({round(ARcle)} -> {round(ARopt)}%)'),'')}")),
                                 collapse = " + "),
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
    dplyr::select(-Run:-PointSpecie_EmiBal) %>% 
    kable(format="latex",caption = caption, 
          align = c("p{0.08\\linewidth}","p{0.08\\linewidth}","l","p{0.1\\linewidth}",
                    "p{0.12\\linewidth}","p{0.1\\linewidth}","p{0.4\\linewidth}"),
          col.names = c("emission reduction (Mg/y)","cost (MEUR)","region","macrosector",
                        "sector","activity","selected measures (and variation in the application rates)")) %>%
    gsub("\\\\hline\n","",.)%>%
    gsub("\\|","@{\\\\hspace{0.1cm}}",.)%>%
    gsub("->","\\\\rightarrow",.)
  
} 

np <- 4
rn <- "fvg4fvg_pm10popavg_tech_nontech_2025"
fn <- glue("MainMeasures_{rn}_p{np}.tex")
if(file.exists(fn))file.remove(fn)
summary_measures(rn,point = np)->tt
for (sp in c("PM10","NOx","SO2","VOC","NH3")) {
  tt %>% latex_measures(.,specie=sp) %>% cat(.,file = fn,append = T)
  cat("\n\n",file = fn,append = T)
}
