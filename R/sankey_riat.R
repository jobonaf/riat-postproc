library(dplyr)
source("R/read_riat.R")

prepare_sankey <- function(dat, min_visible_perc=c(Region=5, 
                                                   Macrosector=5, 
                                                   SA=2, 
                                                   Technology=2),reduce=T) {
  library(stringr)
  dat %>% 
    filter(`CostOverCle[M€]`>0) %>%
    mutate(SA=paste(Sector,Activity,sep=", "),#
           Cost=`CostOverCle[M€]`) ->dat
  dat$TCost <- sum(dat$Cost)

  if(reduce) {
    dat %>% 
      group_by(Region) %>% mutate(Region=ifelse(sum(Cost)/TCost*100>min_visible_perc["Region"],Region,"[other]")) %>% ungroup() %>%
      group_by(Macrosector) %>% mutate(Macrosector=ifelse(sum(Cost)/TCost*100>min_visible_perc["Macrosector"],Macrosector,"[other]")) %>% ungroup() %>%
      group_by(SA) %>% mutate(SA=ifelse(sum(Cost)/TCost*100>min_visible_perc["SA"],SA,"[other]")) %>% ungroup() %>%
      group_by(Technology) %>% mutate(Technology=ifelse(sum(Cost)/TCost*100>min_visible_perc["Technology"],Technology,"[other]")) %>% ungroup() -> dat
  }
    
  dat %>%
    group_by(Region,Macrosector,SA,Technology) %>%
    summarize(Cost=sum(Cost), .groups="drop") %>%
    mutate_if(is.character, function(x) paste(str_wrap(x, width = 30), sep="\n"))-> pdat
  return(pdat)
}

plot_sankey <- function(dat,
                        title="DRAFT",#"optimal set of measures to reduce PM10 averaged on FVG population",
                        subtitle="DRAFT",#"investments on Po Valley") {
                        split_regions=length(unique(dat$Region))>1){
  library(ggplot2,lib.loc="/u/arpa/bonafeg/R/x86_64-pc-linux-gnu-library/3.5")
  library(ggalluvial)
  library(ggrepel)
  library(forcats)
  library(scales)
  library(glue)
  source("/u/arpa/bonafeg/src/rutilante/R/gg_themes.R")
  if(split_regions) {
    ggplot(dat,
           aes(y = Cost, 
               axis1 = fct_reorder(Region,Cost,sum),
               axis2 = fct_reorder(Macrosector,Cost,sum),
               axis3 = fct_reorder(SA,Cost,sum),
               axis4 = fct_reorder(Technology,Cost,sum),
               label=Cost
           )) +
      geom_alluvium(width = 0.4, reverse=F, aes(fill=Region), col="grey90")-> p
  } else {
    ggplot(dat,
           aes(y = Cost, 
               axis1 = fct_reorder(Region,Cost,sum),
               axis2 = fct_reorder(Macrosector,Cost,sum),
               axis3 = fct_reorder(SA,Cost,sum),
               axis4 = fct_reorder(Technology,Cost,sum),
               label=Cost
           )) +
      geom_alluvium(width = 0.4, reverse=F, aes(fill=Macrosector), col="grey90")-> p
  }
  p  +
    geom_stratum(width = 0.4, reverse=F, col="grey50", alpha=0.8) +
    geom_text(stat = "stratum", family="Decima WE", size=2.5, lineheight = .7,
              aes(label = paste0(stat(stratum)," (",percent(after_stat(prop), accuracy = 1),")")), reverse=F, 
              check_overlap = F, col="grey20") +
#    geom_text(stat = "flow", nudge_x = 0.2, reverse=F) +
    scale_x_discrete(limits = c("region","macrosector","sector and\nactivity","technology"), 
                     expand = expansion(mult=c(.0, .15)), position="bottom") +
   # scale_fill_manual() +
    labs(title=glue(title),
         subtitle=glue(subtitle)) +
    guides(fill = "none") +
    ylab("cost (MEUR)") +
    theme_fvg() +
    theme(axis.ticks.x=element_blank(),
          panel.border=element_blank(),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          plot.title=element_text(size=16,face="bold"))-> p
  return(p)
}

source("/u/arpa/bonafeg/src/rutilante/R/gg_themes.R")

read_activity_details_xls(paste0("data/data_prepair_ninfa_er/run/fvg4fvg_pm10popavg_tech_2025/AD3.xls")) %>%
  prepare_sankey(.) %>%
  plot_sankey(.,title="{NULL}",
              subtitle=paste0("AQI: PM10, population averaged\ntarget area: FVG\n",
                              "domain of intervention: FVG (technical measures only)",
                              "\nyear: 2025\ncost: {sum(dat$Cost)} MEUR\nmodel: ninfa_er"))%>%
  ggsave_fvg(.,filename="sankey_fvg4fvg_pm10_Tec_2025_p3_ninfa_er.pdf",width=8,height=5)

read_activity_details_xls(paste0("data/data_prepair_farm_pi/run/fvg4fvg_pm10popavg_tech_2025/AD3.xls")) %>%
  prepare_sankey(.) %>%
  plot_sankey(.,title="{NULL}",
              subtitle=paste0("AQI: PM10, population averaged\ntarget area: FVG\n",
                              "domain of intervention: FVG (technical measures only)",
                              "\nyear: 2025\ncost: {sum(dat$Cost)} MEUR\nmodel: farm_pi"))%>%
  ggsave_fvg(.,filename="sankey_fvg4fvg_pm10_Tec_2025_p3_farm_pi.pdf",width=8,height=5)

read_activity_details_xls(paste0("data/data_prepair_ninfa_er/run/bpa4fvg_pm10popavg_tech_2025/AD3.xls")) %>%
  prepare_sankey(.) %>%
  plot_sankey(.,title="{NULL}",
              subtitle=paste0("AQI: PM10, population averaged\ntarget area: FVG\n",
                              "domain of intervention: Po Valley (technical measures only)",
                              "\nyear: 2025\ncost: {sum(dat$Cost)} MEUR\nmodel: ninfa_er"))%>%
  ggsave_fvg(.,filename="sankey_bpa4fvg_pm10_Tec_2025_p3_ninfa_er.pdf",width=8,height=5)

read_activity_details_xls(paste0("data/data_prepair_farm_pi/run/bpa4fvg_pm10popavg_tech_2025/AD3.xls")) %>%
  prepare_sankey(.) %>%
  plot_sankey(.,title="{NULL}",
              subtitle=paste0("AQI: PM10, population averaged\ntarget area: FVG\n",
                              "domain of intervention: Po Valley (technical measures only)",
                              "\nyear: 2025\ncost: {sum(dat$Cost)} MEUR\nmodel: farm_pi"))%>%
  ggsave_fvg(.,filename="sankey_bpa4fvg_pm10_Tec_2025_p3_farm_pi.pdf",width=8,height=5)



