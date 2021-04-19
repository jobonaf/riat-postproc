recode_tech <- function(tech,expand=F) {
  library(dplyr)
  case_when(
    substr(tech,1,16)=="Riduzione di due" ~ "Local action plans - temperature reduction in houses",
    substr(tech,1,16)=="Il programma  di" ~ "Trieste steelplant - partial disposal",
    substr(tech,1,16)=="Introduzione di " ~ "Restrictions on ship fuels",
    substr(tech,1,16)=="Per i trasporti " ~ "Local action plans - traffic restrictions",
    substr(tech,1,16)=="l) prevedere, ne" ~ "Best practices in livestock",
    substr(tech,1,16)=="Elaborazione di " ~ "Promote energy audits in SMEs",
    substr(tech,1,16)=="g) prevedere, ne" ~ "Mandatory efficiency standards for wood-burning systems",
    substr(tech,1,16)=="A7.1 Programmare" ~ "Integrated local collective transport systems",
    substr(tech,1,16)=="a) prevedere, ne" ~ "Traffic restrictions in urban areas, up to EURO 5",
    substr(tech,1,16)=="A8.1 Ottimizzazi" ~ "Freight transport optimisation and modal shift",
    substr(tech,1,16)=="C.4: Promoting a" ~ "Promoting ammonia low-emission fertilizers",
    substr(tech,1,16)=="PRTRA - A6.3 Obb" ~ "Promoting renewable energy in buildings",
    substr(tech,1,16)=="A7.11 Introdurre" ~ "Limited traffic areas, pedestrian areas, road pricing",
    substr(tech,1,16)=="PRTRA - A3.1 Pre" ~ "Guidelines to reduce road dust resuspension",
    substr(tech,1,16)=="C.15: Training a" ~ "Training on energy efficiency",
    substr(tech,1,16)=="C.6: Technical a" ~ "Training on biomass heating systems",
    substr(tech,1,16)=="C.11: Rationaliz" ~ "Rationalization of short-range freight logistics",
    substr(tech,1,16)=="C.7: Enhancement" ~ "Qualified chimney sweep",
    substr(tech,1,16)=="C.5: Implementat" ~ "Tool to evaluate livestock emissions",
    substr(tech,1,16)=="C.12: Developmen" ~ "ICT tools for public transport users",
    substr(tech,1,16)=="POR FESR -Azione" ~ "Promoting low-energy public buildings",
    substr(tech,1,16)=="C.16: Near Zero " ~ "Near-zero energy buildings",
    substr(tech,1,34)=="Reformulation of products (stage 2" ~ "Reformulation of products (stage 2)",
    substr(tech,1,34)=="Reformulation of products (stage 3" ~ "Reformulation of products (stage 3)",
    TRUE ~ tech
  ) -> tech
  if(expand) {
    case_when(
      substr(tech,1,15)=="Combination of " ~ tech %>% gsub("_",", ",.) %>% 
        gsub("BF","air scrubber",.) %>% gsub("CS","covered outdoor storage of manure",.) %>% 
        gsub("LNA","low ammonia application",.) %>% gsub("LNF","low nitrogen feed",.) %>% 
        gsub("SA","low emission housing",.),
      TRUE ~ tech
    ) -> tech
  }
  return(tech)
}

