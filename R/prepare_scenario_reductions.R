library(readxl)
library(dplyr)

read_reduction <- function(
    region="FRIULI", 
    filein="~/src/riat-postproc/data/reductionPNIEC2030/red_PNIEC2030vsBASE2013.xlsx") {
  dat <- read_excel(filein, sheet = region, col_names = F)
  df <- bind_rows(
    tibble(reduction=unname(unlist(dat[ 3+0:9,14])), region=region, ms=1:10, pollutant="SO2"),
    tibble(reduction=unname(unlist(dat[19+0:9,14])), region=region, ms=1:10, pollutant="NOx"),
    tibble(reduction=unname(unlist(dat[35+0:9,14])), region=region, ms=1:10, pollutant="NH3"),
    tibble(reduction=unname(unlist(dat[ 3+0:9,28])), region=region, ms=1:10, pollutant="PM10"),
    tibble(reduction=unname(unlist(dat[19+0:9,28])), region=region, ms=1:10, pollutant="PM25"),
    tibble(reduction=unname(unlist(dat[35+0:9,28])), region=region, ms=1:10, pollutant="VOC"))
  df
}


## da mettere in C:\Users\Public\RIAT_DATA\<DATASET>\cfg\emission_reduction
library(stringr)
library(glue)
library(tidyr)

write_reduction <- function(
    dat, name, na_repl=0, digits=1,
    storename=str_to_lower(str_replace_all(name, "[[:punct:]]", "_")),
    filename=glue("{storename}.emrd")) {
  df <- expand_grid(
    ms=1:11,
    type=c("Areal","Point"), 
    pollutant=c("NOx","VOC","NH3","PM10","PM25","SO2")) %>%
    left_join(dat) %>%
    select(ms,type,pollutant,reduction) %>%
    replace_na(list(reduction=na_repl)) %>%
    mutate(reduction=paste0("{",round(reduction*100,digits),"}")) %>%
    pivot_wider(names_from = c(type,pollutant), values_from = reduction) %>%
    mutate(ms=paste0("{ms",ms,"}"))
  redu <- paste(paste0("{",apply(df,1,paste,collapse="",sep=""),"}"), 
                collapse="")
  out <- c("# EmissionReduction", "", glue("Name = {name}"),
           glue("StoreName = {storename}"), glue("Matrix = {redu}"))
  fileout <- file(filename, "w")
  writeLines(out, con = fileout, sep="\n")
  close(fileout)
}

filexl <- "~/src/riat-postproc/data/reductionPNIEC2030/red_PNIEC2030vsBASE2013.xlsx"
sheets <- excel_sheets(filexl)
for (s in sheets) {
  read_reduction(region = s, filein = filexl) %>%
    write_reduction(dat = ., name = glue("PNIEC2030vsBASE2013_{s}"))
}