
# Libraries

library(rvest)
library(data.table)
library(haven)
library(fst)

read_sav_label <- function(file, ...){
  
  LabelData <- 
    read_sav(file = file, ...) 
  
  Data <-
    as.data.table(LabelData) |>
    DT(j = lapply(.SD, \(x) if(any(names(attributes(x)) == "labels")){
                            haven::as_factor(x) |> as.character()
                            }else{ x }  ))
  
  setnames(Data, 
           sapply(LabelData, attr, which = "label"))
  
}


# Importing data

TableLinks <-
  read_html("https://www.one.gob.do/datos-y-estadisticas/") |>
  html_elements(xpath = '//table//td/a[@class = "badget-download v2 align-items-center d-flex"]') |>
  (\(x) data.table(doc = html_elements(x,xpath = '//div[@class="wrap-title"]/h5') |> html_text(),
                   link = html_attr(x, name = "href")))() |>
  DT(link %like% "\\.sav$" &
       doc %like% "20(21|1[987])" &
       like(doc, "ENHOGAR", ignore.case = TRUE) &
       like(doc, "miembro|persona", ignore.case = TRUE))


for(link in TableLinks$link){

  FilePath <-
  regexec(".+/(.+)\\.sav$", link) |> 
    regmatches(x = link) |>
    (\(x) x[[1]][2])() |>
    file.path(dir = "data", file = _) |>
    paste0(".fst")
  
  read_sav_label(file = link) |>
    write_fst(path = FilePath)
    
}

