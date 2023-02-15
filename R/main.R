
# Libraries ----

library(data.table)
library(fst)

render_report<- function(input,
                         output){
  
  rmarkdown::render(input = file.path("R",input), 
                    output_file =  output)
  file.copy(file.path("R",output), 
            file.path("output",output),
            overwrite = TRUE)
  file.remove(file.path("R",output))
  
}

# Importing data ----

Personas2021 <-
  read_fst("data/ENHOGAR_BD_2021_PERSONAS.fst",
           as.data.table = TRUE)


# Cleaning data ----

## Cleaning col names ----

ColNamesVector <-
  fread("data/labels/ENHOGAR_BD_2021_PERSONAS_col_names.csv",
        colClasses = "character"
  )[, {name <- new_names
       names(name) <- original_name
       return(name)}]

setnames(Personas2021, ColNamesVector)


## Changing columns from characters to values ----

Personas2021[horas_trabajadas_por_semana %like% "^[A-Za-z ]+$",
             horas_trabajadas_por_semana := NA_character_]

Personas2021[, horas_trabajadas_por_semana :=
               as.integer(horas_trabajadas_por_semana)]


## Adding converting numbers to categories ----

for(variable in list.files("data/labels/", pattern = "\\.text$") ){
  
  correct_vect <-
    fread(file.path("data","labels",variable),
          sep = "\t", colClasses = "character"
    )[,{category <- Categoría
        names(category) <- Valor
        return(category)}]
  
  variable <- sub("\\.text$","",variable)
  
  Personas2021[get(variable) %like% "^\\d+$",
               (variable) := correct_vect[get(variable)]]
  
}


# Creating final report ----

if(!any(dir() == "output")) dir.create("output")

render_report(input = "final-report.Rmd",
              output = "Final Presentation.html")
