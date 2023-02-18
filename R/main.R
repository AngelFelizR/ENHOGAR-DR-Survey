
RenderReport <- TRUE

# Libraries ----

library(data.table)
library(fst)
library(lubridate)
library(stringr)

MonthToNumber <- 1:12

names(MonthToNumber) <- 
  c("Enero", "Febrero", "Marzo", 
    "Abril", "Mayo", "Junio", "Julio", 
    "Agosto", "Septiembre", "Octubre", 
    "Noviembre", "Diciembre")

dificultad_case <- function(x){
  
  fcase(x == "?Dir?a que no tiene ninguna dificultad?",
        "01 - Ninguna dificultad",
        x == "?Dir?a que tiene cierta dificultad?",
        "02 - Cierta dificultad",
        x == "?Dir?a que tiene mucha dificultad?",
        "03 - Mucha dificultad",
        x == "?Dir?a que le resulta imposible?",
        "04 - Imposible")
  
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


## Changing columns from characters to values ----


DificultadCols <-
  names(Personas2021) |>
  grep(pattern = "^dificultad", value = TRUE)

Personas2021[, (DificultadCols) := lapply(.SD, dificultad_case),
             .SDcols = DificultadCols]


Personas2021[hay_otra_persona == "S?", hay_otra_persona := "Si"]


Personas2021[sexo == "Var?n", sexo := "Varón"]


Personas2021[tiene_acta_de_nacimiento == "S? tiene", 
             tiene_acta_de_nacimiento := "Si tiene"]


Personas2021[estado_civil == "Separado(a) de uni?n libre?", 
             estado_civil := "Separado(a) de unión libre"]


Personas2021[estado_civil %like% "\\?$", 
             estado_civil := sub(pattern = "\\?$", 
                                 replacement = "", 
                                 x = estado_civil)]
Personas2021[estado_civil == "9", estado_civil := NA_character_]

Personas2021[tipo_empleo == "99", tipo_empleo := NA_character_]
Personas2021[sabe_leer == "9", sabe_leer := NA_character_]


Personas2021[`nun_covid-19_vacunas` == "3 o m?s Dosis", 
             `nun_covid-19_vacunas` := "3 o más Dosis"]


Personas2021[`tiene_cedula` == "S?, tiene", 
             `tiene_cedula` := "Si, tiene"]


Personas2021[horas_trabajadas_por_semana %like% "^[A-Za-z ]+$",
             horas_trabajadas_por_semana := NA_character_]

Personas2021[,`:=`(horas_trabajadas_por_semana = as.integer(horas_trabajadas_por_semana),
                   total_hogares_vivienda = as.integer(total_hogares_vivienda),
                   nun_vivienda_muestra_registro = as.integer(nun_vivienda_muestra_registro),
                   entrevista_day = NULL,
                   entrevista_month = NULL,
                   entrevista_year = as.integer(entrevista_year),
                   entrevista_date = str_c(entrevista_year,
                                           MonthToNumber[entrevista_month], 
                                           entrevista_day,
                                           sep = "-") |> as_date(),
                   nacimiento_day = NULL,
                   nacimiento_month = NULL,
                   nacimiento_year = as.integer(nacimiento_year),
                   nacimiento_date = str_c(nacimiento_year,
                                           MonthToNumber[nacimiento_month], 
                                           nacimiento_day,
                                           sep = "-") |> as_date(),
                   asiste_a_escuela = fcase(asiste_a_escuela == "No asiste, pero asisti?",
                                            "Asistió",
                                            asiste_a_escuela == "S? asiste",
                                            "Asiste",
                                            asiste_a_escuela == "Nunca asisti?",
                                            "Nunca asistió") |>
                                      factor(levels = c("Nunca asistió",
                                                        "Asiste",
                                                        "Asistió")),
                   nivel_educativo = fcase(nivel_educativo == "9",
                                           NA_character_,
                                           nivel_educativo == "Primaria o B?sico",
                                           "Primaria o Básico",
                                           nivel_educativo == "Postgrado, Maestr?a o Doctorado",
                                           "Postgrado, Maestría o Doctorado",
                                           rep(TRUE,.N), nivel_educativo) |>
                                     factor(levels = c("No sabe",
                                                       "Preescolar o Inicial",
                                                       "Primaria o Básico",
                                                       "Secundario o Medio",
                                                       "Universitario o Superior",
                                                       "Postgrado, Maestría o Doctorado")),
                   `grupo_socio-economico` = 
                     factor(`grupo_socio-economico`,
                            levels = c("Muy bajo",
                                       "Bajo",
                                       "Medio bajo",
                                       "Medio y Medio alto",
                                       "Alto")))]


# Adding variables ----

Personas2021[,`:=`(id = .I,
                   edad_entrevista = 
                      interval(nacimiento_date, entrevista_date) |> 
                      time_length(unit = "year"),
                   `Sordera` = dificultad_para_oir == "04 - Imposible")]


# Filtering data to study ----

PersonasSolderaConfirm <-
  Personas2021[!is.na(`Sordera`) &
                 !is.na(sabe_leer)]

# Creating final report ----

if(RenderReport){
  
  if(!any(dir() == "output")) dir.create("output")
  
  rmarkdown::render(input = "R/final-report.Rmd",
                    output_file = "Final Presentation.html",
                    output_dir = "output",
                    envir = .GlobalEnv)
  
}


