
# Libraries ----

library(data.table)
library(fst)

# Importing data ----

Personas2021 <-
  read_fst("data/ENHOGAR_BD_2021_PERSONAS.fst", as.data.table = TRUE)


# Cleaning data ----

## Cleaning col names ----

ColNamesVector <-
  c('Unidad Primera de Muestra (UPM)' = "unidad_primera_muestra",
    'número de vivienda en la muestra' = "nun_vivienda_muestra",
    'Número del hogar' = 'nun_hogar',
    'Número de línea de la persona en el hogar' = 'nun_persona_en_hogar',
    'Regiones de planificación' = 'region_planificacion',
    'Zona de residencia' = 'residencia_zona',
    'Provincia' = 'provincia',
    'Número de orden de la vivienda ocupada en el registro' = 'nun_vivienda_muestra_registro',
    'Día de la entrevista' = 'entrevista_day',
    'Mes de la entrevista' = 'entrevista_month',
    'Año de la entrevista' = 'entrevista_year',
    'Total de miembros del hogar' = 'total_miembros_hogar',
    'Resultado final de la entrevista al elegido' = 'entrevista_resultado',
    'Número de hogares en la vivienda' = 'total_hogares_vivienda',
    '¿Hay alguna otra persona que viva aquí que no haya sido mencionada?' = 'hay_otra_persona',
    
    '¿Es (nombre) varón o hembra?' = 'sexo',
    '¿Cuántos años cumplidos tiene (nombre)?' = 'edad',
    'Dia de nacimiento' = 'nacimiento_day',
    'Mes de nacimiento' = 'nacimiento_month',
    'Año de nacimiento' = 'nacimiento_year',
    '¿Cuál es la relación de parentesco de (nombre) con el jefe o jefa del hogar?' = 'parentesco_jefe',
    '¿Tiene (nombre) acta de nacimiento, es decir, está declarado(a)?' = 'fue_declarado',
    '¿Actualmente está (nombre)...' = 'estado_civil',
    '¿Ha sido (nombre) vacunado contra el Covid-19?' = 'tiene_covid-19_vacuna',
    '¿Cuántas dósis de la vacuna contra el Covid-19 ha recibido (nombre) hasta ahora?' = 'nun_covid-19_vacunas',
    '¿Cuál es la principal razón por la cual (nombre) no se ha vacunado?' = 'razones_no_covid-19_vacunas',
    '¿Tiene o ha sacado (nombre) la cédula de identidad y electoral?' = 'tiene_cedula',
    '¿Tiene (nombre) dificultad para ver, incluso cuando usa lentes?' = 'dificultad_para_leer',
    '¿Tiene (nombre) dificultad para oír, incluso cuando usa su prótesis auditiva?' = 'dificultad_para_oir',
    '¿Tiene (nombre) dificultad para caminar o subir escalones?' = 'dificultad_para_caminar',
    '¿Tiene (nombre) dificultad para recordar o concentrarse?' = 'dificultad_para_recordar',
    '¿Tiene (nombre) dificultad con su cuidado personal, como bañarse o vestirse?' = 'dificultad_para_cuidado_personal',
    'Usando su idioma habitual, ¿Tiene (nombre) dificultad para comunicarse, por ejemplo, para entender o hacerse entender?' = 'dificultad_comunicarse',
    '¿Sabe (nombre) leer y escribir?' = 'sabe_leer',
    '¿Asiste actualmente o asistió (nombre) a una escuela, colegio, universidad o algún programa de educación para la primera infancia?' = 'tiene_educacion_primaria',
    '¿Por qué razón nunca ha asistido (nombre) a la escuela, Colegio o algún programa para la primera infancia?' = 'razon_carencia_educativa',
    '¿Cuál es el nivel educativo más alto al que asiste o asistió (nombre)?' = 'nivel_educativo',
    '¿Cuál es el curso o año más alto que completó (nombre) en ese nivel?' = 'curso_escolar_mas_alto',
    '¿Se matriculó (nombre) en el presenta año escolar o está inscrito en la universidad?' = 'sigue_estudiando',
    '¿El establecimiento al que asiste (nombre) es, público, privado o sami público?' = 'sector_centro_educativo',
    'Durante el año escolar actual, es decir, 2021-2022 ¿En qué modalidad asiste (nombre) las clases?' = 'modalidad_2021-2022_de_clases',
    '¿Estuvo (nombre) inscrito en la escuela, colegio, universidad o algún programa para la primera infancia el año escolar pasado, es decir, en el período escolar 2020-2021?' = 'estudio_2020-2021_anterior',
    'Durante el año escolar anterior, es decir, 2020-2021 ¿Cuál fue el nivel educativo más alto al que asistió (nombre)?' = 'nivel_educativo_2020-2021',
    'Durante el año escolar anterior, es decir, 2020-2021 ¿Cuál es el curso o año de estudio más alto al que asistió (nombre) en ese nivel?' = 'curso_escolar_mas_alto_2020-2021',
    'Durante el año escolar anterior, es decir, 2020-2021 ¿A qué establecimiento asistió (nombre)?' = 'sector_centro_educativo_2020-2021',
    'Durante el año escolar anterior, es decir, 2020-2021 ¿En qué modalidad tomó (nombre) las clases?' = 'modalidad_2020-2021_de_clases',
    'Durante el año escolar anterior, es decir, 2020-2021 ¿Cuál fue el principal dispositivo o aparato digital en el que recibió (nombre) las clases?' = 'dispositivo_tomar_clases_2020-2021',
    'Durante el año escolar anterior, es decir, 2020-2021 ¿Terminó (nombre) ese año escolar?' = 'termino_curso_2020-2021',
    '¿Por qué (nombre) no terminó? el año escolar anterior, es decir, 2020-2021?' = 'razones_desertar_2020-2021',
    '¿Tiene (nombre) alguna computadora de escritorio, una portátil o una tableta de su propiedad?' = 'tiene_computadora_tableta',
    '¿Ha usado (nombre) computadora de escritorio, una portátil o una tableta alguna vez en los últimos tres meses?' = 'tenia_computadora_tableta_ultimos_3meses',
    '¿Tiene (nombre) teléfono celular para uso personal?' = 'tiene_celular',
    '¿Ha usado (nombre) teléfono celular alguna vez en los últimos tres meses?' = 'tenia_celular_ultimos_3meses',
    '¿Ha usado (nombre) Internet alguna vez en los últimos tres meses desde cualquier lugar?' = 'internet_ultimos_3meses',
    '¿Trabajó o realizó (nombre) alguna actividad económica por lo menos una hora la semana pasada?' = 'trabajo_sem_past',
    'Aunque (nombre) no trabajó la semana pasada, ¿Tiene algún empleo, negocio o actividad?' = 'no_trabajo_sem_past_tiene_empleo',
    'La semana pasada, ¿(nombre) cultivó, cosechó o cuidó ganado u otros animales?' = 'cultivo_ganado_sem_past',
    'La semana pasada, ¿Elaboró (nombre) algún producto como artesanía o comida para vender?' = 'alaboro_comida_artesania_sem_past',
    'La semana pasada, ¿Ayudó (nombre) a algún familiar en su negocio, empresa o finca?' = 'ayudo_familiar_sem_past',
    'La semana pasada, ¿(nombre) cosió, planchó, limpió casa, lavó ropa ajena o realizó otra actividad por paga?' = 'trab_domestico_sem_past',
    'La semana pasada, ¿Por qué (nombre) no trabajó?' = 'razones_no_trabajo_sem_past',
    'Durante las últimas cuatro semanas, ¿Ha buscado (nombre) trabajo o estuvo tratando de establecer su propio negocio, actividad económica o empresa?' = 'negocio_propio_4semanas',
    '¿Por qué (nombre) no ha buscado trabajo?' = 'razones_no_buscar_trabajo',
    '¿Pudiera (nombre) haber aceptado un trabajo la semana pasada si le hubieran ofrecido uno?' = 'dispuesta_a_trabajar_sem_past',
    '¿Dispone (nombre) del tiempo y las condiciones necesarias para salir a trabajar?' = 'tiene_condiciones_para_trabajar',
    'La semana pasada, ¿Habría tenido (nombre) el tiempo y las condiciones necesarias para salir a trabajar?' = 'tenia_condiciones_para_trabajar_sem_past',
    '¿Ha trabajado (nombre) antes por paga o ganancia?' = 'ha_trabajado',
    '¿Cuál fue la principal ocupación u oficio que desempeño (nombre) durante la semana pasada o en el último trabajo que tuvo? Describe' = 'principal_ocupacion_last_job',
    '¿Cuál fue la principal ocupación u oficio que desempeño (nombre) durante la semana pasada o en el último trabajo que tuvo? Código' = 'principal_ocupacion_last_job_codigo',
    '¿A qué se dedica la empresa, negocio o institución en la que trabaja (nombre) actualmente o en el último trabajo que tuvo? Descripción' = 'sector_empresa_last_job',
    '¿A qué se dedica la empresa, negocio o institución en la que trabaja (nombre) actualmente o en el último trabajo que tuvo? Código' = 'sector_empresa_last_job_codigo',
    '¿En ese trabajo (nombre) es o era...?' = 'tipo_empleo',
    'Regularmente ¿Cuántas horas a la semana trabaja o trabajó (nombre) en su ocupación principal?' = 'horas_trabajadas_por_semana',
    'Factor de expansión hogares' = 'factor_expansion',
    'Factor de ponderación hogares' = 'factor_ponderacion',
    'Estratos Geográficos' = 'estrato_geografico',
    'Grupo Socio-económico Familiar' = 'grupo_socio-economico')

setnames(Personas2021, ColNamesVector)


## Cleaning values ----

ParentescoVect <-
  fread("data/parestesco-label.text", sep = "\t", colClasses = "character"
  )[,{Parentesco <- Categoría
      names(Parentesco) <- Valor
  return(ParentescoVect)}]

Personas2021[parentesco_jefe %like% "^\\d+$",
             parentesco_jefe := ParentescoVect[parentesco_jefe]]


# Columns por corregir

# '¿Cuál es el curso o año más alto que completó (nombre) en ese nivel?' = 'curso_escolar_mas_alto',
# 'Durante el año escolar anterior, es decir, 2020-2021 ¿Cuál es el curso o año de estudio más alto al que asistió (nombre) en ese nivel?' = 'curso_escolar_mas_alto_2020-2021',


DataExplorer::create_report(Personas2021,
                            output_file = "data/EDA-report.html")

