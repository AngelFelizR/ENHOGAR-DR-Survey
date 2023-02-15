# ENHOGAR 2021

In this file we will make sense of the data reported by Ofinal Nacional de Estadística (ONE) of Dominican Republic.

To run this analysis you need to  `renv::restore()` to install the dependecies and `source("R/main.R")`.

## Importing data

To get the data in this project we scrapped the we site, got the `.sav` related to individual answers and save them as `.fst` files to maximize importing performance in the `R/downloading-sav-files.R`.

## Cleaning data

Once we had the data we start to clean the files and create the final report in the `R/main.R` file.

## Report

The report is created by `final-report.Rmd` and render as `.html` in the `output` folder.