#**************************************************************************************#
#**************************************************************************************#
#
#                           TRABAJO FINAL CURSO DE DEMOGRAFÍA
#                                       2026-1
#                             FACULTAD DE CIENCIAS, UNAM
#                           PRE PROCESS APV 2010, 2019, 2021
#
#         Creado por:               SALGADO BAHENA ARTURO
#         Fecha de creación:        04/11/2025
#         Actualizado por:          
#         Fecha de actualización:   
#         Contacto:                 arturooo@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#

# Preámbulo ----


## Limpieza de memoria ---
rm(list = ls())


## Carga de paquetes y funciones ---

source("script/functions.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

## CARGA DE TABLA DE POBLACIÓN 2010 ----

censo2010 <- read_excel("data/CENSO_PUEBLA_2010.xls", sheet = 1, 
                 range = "A10:F110")

names(censo2010) <- c("reg", "year", "age", 
                "tot", "male", "female")
setDT(censo2010)

# Filtro

censo2010[ , .N, .(age)]

censo2010[grepl("^100 años", age), age := "100"]
censo2010[ , age := substr(age, 1, 2)]
censo2010[age=="1 ", age:=0]
censo2010[age=="1-", age:=1]
censo2010[age=="5-", age:=5]
censo2010[ , age:=as.numeric(age)]


censo2010[ , tot := as.numeric(gsub(",", "", tot))]
censo2010[ , male := as.numeric(gsub(",", "", male))]
censo2010[ , female := as.numeric(gsub(",", "", female))]

censo2010[ , year := 2010L]


censo_pro_2010 <- melt(
  censo2010,
  id.vars = c("year", "age"),
  measure.vars = c("male", "female"),
  variable.name = "sex",
  value.name = "pop"
)[ , .(year, age, sex, pop)]



## CARGA DE TABLA DE POBLACIÓN 2010

censo2020 <- read_excel("data/puebla.xlsx", sheet = 4, 
                        range = "A9:F109")

names(censo2020) <- c("reg", "year", "age", 
                      "tot", "male", "female")
setDT(censo2020)

# Filtro

censo2020[ , .N, .(age)]

censo2020[grepl("^100 años", age), age := "100"]
censo2020[ , age := substr(age, 1, 2)]
censo2020[age=="1 ", age:=0]
censo2020[age=="1-", age:=1]
censo2020[age=="5-", age:=5]
censo2020[ , age:=as.numeric(age)]


censo2020[ , tot := as.numeric(gsub(",", "", tot))]
censo2020[ , male := as.numeric(gsub(",", "", male))]
censo2020[ , female := as.numeric(gsub(",", "", female))]

censo2020[ , year := 2020L]


censo_pro_2020 <- melt(
  censo2020,
  id.vars = c("year", "age"),
  measure.vars = c("male", "female"),
  variable.name = "sex",
  value.name = "pop"
)[ , .(year, age, sex, pop)]


# Guardar tabla de POBLACIÓN----
censos <- rbindlist(list(censo_pro_2010, censo_pro_2020))

# Convertimos age simple → quinquenal, pero solo temporalmente
censos_pro <- censos[
  , .(age = (age %/% 5) * 5, pop, year, sex)
][
  , .(pop = sum(pop)), 
  by = .(year, age, sex)
][order(year, age, sex)]

# guardar si quieres
write.csv(censos_pro, "data/censos_pro.csv", row.names = FALSE)

# -------- FIN ----------*


