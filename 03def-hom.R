#**************************************************************************************#
#**************************************************************************************#
#
#                           TRABAJO FINAL CURSO DE DEMOGRAFÍA
#                                       2026-1
#                             FACULTAD DE CIENCIAS, UNAM
#                                Causa Eliminada PROCESS
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


# 1) Carga de homicidios del INEGI (solo Puebla) ----
hom <- read_xlsx("data/INEGI_HOM.xls", sheet = 1,
                 range = "A5:F734")

# Ajuste de nombres de columnas según el archivo;
names(hom) <- c("year", "age", "tot", "male", "female", "ns")
setDT(hom)


# 2) Limpiar edades igual que en def_pro ----
hom <- hom[age != "Total" & year != "Total"]

hom[ , age := gsub("Menores de ", "", age)]
hom[ , age := substr(age, 1, 2)]
hom[age == "1 ", age := 0]
hom[age == "1-", age := 1]
hom[age == "5-", age := 5]
hom[age == "No", age := NA]  # si aplica
hom[ , age := as.numeric(age)]

# 3) Pasar tot/male/female a numérico ----
hom[ , tot    := as.numeric(gsub(",", "", tot))]
hom[ , male   := as.numeric(gsub(",", "", male))]
hom[ , female := as.numeric(gsub(",", "", female))]
hom[ , ns     := as.numeric(gsub(",", "", ns))]

# 4) Agregar por year, age, sex (como con def_pro) ----
hom_pro <- hom[ , .(male = sum(male,   na.rm = TRUE),
                    female = sum(female, na.rm = TRUE),
                    ns = sum(ns,       na.rm = TRUE)),
                .(year, age)]

# 5) Imputar no especificado (ns) proporcional, igual que def_pro ----
hom_pro[ , tot := male + female]
hom_pro[ , `:=`(p_male = male / tot,
                p_female = female / tot)]

hom_pro[ , `:=`(
  male_adj   = male   + p_male   * ns,
  female_adj = female + p_female * ns
)]

hom_pro <- hom_pro[ , .(year, age, male = male_adj, female = female_adj)]

# 6) Pasar a formato largo (male/female → sex) ----
hom_pro <- melt.data.table(
  hom_pro,
  id.vars      = c("year", "age"),
  measure.vars = c("male", "female"),
  variable.name = "sex",
  value.name   = "deaths_hom"
)

# 7) Dejar solo 2010, 2019, 2021 y aplicar misma media móvil ----
hom_pro <- hom_pro[year %in% c(2009, 2010, 2011, 2018, 2019, 2021)]

hom_pro[ , year_new := ifelse(year %in% 2009:2011,
                              2010,
                              ifelse(year %in% 2018:2019,
                                     2019,
                                     year))]

hom_pue <- hom_pro[
  , .(deaths_hom = mean(deaths_hom)),
  .(year = year_new, sex, age)
]

# 8) Guardar tabla final de homicidios ----
fwrite(hom_pue, "data/def_hom.csv")

