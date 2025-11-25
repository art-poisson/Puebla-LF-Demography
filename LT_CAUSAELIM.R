#**************************************************************************************#
#**************************************************************************************#
#
#                           TRABAJO FINAL CURSO DE DEMOGRAFÍA
#                                       2026-1
#                             FACULTAD DE CIENCIAS, UNAM
#              TABLAS DE MORTALIDAD PUEBLA 2010, 2019, 2021 CON CAUSA ELIMINADA, 202
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

## Limpieza de gráficas ----
graphics.off()

## Limpieza de memoria ----
rm(list = ls())

## Carga de paquetes y funciones----
source("Script/functions.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

# Carga de tablas de datos ----
def <- fread("data/def.csv")
def_hom <- fread("data/def_hom.csv") # homicidios
apv <- fread("data/apv.csv")

# 2) Unir defunciones totales + homicidios ----
def_join <- merge(def, def_hom,
                  by = c("year","sex","age"),
                  all.x = TRUE)

def_join[is.na(deaths_hom), deaths_hom := 0]

# 3) Defunciones sin homicidios ----
def_join[ , deaths_star := deaths - deaths_hom]

# 4) Unir con APV ----
lt_input_star <- merge(apv, def_join,
                       by = c("year","sex","age"))

max_age <- max(def_join$age)
lt_input_star <- lt_input_star[age <= max_age]

# 5) Calcular mx* ----
lt_input_star[ , mx_star := deaths_star / N]
lt_input_star[ , sex := if_else(sex=="male", "m", "f")]

# 6) Tablas de vida causa eliminada ----
lt_output_star <- data.table()

for (s in c("m","f")) {
  for (y in sort(unique(lt_input_star$year))) {
    temp_dt <- lt_input_star[sex == s & year == y][order(age)]
    temp_lt <-
      lt_abr(
        x   = temp_dt$age,
        mx  = temp_dt$mx_star,
        sex = s
      ) %>%
      setDT() %>%
      .[, year := y] %>%
      .[, sex := s]
    
    lt_output_star <-
      rbind(
        lt_output_star,
        temp_lt[, .(
          year = y,
          sex,
          age = x,
          mx  = round(mx, 6),
          qx  = round(qx, 6),
          ax  = round(ax, 2),
          lx  = round(lx, 0),
          dx  = round(dx, 0),
          Lx  = round(Lx, 0),
          Tx  = round(Tx, 0),
          ex  = round(ex, 2)
        )]
      )
  }
}

# 7) Guardar para el Excel de salida y usar en gráficas ----
fwrite(lt_output_star, "data/tabla_mortalidad_PUEBLA_hom_elim.csv", row.names = FALSE)
