#**************************************************************************************#
#**************************************************************************************#
#
#                           TRABAJO FINAL CURSO DE DEMOGRAFÍA
#                                       2026-1
#                             FACULTAD DE CIENCIAS, UNAM
#                                    DEFUNCIONES
#
#         Creado por:               SALGADO BAHENA ARTURO
#         Fecha de creación:        04/11/2025
#         Actualizado por:          
#         Fecha de actualización:   
#         Contacto:                 arturooo@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#

"---------------------------------------------------------"

#Preámbulo ----

## Limpieza de gráficas ----
graphics.off()

##Limpieza de memoria ----
rm(list = ls())

#Carga de paquetes y funciones ----
source("script/functions.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

# Carga de tablas de datos ----
def_pro <- fread("data/def_pro.csv") %>%
        .[year %in% c(2009, 2010, 2011, 2018, 2019, 2021)]

# Media móvil para el año de referencia ----
def_pro[ , year_new := ifelse( year %in% 2009:2011, 
                                    2010,
                                    ifelse( year %in% 2018:2019, 
                                            2019,
                                            year ) ) ]

# Datos preparados de defunciones
def <- 
  def_pro[ , 
           .( deaths = mean( deaths ) ),
           .( year = year_new, sex, age ) ]

#Guardar tabla de DEF ---
write.csv(def, "data/def.csv", row.names = F)

# --------- FIN ----------*


# Preprocesamiento de defunciones 1990-2024----
def <- read_xlsx("data/INEGI_def.xls", sheet = 1, 
                 range = "A5:H740")

names(def) <- c( "year", "age", "" , "reg", 
                "tot", "male", "female", "ns")
setDT(def)

# Filtro
def <- def[age!="Total" & year!="Total" & year>=1990]

def[ , .N, .(age)]

def[ , age := gsub("Menores de ", "", age)]
def[ , age := substr(age, 1, 2)]
def[age=="1 ", age:=0]
def[age=="1-", age:=1]
def[age=="5-", age:=5]
def[age=="No", age:=NA] # prorrateo
def[ , age:=as.numeric(age)]


def[ , tot := as.numeric(gsub(",", "", tot))]
def[ , male := as.numeric(gsub(",", "", male))]
def[ , female := as.numeric(gsub(",", "", female))]
def[ , ns := as.numeric(gsub(",", "", ns))]

# Tabla de defunciones - comprobación
def_comp <- def[ , .(tot=sum(tot, na.rm = T),
                     male=sum(male, na.rm = T), 
                     female=sum(female, na.rm = T),
                     ns=sum(ns, na.rm = T)), 
                 .(year)]

# Imputación
def[year=="No especificado", year:=reg] 
def[ , year:=as.numeric(year)] 
def_comp[ , sum(tot)]


# Tabla final de defunciones 
def_pro <- def[ , .(male=sum(male, na.rm = T), 
                    female=sum(female, na.rm = T),
                    ns=sum(ns, na.rm = T)), 
                .(year, age)]


# Prorrateo de los valores perdidos (missing)
def_pro[ , tot:=male+female][ , `:=`(p_male=male/tot, p_female=female/tot)]
def_pro[ , `:=`(male_adj=male+p_male*ns, female_adj=female+p_female*ns)]
def_pro <- def_pro[ , .(year, age, male=male_adj, female=female_adj)]
sum(def_pro$male)+sum(def_pro$female)


def_pro <- melt.data.table(def_pro, 
                           id.vars = c("year", "age"),
                           measure.vars = c("male", "female"),
                           variable.name = "sex",
                           value.name = "deaths")
sum(def_pro$deaths)
def_pro[ , sum(deaths), .(year, sex)]

#
def_pro <- def_pro[ !is.na(age) ] %>% 
  .[ , p_deaths := deaths / sum(deaths), .(year, sex)] %>% 
  merge( def_pro[ is.na(age), 
                  .(sex, year, na_deaths=deaths)], 
         by = c("sex", "year")) %>% 
  .[ , deaths_adj := deaths + na_deaths * p_deaths] %>% 
  .[ , .(year, sex, age, deaths = deaths_adj) ]

def_gr <- def_pro[ , .(deaths=sum(deaths)), .(year, sex)]


# Guardar tabla de DEF prorrateadas----
write.csv(def_pro, "data/def_pro.csv", row.names = F)

# -------- FIN ----------*