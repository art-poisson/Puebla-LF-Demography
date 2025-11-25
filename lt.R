#**************************************************************************************#
#**************************************************************************************#
#
#                           TRABAJO FINAL CURSO DE DEMOGRAFÍA
#                                       2026-1
#                             FACULTAD DE CIENCIAS, UNAM
#                     TABLAS DE MORTALIDAD PUEBLA 2010, 2019, 2021
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
apv <- fread("data/apv.csv")

# Unión de tablas de Años Persona Vividos y Defunciones ----
lt_input <- setDT(left_join(apv, def, by = c("year", "sex", "age")))

#Misma edad máxima en ambas tablas
max_age <- max(def$age)
lt_input <- lt_input[age <= max_age]

# Cálculo de mx ----
lt_input[ , mx := deaths/N]
lt_input[ , sex := if_else(sex=="male", "m", "f")]
# Tablas de mortalidad nacional - eevv + censales 2010, 2019 ----
lt_output <- data.table()
for( s in c( 'm', 'f' ) ){
  for( y in unique( lt_input$year ) ){
    temp_dt <- lt_input[ sex == s & year == y ]
    temp_lt <-
      lt_abr(x = temp_dt$age,
             mx = temp_dt$mx,
             sex = s) %>%
      setDT %>%
      .[ , year := y ] %>%
      .[ , sex := s ]
    lt_output <-
      rbind(
        lt_output,
        temp_lt[ , .( lt_desc = 'LT PUE',
                      year = y,
                      sex,
                      age = x,
                      mx = round( mx, 6 ),
                      qx = round( qx, 6 ),
                      ax = round( ax, 2 ),
                      lx = round( lx, 0 ),
                      dx = round( dx, 0 ),
                      Lx = round( Lx, 0 ),
                      Tx = round( Tx, 0 ),
                      ex = round( ex, 2 )) ]
      )
  }
}

## Esperanzas de vida al nacer ----
lt_output[ age == 0 ] %>% dcast( year ~ sex, value.var = 'ex' )


## Mortalidad infantil ----
lt_output[ age == 0 ] %>% dcast( year ~ sex, value.var = 'qx' )


write.csv(lt_output, "data/tabla_mortalidad_PUEBLA.csv", row.names = FALSE)
knitr::kable(lt_output, caption = "Tabla de Vida Puebla")




## Gráfica - mx por año y sexo ----
ggplot(lt_input, aes(x = age, y = log(mx), color = sex, group = sex)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ year, ncol = 2) +
  scale_color_manual(
      values = c("m" = "steelblue", "f" = "lightcoral"),
      labels = c("m" = "Hombres", "f" = "Mujeres")
  ) +
  labs(
    title = "Tasa de mortalidad de Puebla por año y sexo",
    x = "Edad",
    y = "log(mx)",
    color = "Sexo"
  ) +
  theme_minimal()


## Gráfica - mx por sexo y año ----
ggplot(lt_input, aes(x = age, y = log(mx), color = factor(year), group = year))+
    geom_line(linewidth = 1) +
    geom_point(size = 1.2) +
    facet_grid(. ~ sex, labeller = as_labeller(c("m" = "Hombres", "f" = "Mujeres"))) + 
    scale_color_manual(
      values = c("2010" = "steelblue", "2019" = "brown"),
      name = "Año"
    )+
  labs(
    title = "Evolución de la tasa de mortalidad",
#   subtitle = "México",
    x = "Edad",
    y = "log(mx)"
  ) +
  theme_minimal()

## Gráfica - qx por sexo y año ----
lt_output %>%
  ggplot() +
  geom_line( aes(x = age, y = qx, color = factor ( year ), group = factor (year)))
  scale_y_1og10 () +
  scale_color_manual(
    values = c(
      "2010" = "#C75DAA", 
      "2019" = "#40E0D0",
      "Other Years" = "gray70"
  ),
  name = "Años"
  ) + 
  facet_wrap( ~ sex, ) +
    labs(color = 'año') +
    theme_classic()+
    ylab("Probabilidad de Muerte (q_x)")+
    xlab("Edad") +
    labs(colour = "Años")
                          
  
  -------
  
  lt_2010_m <- lt_output[year == 2010 & sex == "m"][order(age)]
  lt_2019_m <- lt_output[year == 2019 & sex == "m"][order(age)]
  
  contrib_m_10_19 <- desc(
    lx1 = lt_2010_m$lx,
    Lx1 = lt_2010_m$Lx,
    lx2 = lt_2019_m$lx,
    Lx2 = lt_2019_m$Lx
  )
  
  # Checar que la suma cuadra con la diferencia de e0
  sum(contrib_m_10_19)
  lt_2019_m$ex[1] - lt_2010_m$ex[1]
  
  
  ----
    lt_2010_f <- lt_output[year == 2010 & sex == "f"][order(age)]
  lt_2019_f <- lt_output[year == 2019 & sex == "f"][order(age)]
  
  contrib_f_10_19 <- desc(
    lx1 = lt_2010_f$lx,
    Lx1 = lt_2010_f$Lx,
    lx2 = lt_2019_f$lx,
    Lx2 = lt_2019_f$Lx
  )
  
  -----
    lt_2019_m <- lt_output[year == 2019 & sex == "m"][order(age)]
  lt_2021_m <- lt_output[year == 2021 & sex == "m"][order(age)]
  
  contrib_m_19_21 <- desc(
    lx1 = lt_2019_m$lx,
    Lx1 = lt_2019_m$Lx,
    lx2 = lt_2021_m$lx,
    Lx2 = lt_2021_m$Lx
  )
  
  lt_2019_f <- lt_output[year == 2019 & sex == "f"][order(age)]
  lt_2021_f <- lt_output[year == 2021 & sex == "f"][order(age)]
  
  contrib_f_19_21 <- desc(
    lx1 = lt_2019_f$lx,
    Lx1 = lt_2019_f$Lx,
    lx2 = lt_2021_f$lx,
    Lx2 = lt_2021_f$Lx
  )
  
  ----
    
    decomp_10_19 <- rbind(
      data.table(age = lt_2010_m$age,
                 contrib = contrib_m_10_19,
                 sex = "Hombres",
                 period = "2010–2019"),
      data.table(age = lt_2010_f$age,
                 contrib = contrib_f_10_19,
                 sex = "Mujeres",
                 period = "2010–2019")
    )
  
  
  -----
    
  
  decomp_19_21 <- rbind(
    data.table(age = lt_2019_m$age,
               contrib = contrib_m_19_21,
               sex = "Hombres",
               period = "2019–2021"),
    data.table(age = lt_2019_f$age,
               contrib = contrib_f_19_21,
               sex = "Mujeres",
               period = "2019–2021")
  )
  
  decomp_all <- rbind(decomp_10_19, decomp_19_21)
  
  
  
  
  ---
    
    ggplot(decomp_all,
           aes(x = age, y = contrib, fill = sex)) +
    geom_col(position = "dodge") +
    facet_wrap(~ period, ncol = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Contribución por edad a la diferencia de e₀",
      x = "Edad (grupo quinquenal)",
      y = "Contribución a Δe₀ (años)",
      fill = "Sexo"
    ) +
    theme_minimal()
  