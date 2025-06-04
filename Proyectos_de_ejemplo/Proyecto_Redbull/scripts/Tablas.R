salida_tablas = list() 

library(here)
library(readr)
library(dplyr)

Redbull <- here("C:/Users/Equipo/Desktop/US/INFORMÁTICA/Redbull/Redbull.csv")
Tabla = read_csv(Redbull)
Tabla = Tabla %>% select(-6) # Eliminamos la columna 6 ya que no aporta información.
Tabla

# Tablas de datos
estructura_tabla <- glimpse(Tabla) 

resumen_estadistico <- summary(Tabla)

seis_primeros <- Tabla %>%
  group_by(sex, agegrp) %>% #agrupar según una o más variables (según sexo y edad)
  mutate(diferencia = bp_after - bp_before) %>% #agregar nuevas columnas
  slice_max(diferencia) %>% #seleccionar las filas con los valores más altos
  arrange(desc(diferencia)) #ordena de menor a mayor

rango_edad_y_sexo <- Tabla %>%
  mutate(diferencia = bp_after - bp_before) %>% #agregar nuevas columnas
  filter(agegrp == "46-59") %>% #filtra las filas
  group_by(sex) %>% #agrupar según una o más variables
  slice_max(diferencia) #seleccionar las filas con el valores más altos

rango_edad_5 <- Tabla %>%
  filter(agegrp == "46-59") %>% #filtra las filas 
  arrange(desc(bp_before)) %>% #arrange = ordena de menor a mayor #desc = ordena de forma descendiente
  head(Tabla, n = 5) #muestra n elementos (en este caso n = 5)

# Lista de salida
salida_tablas$estructura_tabla <- estructura_tabla
salida_tablas$resumen_estadistico <- resumen_estadistico
salida_tablas$afecta_mas$seis_primeros <- seis_primeros
salida_tablas$afecta_mas$rango_edad_y_sexo <- rango_edad_y_sexo
salida_tablas$afecta_mas$rango_edad_5 <- rango_edad_5