salida_tablas = list() 

Redbull <- here("C:/Users/Equipo/Desktop/US/INFORMÁTICA/Redbull/Redbull.csv")
Tabla = read_csv(Redbull)
Tabla = Tabla %>% select(-6) # Eliminamos la columna 6 ya que no aporta información.
Tabla

#Estructura tabla, vista general rápida de la estructura de los datos
glimpse(Tabla) 

#Resúmen estadístico
summary(Tabla)

## Vamos a analizar a aquellos individuos a los que les afecta más tomar Redbull

Tabla %>%
  group_by(sex, agegrp) %>% #agrupar según una o más variables (según sexo y edad)
  mutate(diferencia = bp_after - bp_before) %>% #agregar nuevas columnas
  slice_max(diferencia) %>% #seleccionar las filas con los valores más altos
  arrange(diferencia) #ordena de menor a mayor

# Podemos observar que al individuo al cual le afecta más tomar Redbull es a un hombre de entre 46-59 años.
# Por consiguiente, nos vamos a centrar en dicho rango de edad.

Tabla %>%
  filter(agegrp == "46-59") %>% #filtra las filas
  group_by(sex) %>% #agrupar según una o más variables
  slice_max(bp_after - bp_before) #seleccionar las filas con el valores más altos

# Estos los individuos de entre 46-59 años de cada sexo a los que les afecta mayormente tomar dicha bebida.

Tabla %>%
  filter(agegrp == "46-59") %>% #filtra las filas 
  arrange(desc(bp_before)) %>% #arrange = ordena de menor a mayor #desc = ordena de forma descendiente
  head(Tabla, n = 5) #muestra n elementos (en este caso n = 5)

# Luego estos son los 5 individuos de entre 46-59 años a los que les afecta más el tomar Redbull.

