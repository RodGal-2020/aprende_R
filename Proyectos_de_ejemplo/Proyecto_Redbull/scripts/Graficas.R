salida_graficas = list() 

library(readr)   # Leer archivos CSV
library(ggplot2) # Crear gráficos
library(dplyr)   # Manipulación de datos
library(here)    # Trabajar con rutas de archivos
library(tidyverse) # Manipulación de datos

Redbull <- here("C:/Users/Equipo/Desktop/US/INFORMÁTICA/Redbull/Redbull.csv")
Tabla = read_csv(Redbull)
Tabla = Tabla %>% select(-6) # Eliminamos la columna 6 ya que no aporta información.

# Gráficas:

grafica_barras <- Tabla %>% mutate(Diferencia = bp_after - bp_before) %>% 
  group_by(agegrp) %>%
  summarise(
    Media = mean(Diferencia), SD = sd(Diferencia)) %>% 
  mutate(Diferencia = fct_reorder(agegrp, Media)) %>% # Reordenar las zonas por la media (mayor a menor)
  ggplot() +
  aes(x = Media, y = Diferencia, fill = Diferencia) + # Ponemos fill para que se llene el color de las barras con los colores de scale_fill_manual
  scale_fill_manual(values = c("#4682B4","#E67E22","#A7FC00")) +
  geom_col() + # geom_col hace barras usando y explícitamente
  geom_errorbarh(aes(xmin = Media - SD, xmax = Media + SD), height = 0.2) + # Añadir barras de error 
  theme_minimal() + # Cambiar el tema
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes
  ) +
  labs(title = "Media de la diferencia entre la frecuencia cardíaca antes y 
       después de tomar Red Bull por edades",
       x = "Diferencia frecuencia cardíaca antes y después",
       y = "Zonas",
       fill = "Edad") 
print(grafica_barras) # Mostrar la gráfica
graficas_sectores <- Tabla %>%
  mutate(Diferencia = bp_after - bp_before) %>%
  group_by(agegrp) %>%
  summarise(Suma_dif = sum(Diferencia, na.rm = TRUE)) %>% # Suma de la diferencia. na.rm = TRUE: eliminar NA
  ggplot(aes(x = "", y = Suma_dif, fill = agegrp)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(
    title = "Suma de la diferencia entre la frecuencia cardíaca antes y 
       después de tomar Red Bull por edades",
    fill = "Edad"
  ) +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))
print(graficas_sectores) # Mostrar la gráfica)
graficas_histroriograma.1 <- Tabla %>% mutate(Diferencia = bp_after - bp_before) %>%
  ggplot() +
  geom_histogram(aes(x = Diferencia), bins = 30, fill = "#87CEEB", color = "black") +
  ylab("Individuos") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes
  ) +
  labs(title = "Diferencia de la frecuencia cardíaca antes y después")

graficas_histroriograma.2 <- Tabla %>% mutate(Diferencia = bp_after - bp_before) %>%  
  ggplot() +
  geom_histogram(aes(x = Diferencia, fill = agegrp), bins = 30) +
  ylab("Individuos") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes, es decir, el nombre de los ejes
  ) +
  labs(
    title = "Diferencia de la frecuencia cardíaca 
        antes y después, según la edad",
    fill = "Edad"  # Cambiar el nombre de la leyenda aquí
  ) +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))


graficas_histroriograma.3 <- Tabla %>% mutate(diferencia = bp_after - bp_before) %>%  
  ggplot() +
  geom_density(aes(x = diferencia, fill = agegrp), alpha = 0.5) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes, es decir, el nombre de los ejes
  ) +
  labs(title = "Diferencia de la frecuencia cardíaca 
        antes y después, según la edad",
       fill = "Edad"  # Cambiar el nombre de la leyenda aquí
  ) +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))

graficas_boxplot <- Tabla %>% mutate(Diferencia = bp_after - bp_before) %>% 
  ggplot() +
  geom_boxplot(aes(x = Diferencia, fill = agegrp)) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes, es decir, el nombre de los ejes
  ) +
  labs(title = "Diferencia de la frecuencia cardíaca 
        antes y después, según la edad", 
       fill = "Edad") +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))


graficas_violines <- Tabla %>% mutate(Diferencia = bp_after - bp_before) %>% 
  ggplot() +
  geom_violin(aes(x = agegrp, y = Diferencia, fill = agegrp)) +
  xlab("Edad") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes, es decir, el nombre de los ejes
  ) +
  labs(title = "Diferencia de la frecuancia cardíaca 
        antes y después, según la edad", 
       fill = "Edad") +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))


## Graficas clave:

graficas_recta <- Tabla %>% mutate(Edad = agegrp) %>%
  ggplot() +
  geom_point(aes(x = bp_before, y = bp_after, color = Edad)) +
  geom_smooth(aes(x = bp_before, y = bp_after), fill = "#A7D8D8", color = "#A7D8D8") + #línea que muestra la tendencia general de los datos
  scale_color_manual(values = c("#E67E22", "#A7FC00", "#4682B4")) +  # Colores personalizados para cada grupo de "agegrp"
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes, es decir, el nombre de los ejes
  ) +
  labs(title = "Efecto del consumo de Red Bull") 

graficas_crisis <- Tabla %>% 
  mutate(lpm = case_when( # case_when: función que permite clasificar los datos en rangos.
    bp_after >= 201 ~ "Crisis",        # Para Crisis (200+) # ~ : asigna el valor a la variable lpm
    bp_after >= 151 & bp_after < 201 ~ "Severa",  # Para Severa (151-200) 
    bp_after >= 121 & bp_after < 151 ~ "Moderada", # Para Moderada (121-150)
    bp_after >= 100 & bp_after < 121 ~ "Leve",     # Para Leve (100-120)
    TRUE ~ "No Crisis"               # Para No Crisis si no cumple con los rangos # TRUE: si no cumple con ninguna de las condiciones anteriores
  )) %>%
  group_by(agegrp, lpm) %>%
  summarise(n = n(), .groups = "drop") %>% # summarize: función que permite resumir los datos. n: número de individuos. groups: (drop: elimina la agrupación)
  ggplot(aes(x = agegrp, y = n, fill = lpm)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + #stat: tipo de gráfico, position: posición de las barras (dodge: separadas), color: color de los bordes.
  labs(title = "Frecuencia del nivel de taquicardia",
       fill = "Nivel de taquicardia") + # fill: nombre de la leyenda
  xlab("Edad") +
  ylab("Individuos") +
  theme_minimal() + # Tema de la gráfica
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes, es decir, el nombre de los ejes
  ) +
  scale_fill_manual(values = c("No Crisis"= "#00A859", "Leve" = "#FFD700", "Moderada" = "#FF8C00", "Severa" = "#FF4F58", "Crisis" = "#8B0000"))#scale_fill_manual: colores personalizados para cada nivel de taquicardia

# Lista de salida
salida_graficas$normales$barras <- grafica_barras 
salida_graficas$normales$sectores <- graficas_sectores
salida_graficas$normales$histograma$sencillo <- graficas_histroriograma.1
salida_graficas$normales$histograma$por_edad <- graficas_histroriograma.2
salida_graficas$normales$histograma$densidad <- graficas_histroriograma.3
salida_graficas$normales$boxplot <- graficas_boxplot
salida_graficas$normales$violines <- graficas_violines
salida_graficas$interesantes$recta <- graficas_recta
salida_graficas$interesantes$crisis <- graficas_crisis