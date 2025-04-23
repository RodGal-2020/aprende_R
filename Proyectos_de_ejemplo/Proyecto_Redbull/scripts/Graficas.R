library(readr)
library(ggplot2)
library(dplyr)
library(here)
Redbull <- here("C:/Users/Equipo/Desktop/US/INFORMÁTICA/Redbull/Redbull.csv")
Tabla = read_csv(Redbull)
Tabla = Tabla %>% select(-6) # Eliminamos la columna 6 ya que no aporta información.

# GRÁFICAS:

Tabla %>% mutate(Diferencia = bp_after - bp_before) %>%
  ggplot() +
  aes(x = Diferencia, y = agegrp, fill = agegrp) +
  ylab("Edad") +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes
  ) +
  labs(title = "Suma de la diferencia entre la frecuecuencia cardiaca antes y 
       después de tomar Redbull clasificado por edades",
       fill = "Edad") +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))


Tabla %>% mutate(Diferencia = bp_after - bp_before) %>%
  ggplot() +
  aes(x = Diferencia, y = agegrp, fill = agegrp) +
  ylab("Edad") +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  theme_void() +
  theme_light() +
  labs(title = "Suma de la diferencia entre la frecuecuencia cardiaca antes y
       después de tomar Redbull clasificado por edades",
       fill = "Edad") +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))
# 600 400 200 0 PREGUNTAR POR ESTO

Tabla %>% mutate(Diferencia = bp_after - bp_before) %>%
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
  labs(title = "Distribución de la diferencia entre la frecuecuencia cardiaca 
       antes de tomar Redbull con la de después")


Tabla %>% mutate(Diferencia = bp_after - bp_before) %>%  
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
    title = "Distribución de la diferencia entre la frecuecuencia cardiaca antes de tomar Redbull 
    con la de después, observando donde se posiciona cada clase de edad",
    fill = "Edad"  # Cambiar el nombre de la leyenda aquí
  ) +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))


Tabla %>% mutate(diferencia = bp_after - bp_before) %>%  
  ggplot() +
  geom_density(aes(x = diferencia, fill = agegrp), alpha = 0.5) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes, es decir, el nombre de los ejes
  ) +
  labs(title = "Distribución de la diferencia entre la frecuecuencia cardiaca antes de tomar Redbull 
       con la de después, observando donde se posicona cada clase de edad",
       fill = "Edad"  # Cambiar el nombre de la leyenda aquí
  ) +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))

# Podemos observar que la mayoría de los individuos de entre 30 y 35 años presentan una diferencia de 15 lpm en la frecuencia cardiaca
# Por otro lado, los individuos a quienes más les ha afectado el consumo de Redbull son unos pocos que pertenecen al grupo de edad de entre 46 y 59 años.


Tabla %>% mutate(Diferencia = bp_after - bp_before) %>% 
  ggplot() +
  geom_boxplot(aes(x = Diferencia, fill = agegrp)) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),       # Ejes X e Y
    axis.ticks = element_line(color = "black"),      # Pequeñas marcas de los ejes
    axis.text = element_text(color = "black"),       # Etiquetas de los ejes
    axis.title = element_text(color = "black")       # Títulos de los ejes, es decir, el nombre de los ejes
  ) +
  labs(title = "Distribución de la diferencia entre la frecuecuencia cardiaca antes de tomar Redbull 
       con la de después, observando donde se posicona cada clase de edad", 
       fill = "Edad") +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))


Tabla %>% mutate(Diferencia = bp_after - bp_before) %>% 
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
  labs(title = "Distribución de la diferencia entre la frecuecuencia cardiaca antes de tomar Redbull 
       con la de después, observando donde se posicona cada clase de edad", 
       fill = "Edad") +
  scale_fill_manual(values = c("#E67E22", "#A7FC00", "#4682B4"))


## GRÁFICAS MÁS INTERESANTES:


Tabla %>% mutate(Edad = agegrp) %>%
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
  labs(title = "La forma en la cual afecta Redbull en cada individuo clasificado por edades") 



# Podemos observar que la relación entre la frecuecuencia cardiaca antes y después de tomar Redbull es directa, lo que significa que ambas tienden a aumentar juntas.


## Taquicardia: Leve = 100-120, Moderada = 121-150, Severa = 151-200, Crisis = +200
Tabla %>% 
  mutate(lpm = case_when( # case_when: función que permite clasificar los datos en rangos.
    bp_after >= 201 ~ "Crisis",        # Para Crisis (160+) # ~ : asigna el valor a la variable lpm
    bp_after >= 151 & bp_after < 201 ~ "Severa",  # Para Severa (140-159) 
    bp_after >= 121 & bp_after < 151 ~ "Moderada", # Para Moderada (120-139)
    bp_after >= 100 & bp_after < 121 ~ "Leve",     # Para Leve (100-119)
    TRUE ~ "No Crisis"               # Para No Crisis si no cumple con los rangos # TRUE: si no cumple con ninguna de las condiciones anteriores
  )) %>%
  group_by(agegrp, lpm) %>%
  summarise(n = n(), .groups = "drop") %>% # summarize: función que permite resumir los datos. n: número de individuos. groups: (drop: elimina la agrupación)
  ggplot(aes(x = agegrp, y = n, fill = lpm)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + #stat: tipo de gráfico, position: posición de las barras (dodge: separadas), color: color de los bordes.
  labs(title = "Taquicardia en individuos clasificados por edades después de tomar Redbull",
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

# Podemos observar que la mayoría de los individuos presentan una taquicardia
# severa después de tomar Redbull. Mientras que los individuos de entre 46 y 59 años
# y aquellos de más de 60 años presentan una taquicardia crítica con la misma
# frecuencia.

