salida_modelos = list()

library(dplyr)    # Para manipulación de datos
library(readr)    # Para leer archivos CSV
library(here)     # Para manejar rutas de archivos
library(nnet)     # Para la regresión logística multinomial
library(ggplot2)  # Para graficar
library(tidyr)    # Para manipulación de datos
library(splines)  # Para splines

Redbull <- here("C:/Users/Equipo/Desktop/US/INFORMÁTICA/Redbull/Redbull.csv")
Tabla = read_csv(Redbull)

# Establecer una semilla para que la selección aleatoria sea reproducible
set.seed(123)  

Tabla <- Tabla %>%
  # Eliminar la columna 6
  select(-6) %>%
  # Agrupar por sexo y rango de edad y seleccionar 10 individuos aleatorios por grupo
  group_by(sex, agegrp) %>%
  sample_n(10) %>% # Selecciona 10 filas aleatorias por grupo
  ungroup() # Desagrupar los datos

Tabla

# Modelo lineal

modelo_lineal <- lm(bp_after ~ bp_before + agegrp + sex, data = Tabla) # bp_after es la variable que queremos predecir en función de bp_before y edad_rango.

sumario_lineal <- summary(modelo_lineal)

## Predicciones
predicciones <- predict(modelo_lineal, Tabla)

## Crear un data.frame con las predicciones
Tabla$predicciones <- predicciones

## Gráfica: valores reales comparados con predicciones
grafica_lineal <- ggplot(data = Tabla, 
                         aes(x = bp_after, y = predicciones)) +
  geom_point(color = "#1ABC9C", alpha = 0.6, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#2C3E50", linewidth = 1.2, linetype = "solid") +
  labs(
    x = "Valores reales",
    y = "Predicciones",
    title = "Modelo lineal"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

plot(grafica_lineal)

## Gráfica: valores reales comparados con los errores
errores <- Tabla$bp_after - predicciones

## Crear un data.frame con la columna de errores
Tabla_graf <- Tabla %>% 
  mutate(errores = errores, # calcular errores
         grupo_error = case_when(     # definir grupos de errores. case_wen = sirve para crear una nueva variable en función de otras variables.
           abs(errores) <= 5 ~ "Pequeño (≤5)",  # abs: sirve para obtener el valor absoluto de un número.
           abs(errores) <= 15 ~ "Moderado (5-15)",
           TRUE ~ "Grande (>15)"
         ))

## Crear la gráfica
error_lineal <- ggplot(Tabla_graf, aes(x = bp_after, y = errores)) +
  geom_hline(yintercept = 0, color = "#DC143C", linewidth = 1.2) +
  geom_point(aes(color = grupo_error), size = 3, alpha = 0.8) + # puntos más suaves gracias a alpha
  scale_color_manual(
    values = c(
      "Pequeño (≤5)" = "#66FF66",   
      "Moderado (5-15)" = "#FFA500", 
      "Grande (>15)" = "#B22222"    
    ),
    name = "Tamaño del error"
  ) +
  labs(
    x = "Valores reales",
    y = "Errores",
    title = "Valores reales comparado con los errores"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5), # hjust = 0.5: centrar el título
    axis.title = element_text(size = 12),   # Cambiar el tamaño de los ejes
    axis.text = element_text(size = 10),    # Cambiar el tamaño de los ejes
    legend.title = element_text(size = 11), # Cambiar el tamaño de la leyenda
    legend.text = element_text(size = 10)   # Cambiar el tamaño de la leyenda
  )

plot(error_lineal)


# Modelos con splines (interpolación).

## Modelo simplemente comparando la frecuencia cardíaca antes y después.
# Ajustar un modelo con splines
modelo_splines1 <- lm(bp_after ~ bs(bp_before, df = 4), data = Tabla) # variable a la que quieres interpolar y sus grados de libertad.

# Resumen del modelo
sumario_splines1 <- summary(modelo_splines1)

# Crear datos para predicciones
nuevos_datos1 <- data.frame(bp_before = seq(min(Tabla$bp_before), max(Tabla$bp_before), length.out = 100)) # seq sirve para crear una secuencia de números en un rango determinado. length.out = 100 es el número de elementos en la secuencia.

nuevos_datos1$prediccion1 <- predict(modelo_splines1, nuevos_datos1)

# Gráfica
grafica_splines1 <- ggplot() +
  # Puntos reales
  geom_point(data = Tabla, aes(x = bp_before, y = bp_after),
             color = "#77DD77", size = 3, alpha = 0.7) +
  
  # Línea de predicción interpolada
  geom_line(data = nuevos_datos1, aes(x = bp_before, y = prediccion1),
            color = "#C28DCD", linewidth = 1.5) +
  
  labs(
    x = "Frecuencia cardíaca antes",
    y = "Frecuencia cardíaca después",
    title = "Modelo interpolado"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
plot(grafica_splines1)

# Modelo de regresión con splines para bp_before y efectos de agegrp y sex
modelo_splines2 <- lm(bp_after ~ bs(bp_before, df = 4) + agegrp + sex, data = Tabla)

## Resumen del modelo
sumario_splines2 <- summary(modelo_splines2)

## Crear datos para predicciones (ahora incluyendo agegrp y sex)
nuevos_datos2 <- data.frame(bp_before = seq(min(Tabla$bp_before), max(Tabla$bp_before), length.out = 100),
                           agegrp = "30-45",  # Cambiar según el rango de edad que quieras predecir
                           sex = "Male")      # Cambiar según el sexo que quieras predecir

## Predicciones utilizando el modelo ajustado
nuevos_datos2$prediccion2 <- predict(modelo_splines2, nuevos_datos2)

## Gráfica
grafica_splines2 <- ggplot() +
  # Puntos reales
  geom_point(data = Tabla, aes(x = bp_before, y = bp_after),
             color = "#C28DCD", size = 3) +  # Lila pastel más oscuro
  
  # Línea de predicción
  geom_line(data = nuevos_datos2, aes(x = bp_before, y = prediccion2),
            color = "#77DD77", linewidth = 1.5) +  # Verde pastel
  
  labs(
    x = "Frecuencia cardíaca antes",
    y = "Frecuencia cardíaca después",
    title = "Modelo interpolado considerando 
      el rango de edad y el sexo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

plot(grafica_splines2)

# Lista de salida
salida_modelos$lineal$sumario <- sumario_lineal
salida_modelos$lineal$grafica <- grafica_lineal
salida_modelos$lineal$error <- error_lineal
salida_modelos$splines$sumario1 <- sumario_splines1
salida_modelos$splines$grafica1 <- grafica_splines1
salida_modelos$splines$sumario2 <- sumario_splines2
salida_modelos$splines$grafica2 <- grafica_splines2

