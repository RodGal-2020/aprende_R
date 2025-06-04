salida_modelos2.0 = list() 
library(dplyr)       # Para manipulación de datos
library(readr)       # Para leer archivos CSV
library(here)        # Para manejar rutas de archivos
library(caret)       # Para crear la partición de entrenamiento y prueba
library(rpart)       # Para construir el árbol de decisión
library(rpart.plot)  # Para visualizar el árbol
library(ggplot2)     # Para graficar
library(tidyr)       # Para manipulación de datos
library(nnet)        # Para regresión logística multinomial


Redbull <- here("C:/Users/Equipo/Desktop/US/INFORMÁTICA/Redbull/Redbull.csv")
Tabla = read_csv(Redbull)

### Árboles de Decisión
# Los árboles de decisión son fáciles de interpretar y pueden manejar relaciones no lineales.

# Establecer una semilla para que la selección aleatoria sea reproducible
set.seed(123)  # Puedes cambiar el número de la semilla para obtener un conjunto diferente de 10 aleatorios

Tabla <- Tabla %>%
  # Eliminar la columna 6
  select(-6) %>%
  # Agrupar por sexo y rango de edad y seleccionar 10 individuos aleatorios por grupo
  group_by(sex, agegrp) %>%
  sample_n(10) %>% # Selecciona 10 filas aleatorias por grupo
  ungroup() # Desagrupar los datos

Tabla
# Taquicardia: Leve = 100-120, Moderada = 121-150, Severa = 151-200, Crisis = +200

##Árboles de decisión

# Convertir las variables categóricas a factores
Tabla$sex <- as.factor(Tabla$sex)
Tabla$agegrp <- factor(Tabla$agegrp, levels = c("30-45", "46-59", "60+"))  
Tabla$taquicardia <- cut(Tabla$bp_after,
                         breaks = c(0, 120, 150, 200, Inf),
                         labels = c("Leve", "Moderada", "Severa", "Crisis"),
                         right = FALSE)

# Dividir los datos en entrenamiento y prueba
set.seed(77)
trainIndex <- createDataPartition(Tabla$taquicardia, p = 0.8, list = FALSE) # p = 0.8 significa que el 80% de los datos se usarán para entrenamiento
train_data <- Tabla[trainIndex, ]
test_data <- Tabla[-trainIndex, ]

# Ajustar el modelo de árbol de decisión
modelo_arbol <- rpart(taquicardia ~ bp_before + agegrp + sex, # rpart: sirve para crear árboles de decisión
                      data = train_data, 
                      method = "class", # method="class" para clasificación
                      control = rpart.control(minsplit = 5, cp = 0.01))  # cp = controlar la poda del árbol (es decir, cuántas divisiones se permiten antes de que el árbol sea considerado demasiado complejo)
                                                                          # cp bajo = árbol con muchas reglas detalladas (puede sobreajustar).
                                                                          # cp alto = árbol más simple (puede subajustar)..
# Visualizar el árbol
arbol_decision <- rpart.plot(modelo_arbol, 
                                 type = 2, 
                                 extra = 104, 
                                 fallen.leaves = TRUE, 
                                 box.palette = list("#FFB74D", "#FF8A80", "#A52A2A"), # Colores para las clases
                                 main = "Árbol de Decisión para Niveles de Taquicardia")

# Predicciones sobre el conjunto de prueba
predicciones <- predict(modelo_arbol, newdata = test_data, type = "class")

# Evaluar el rendimiento del modelo
confusion_matrix <- table(Predicción = predicciones, Real = test_data$taquicardia)

# Convertir el objeto table en data.frame de formato largo directamente
confusion_largo <- as.data.frame(confusion_matrix)

# Graficar con ggplot2
matriz_confu_arbol <- ggplot(confusion_largo, aes(x = Real, y = Predicción, fill = Freq)) + 
  geom_tile(color = "#B0C4DE", linewidth = 0.8) + # Líneas de separación entre celdas
  geom_text(aes(label = Freq), color = "black", size = 5, fontface = "bold") + # Etiquetas de frecuencia
  scale_fill_gradient( # Cambiar el color del relleno
    low = "#FFF5E1",
    high = "#FF7F50",
    name = "Frecuencia"
  ) +
  theme_minimal() + # Estilo visual minimalista. base_family = fuente 
  theme(  # Ajustes estéticos
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Centrar el título
    axis.title = element_text(size = 13, face = "bold"), # Títulos de los ejes
    axis.text = element_text(size = 12), # Texto de los ejes
    panel.grid = element_blank(), # Eliminar la cuadrícula
    legend.position = "right" # Posición de la leyenda
  ) +
  labs( 
    title = "Matriz de Confusión",
    x = "Valor Real",
    y = "Predicción"
  )
plot(matriz_confu_arbol) # Mostrar la matriz de confusión

# Métricas del error
# Calcular la precisión del modelo
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Precisión del modelo: ", accuracy, "\n")

## Gráfico de predicciones por grupo de edad
grafica_arbol <- ggplot(data.frame(Real = test_data$taquicardia, 
                                   Predicción = factor(predicciones, levels = c("Leve", "Moderada", "Severa", "Crisis")), 
                                   bp_after = test_data$bp_after, 
                                   agegrp = test_data$agegrp), 
                        aes(x = agegrp, y = bp_after, color = Predicción)) + #aes: especifica cómo se mapean los datos a los atributos visuales
  
  # geom_jitter para evitar solapamiento + shape 21 para relleno + borde
  geom_jitter(aes(color = Predicción), shape = 21, size = 5, width = 0.25, alpha = 0.9, stroke = 1.2) +
  
  # Líneas verticales para separar grupos (entre 1 y 2, y entre 2 y 3)
  geom_vline(xintercept = c(1.5, 2.5), linetype = "dashed", color = "grey40", linewidth = 1) +
  
  # Colores más oscuros para el borde
  scale_color_manual(
    values = c(
      "Leve" = "#FBC02D",      # Amarillo fuerte
      "Moderada" = "#F57C00",  # Naranja fuerte
      "Severa" = "#E53935",    # Rojo fuerte
      "Crisis" = "#4E0000"     # Rojo oscuro
    ),
    name = "Predicción (Borde)"
  ) +
  
  # Etiquetas y título
  labs(
    title = "Predicción de Niveles de Taquicardia",
    subtitle = "Cada punto representa una persona por grupo de edad y BP posterior",
    x = "Grupo de Edad",
    y = "Presión Arterial Después (BP After)"
  ) +
  
  # Estilo visual minimalista
  theme_minimal(base_size = 14) +
  
  # Ajustes estéticos adicionales
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


print(grafica_arbol)

# Regresión Logística Multinomial con Pruebas de Hipótesis
# Ajustar el modelo
modelo_logit <- multinom(taquicardia ~ bp_before + agegrp + sex, data = Tabla)

# Resumen del modelo con errores estándar y coeficientes
regresion_logistica_multinomial <- summary(modelo_logit)

# Calcular los p-valores manualmente
z <- summary(modelo_logit)$coefficients / summary(modelo_logit)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))

arbol_p_valores <- print(p_values)

# Lista de salida
salida_modelos2.0$arbol$matriz_confu <- matriz_confu_arbol
salida_modelos2.0$arbol$grafica <- grafica_arbol
salida_modelos2.0$arbol$rlm <- regresion_logistica_multinomial
salida_modelos2.0$arbol$p_valores <- arbol_p_valores