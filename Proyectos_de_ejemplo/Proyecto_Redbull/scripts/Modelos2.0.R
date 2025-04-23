salida_modelos = list() 
library(dplyr)
library(readr)
library(here)
library(caret)
library(rpart)       # Para construir el árbol de decisión
library(rpart.plot)  # Para visualizar el árbol
library(ggplot2)


Redbull <- here("C:/Users/Equipo/Desktop/US/INFORMÁTICA/Redbull/Redbull.csv")
Tabla = read_csv(Redbull)

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
trainIndex <- createDataPartition(Tabla$taquicardia, p = 0.8, list = FALSE)
train_data <- Tabla[trainIndex, ]
test_data <- Tabla[-trainIndex, ]

# Ajustar el modelo de árbol de decisión
modelo_arbol <- rpart(taquicardia ~ bp_before + agegrp + sex, 
                      data = train_data, 
                      method = "class", # method="class" para clasificación
                      control = rpart.control(minsplit = 5, cp = 0.01))  # cp = controlar la poda del árbol (es decir, cuántas divisiones se permiten antes de que el árbol sea considerado demasiado complejo)
                                                                          # cp bajo = árbol con muchas reglas detalladas (puede sobreajustar).
                                                                          # cp alto = árbol más simple (puede subajustar)..
# Visualizar el árbol
rpart.plot(modelo_arbol, type = 2, extra = 104, fallen.leaves = TRUE, 
           box.palette = "RdYlGn", main = "Árbol de Decisión para Niveles de Taquicardia")

# Predicciones sobre el conjunto de prueba
predicciones <- predict(modelo_arbol, newdata = test_data, type = "class")

# Evaluar el rendimiento del modelo
confusion_matrix <- table(Predicción = predicciones, Real = test_data$taquicardia)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Precisión del modelo: ", accuracy, "\n")

# Gráfico de predicciones por grupo de edad
grafica_arbol <- ggplot(data.frame(Real = test_data$taquicardia, Predicción = predicciones, 
                                   bp_after = test_data$bp_after, agegrp = test_data$agegrp), 
                        aes(x = agegrp, y = bp_after, color = Predicción)) +
  geom_tile() +
  scale_color_manual(values = c("Leve" = "#FFD700", "Moderada" = "#FF8C00",
                                "Severa" = "#FF4F58", "Crisis" = "#8B0000")) +
  labs(title = "Predicción de Niveles de Taquicardia por Árbol de Decisión",
       x = "Grupo de Edad",
       y = "Valor de BP After") +
  theme_minimal() +
  theme(legend.position = "top")

print(grafica_arbol)

# salida_modelos$logis$grafica <- grafica_arbol
salida_modelos$arbol$matriz_confu <- grafica_arbol



# ### 2. Árboles de Decisión
# Los árboles de decisión son fáciles de interpretar y pueden manejar relaciones no lineales.


# ### 3. Random Forest
# El Random Forest es robusto y maneja bien la variabilidad en los datos.

# ### 4. Support Vector Machine (SVM)