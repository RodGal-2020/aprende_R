library(dplyr)
library(readr)
library(here)
library(nnet)
library(ggplot2)
library(tidyr)

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

# Sabiendo la frecuencia cardiaca y la edad de un individuo, deducir cuanta frecuencia cardiaca tendrá después de tomar Redbull.
# Empezaremos con un modelo lineal predictivo.


## Modelo lineal

modelo_lineal <- lm(bp_after ~ bp_before + agegrp + sex, data = Tabla)
# modelo_lineal <- lm(bp_after ~ sex, data = Tabla)
# bp_after es la variable que queremos predecir en función de bp_before y edad_rango.
summary(modelo_lineal)



# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -10.56867   11.62230  -0.909    0.367    
# bp_before     1.14989    0.07717  14.901   <2e-16 ***
#   agegrp46-59   1.46307    1.95920   0.747    0.458    
# agegrp60+     0.36351    2.04545   0.178    0.860    
# sexMale       0.74063    1.62644   0.455    0.651
# ---
#
# A la vista de los coeficientes anteriores, podemos ver que la frecuencia cardiaca después de
# tomar Redbull aumenta en 1.15 lpm por cada lpm que aumenta la frecuencia cardiaca antes de
# tomarRedbull. Además, la frecuencia cardiaca después de tomar Redbull aumenta en 1.46 lpm si el
# individuo pertenece al grupo de edad de 46-59 años, y aumenta 0.36 lpm si
# el individuo pertenece al grupo de edad de 60 años o más. Por último, sie el individuo
# es un hombre, su frecuencia cardiaca aumentará 0.74 con respecto a una mujer.

# El modelo lineal tendría la siguiente forma:
# y=alfa*bp_before+beta*agegrp46-59+gamma*agegrp60+mu*sexMale
# y=-10.56867+1.14989*bp_before+1.46307*agegrp46-59+0.36351*agegrp60+0.74063*sexMale


## Predicciones
predicciones <- predict(modelo_lineal, Tabla)

# Gráfico: valores reales comparados con predicciones
plot(Tabla$bp_after, predicciones,
     xlab = "Valores reales", ylab = "Predicciones",
     main = "Modelo Lineal:  Valores reales en comparación con las predicciones")
abline(0, 1, col = "cyan", lw = 2) # Agregar líneas rectas a un gráfico. El primer argumento es la ordenada al origen y el segundo es la pendiente.


# Gráfico: valores reales comparado con los errores
errores <- Tabla$bp_after - predicciones
plot(Tabla$bp_after, errores,
     xlab = "Valores reales", ylab = "Errores",
     main = "Modelo Lineal: Valores Reales comparado con los errores")
abline(0, 0, col = "#DC143C", lwd = 2) # Agregar líneas rectas a un gráfico. El primer argumento es la ordenada al origen y el segundo es la pendiente. lwd: grosor de la línea.
# Agregar colores a los puntos en función de la magnitud de los errores
# Diferencia de +-5
points(Tabla$bp_after[abs(errores) <= 5], errores[abs(errores) <= 5], col = "#66FF66", pch = 16)
# Diferencia de +-15
points(Tabla$bp_after[abs(errores) > 5 & abs(errores) <= 15],
       errores[abs(errores) > 5 & abs(errores) <= 15],
       col = "#FFA500", pch = 16)
# Errores mayores a 20 (opcional, si deseas añadir más colores)
points(Tabla$bp_after[abs(errores) > 15], errores[abs(errores) > 15], col = "#B22222", pch = 16)

# abs: sirve para obtener el valor absoluto de un número.
# pch: sirve para cambiar el tipo de punto en un gráfico. 16 corresponde a un círculo sólido.

## Interpolación (splines).

# Modelo simplemente comparando la frecuencia cardiaca antes y después.
# Ajustar un modelo con splines
library(splines)
modelo_splines <- lm(bp_after ~ bs(bp_before, df = 4), data = Tabla) # variable a la que quieres interpolar y sus grados de libertad.

# Resumen del modelo
summary(modelo_splines)

# Crear datos para predicciones
nuevos_datos <- data.frame(bp_before = seq(min(Tabla$bp_before), max(Tabla$bp_before), length.out = 100)) # seq sirve para crear una secuencia de números en un rango determinado. length.out = 100 es el número de elementos en la secuencia.

nuevos_datos$prediccion <- predict(modelo_splines, nuevos_datos)

# Gráfico
plot(Tabla$bp_before, Tabla$bp_after,
     xlab = "Frecuencia cardiaca antes", ylab = "Frecuencia cardiaca después",
     main = "Modelo interpolado")
lines(nuevos_datos$bp_before, nuevos_datos$prediccion, col = "#87CEEB", lwd = 2)


# Modelo de regresión con splines para bp_before y efectos de agegrp y sex
modelo_splines <- lm(bp_after ~ bs(bp_before, df = 4) + agegrp + sex, data = Tabla)

# Resumen del modelo
summary(modelo_splines)

# Crear datos para predicciones (ahora incluyendo agegrp y sex)
# Suponiendo que quieres predecir para todos los valores de agegrp y sex en el rango de bp_before
nuevos_datos <- data.frame(bp_before = seq(min(Tabla$bp_before), max(Tabla$bp_before), length.out = 100),
                           agegrp = "30-45",  # Cambiar según el rango de edad que quieras predecir
                           sex = "Male")      # Cambiar según el sexo que quieras predecir

# Predicciones utilizando el modelo ajustado
nuevos_datos$prediccion <- predict(modelo_splines, nuevos_datos)

# Gráfico
plot(Tabla$bp_before, Tabla$bp_after,
     xlab = "Frecuencia cardiaca antes", ylab = "Frecuencia cardiaca después",
     main = "Modelo interpolado")
lines(nuevos_datos$bp_before, nuevos_datos$prediccion, col = "#87CEEB", lwd = 2)

#Comparando ambas gráficos vemos que varía muy poco, pero ese poco puede ser determinante,
#por lo que es importante tener en cuenta todos los factores posibles.
