library(dplyr)
library(tidyverse)
library(ggpubr)
library(scatterplot3d)
install.packages("caret")
library(caret)

# Integrantes
# - Nicolás Henríquez Turner - 20.730.845-5
# - Iván Zúñiga Quiróz       - 20.003.345-0
# - Bastián Loyola Jara      - 20.552.001-5
# - David Morales Perez      - 19.881.480-6

# Dado que Nicolás es el menor de edad se definirá la seed como sus últimos 4 dígitos de su RUN.
set.seed(0845)

# Se lee el archivo de entrada.
datos <- read.csv(file.choose(), encoding = "UTF-8", sep = ";")

# Tamaño de la muestra.
n <- 50

# Dado que la semilla es impar se sacará una muestra de 50 hombres.
datos <- datos%>%filter(Gender == "0")

# Muestra aleatoria.
set.seed(0845)
datos <- datos%>%sample_n(n, replace = FALSE)

# Tomamos aleatoriamente 8 variables predictoras, que se encuentran entre las columnas 1 a 21.
columnas <- 1:21

set.seed(0845)
variablesPredictorias <- sample(columnas,8)
# Las variables predictoras serán: 
# 4-Profundidad del pecho (entre la espina y el esternón a la altura de los pezones) 
# 9-Suma de los diámetros de los tobillos 
# 10-Grosor de los hombros sobre los músculos deltoides
# 11-Grosor del pecho, sobre tejido mamario en mujeres
# 14-Grosor a la altura de las caderas
# 16-Grosor promedio de ambos bíceps, brazos flectados 
# 17-Grosor promedio de ambos antebrazos, brazos extendidos palmas hacia arriba 
# 20-Grosor promedio de la parte más delgada de ambos tobillos

datosVariables <- subset(datos, select= c("Chest.depth",
                                          "Ankles.diameter",
                                          "Shoulder.Girth",
                                          "Chest.Girth",
                                          "Hip.Girth",
                                          "Bicep.Girth",
                                          "Forearm.Girth",
                                          "Ankle.Minimum.Girth"))

# Se elige como variable útil la estatura, ya que está nos permite complementar de mejor la medidas de
# las distintas partes del cuerpo ya que se encuentran en la misma unidad de medida La estatura corresponde
# a la columna 24.
estaturas <- datos$Height 

#--------------------------------- Condiciones ------------------------------------------------------------.
# Con los datos previamente definidos se procede a crear una regresión lineal simple
# Para lo cual se deben cumplir 4 condiciones: 
# 1. Los datos deben presentar una relación lineal.
# 2. La distribución de los residuos debe ser cercana a la normal.
# 3. La variabilidad de los puntos en torno a la línea de mínimos cuadrados debe ser aproximadamente
#    constante.
# 4. Las observaciones deben ser independientes entre sí. Esto significa que no se puede usar regresión
#    lineal con series de tiempo (tema que va más allá de los alcances de este texto).
#-----------------------------------------------------------------------------------------------------------.

#Determinamos el modelo y se aplica el ajuste con la función lm
modelo <- lm(Weight ~ Height , data = datos)
print(summary(modelo))

# Verificación condiciones:

# 1- Se puede observar que los datos presentan una relación lineal, aunque varios puntos
#    parecen estar alejados de la recta ajustada.

p <- ggscatter ( datos, x = "Height", y = "Weight", color = "blue", fill = "blue",
                 xlab = "Peso [Kg]", ylab = "Estatura [cm]")

p <- p + geom_smooth(method = lm ,se = FALSE , colour = "red")
print(p)

# 2- Se observa que la variabilidad de los residuos no es muy grande y en general siguen una distribución
#    cercana a la normal, aunque en algunos gráficos se observan modelos que se comportan como valores atípicos.

# Se crean gráficos para evaluar el modelo.
plot(modelo)

# 3- Se observa que los residuos forman una “banda horizontal" en torno a la línea de valor 0,
#    por lo cual, se espera que exista una variabilidad aproximadamente constante de los residuos.

# 4- Se verifica por el enunciado, ya que se realiza un estudio donde se recolectan medidas medidas anatómicas 
#    de 247 hombres y 260 mujeres.


# Se agregan predictores para el modelo de regresión lineal simple.
modelo <- lm(Weight ~ Height + Chest.depth, data = datos)
print(summary(modelo))

# Se gráfica el modelo ajustado.
g <- scatterplot3d(datos$Height, datos$Chest.depth, datos$Weight, type = "p",
                   highlight.3d = TRUE, pch = 20, xlab = "Peso [Kg]", ylab = "Estatura [cm]", zlab = "Profundidad del pecho [cm]")

g$plane3d(modelo, draw_polygon = TRUE, draw_lines = TRUE)
print(g)


# Se realiza la validación cruzada.
modelo <- train(Weight ~ Height + Chest.depth, data = datos, method = "lm",
                trControl = trainControl(method = "cv", number= 5))

print(summary(modelo))
 

 
