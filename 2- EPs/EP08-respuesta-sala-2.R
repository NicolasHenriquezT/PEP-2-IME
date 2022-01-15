library(MASS)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(ez)

# Integrantes
# - Nicolás Henríquez Turner - 20.730.845-5
# - Iván Zúñiga Quiróz       - 20.003.345-0
# - Bastián Loyola Jara      - 20.552.001-5
# - David Morales Perez      - 19.881.480-6

# Enunciado:
# El conjunto de datos birthwt del paquete MASS contiene registros de
# 189 nacimientos en un centro médico de Springfield, Massachusetts, en
# 1986. Estos registros incluyen varias variables interesantes. Un equipo 
# médico desea saber si el peso de la madre (en libras) al momento de la última 
# menstruación varía de acuerdo a su raza.

# Variables del conjunto de datos birthwt:
# low = Indicador de peso al nacer menor a 2,5 kg.
# age = Edad de la madre en años.
# lwt = Peso de la madre en libras en el último período menstrual.
# race = Raza de la madre (1 = blanca, 2 = negra, 3 = otra).
# smoke = Fumaba durante el embarazo.
# ptl = Número de partos prematuros previos.
# ht = Historia de hipertensión.
# ui = Presencia de irritabilidad uterina.
# ftv = Número de visitas al médico durante el primer trimestre.
# bwt = Peso al nacer en gramos.

# Solución:

# Se filtra de acuerdo a las variables que se necesitan para este problema, estas 
# son el peso de la madre (lwt) y la raza (race).
datosBirthwt <- birthwt
datos <- select(datosBirthwt, lwt, race)
datos[["race"]] <- factor(datos[["race"]])
datos[["instancia"]] <- factor(1: nrow(datos))

# Para este caso, se han definido las siguientes hipótesis:
# Hipótesis Nula (H0)
# H0: El peso de la madre al momento de la ultima menstruación no varia significativamente de acuerdo a su raza.

# Hipótesis alternativa (HA)
# HA: El peso de la madre al momento de la ultima menstruación varia significativamente de acuerdo a su raza.

# Verificación condiciones para ANOVA:

# 1. La primera condición se verifica, debido a que la medición del peso se realiza mediante una escala de
#    intervalos iguales.
# 2. En el enunciado de este problema se indica que el conjunto de datos a utilizar contiene registros de 189 
#    nacimientos en un centro médico de Springfield, Massachusetts, en 1986. Por lo que se puede afirmar que
#    las k muestras fueron obtenidas de manera aleatoria e independiente.
# 3. Se puede observar en el gráfico Q-Q que existen algunos valores atípicos, por lo que se procederá a 
#    utilizar un nivel de significación pequeño (α = 0.01).

# Comprobación de normalidad. 
g <- ggqqplot(datos,
              x = "lwt",
              y = "race",
              color = "race")

g <- g + facet_wrap(~ race)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# 4. Para verificar la homogeneidad de las varianzas, se comprobara que la razón entre la máxima y mínima 
#    varianza muestral no sea superior a 1,5.

#Raza = 1
datosRaza1 <- datos %>% filter(race == 1)
totalPesos1 <- sum(datosRaza1$lwt)
cantidad1 <- count(datosRaza1)

X1 <- totalPesos1 / cantidad1
S1 <- sum((datosRaza1$lwt - X1)^2) / cantidad1 - 1
  
#Raza = 2
datosRaza2 <- datos %>% filter(race == 2)
totalPesos2 <- sum(datosRaza2$lwt)
cantidad2 <- count(datosRaza2)

X2 <- totalPesos2 / cantidad2
S2 <- sum((datosRaza2$lwt - X2)^2) / cantidad2 - 1

#Raza = 3
datosRaza3 <- datos %>% filter(race == 3)
totalPesos3 <- sum(datosRaza3$lwt)
cantidad3 <- count(datosRaza3)

X3 <- totalPesos3 / cantidad3
S3 <- sum((datosRaza3$lwt - X3)^2) / cantidad3 - 1

# Se observa que la muestra con raza igual a 1 tiene la menor varianza, mientras que la 
# muestra con raza igual a 2 tiene la mayor:
varianzaFinal <- S2 / S1 

# La condición de homocedasticidad no se cumple, ya que la la razón entre la máxima y mínima 
# varianza es igual a 7,04, lo que es mayor a 1,5. Sin embargo creemos que puede existir un error 
# en el desarrollo del calculo de este valor, por lo que se verificara esta condición con la prueba 
# de ezANOVA() que realiza la homocedasticidad de Levene, permitiéndonos comprobar si las k muestras 
# tienen igual varianza.

# Procedimiento ANOVA con aov()
prueba <- aov(lwt ~ race, data = datos)
print(summary(prueba))

# Procedimiento ANOVa con ezANOVA()
prueba2 <- ezANOVA(
  data = datos,
  dv = lwt,
  between = race,
  wid = instancia,
  return_aov = TRUE)

print(prueba2)

# Gráfico del tamaño del efecto.
g2 <- ezPlot(
  data = datos,
  dv = lwt,
  wid = instancia,
  between = race,
  y_lab = "Peso promedio",
  x = race)

print(g2)

 
# Se puede observar con el resultado de la prueba2, que el valor p obtenido para la prueba de homocedasticidad
# de Levene es igual a 0.0151, mínimamente superior a nuestro alfa escogido igual a 0.01. Por lo que podemos
# aceptar la hipótesis nula de la prueba de Levene, la cual indica que las varianzas de las k muestras son iguales.

# Conclusión:
# Finalmente, a través de la prueba ANOVA realizada, el valor p obtenido es igual 0.00034, el cual es inferior
# a nuestro α = 0.01, por lo que rechazamos la hipótesis nula en favor de la hipótesis alternativa. En consecuencia,
# podemos concluir con 99% de confianza que el peso de la madre al momento de la ultima menstruación varia 
# significativamente de acuerdo a su raza.

# Se procede a realizar el procedimiento Post-Hoc para comparar los resultados obtenidos con la prueba de ANOVA.

# Se establece el nivel de significación (mismo que se uso para ANOVA).
alfa <- 0.01

# Procedimiento post-hoc de Bonferoni.
bonferroni <- pairwise.t.test(datos[["lwt"]],
                              datos[["race"]],
                              p.adj = "bonferroni",
                              pool.sd = TRUE,
                              paired = FALSE,
                              conf.level = 1 - alfa)

print(bonferroni)

# Procedimiento post-hoc de Holm.
holm <- pairwise.t.test(datos[["lwt"]],
                        datos[["race"]],
                        p.adj = "holm",
                        pool.sd = TRUE,
                        paired = FALSE, 
                        conf.level = 1 - alfa)

print(holm)

# A pesar de las diferencias de los valores en los procedimientos post-hoc realizados, se puede observar que en ambos
# casos los pesos de las razas 2 y 3 presentan una diferencia significativa al comparar el valor p obtenido con el nivel
# de significación escogido. Esto también se puede evidenciar en el gráfico del tamaño del efecto realizado. Podemos concluir
# con 99% de confianza que el peso de las mujeres de raza 3 es menor que el peso de las mujeres de raza 2.



  