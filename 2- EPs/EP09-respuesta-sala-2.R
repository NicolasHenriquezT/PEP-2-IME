library(tidyverse)
library(ggpubr)
library(ez)
library(nlme)
library(emmeans)

# Integrantes
# - Nicolás Henríquez Turner - 20.730.845-5
# - Iván Zúñiga Quiróz       - 20.003.345-0
# - Bastián Loyola Jara      - 20.552.001-5
# - David Morales Perez      - 19.881.480-6


# Enunciado:
# El siguiente código R carga los datos que aparecen en una tabla que compara las mejores soluciones
# encontradas por cuatro algoritmos para instancias del problema del vendedor viajero con solución óptima
# conocida, tomados desde una memoria de título del DIINF. Con estos datos responda la pregunta de
# investigación: ¿hay algoritmos mejores que otros?

texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 29 18.6 19.3 20.2 22
'brock400_4' 33 16.8 19.3 20.4 22
'C2000.9' 80 57.2 59.6 62.4 66
'c-fat500-10' 126 125 125 125 126
'hamming10-2' 512 343.2 419.1 422.4 512
'johnson32-2-4' 16 16 16 16 16
'keller6' 59 34.5 43 45.5 48.2
'MANN_a81' 1100 1082.2 1082.2 1082.2 1095.9
'p-hat1500-1' 12 6.9 8.1 8.9 10
'p-hat1500-3' 94 75.8 77.7 84.6 86
'san1000' 15 7.6 7.6 7.7 10
'san400_0.7_1' 40 19.6 20.5 20.5 21
'san400_0.9_1' 100 44.1 54.4 56.4 92
'frb100-40' 100 66.4 76.7 80.5 82
'frb59-26-1' 59 39.2 45.9 48.3 48
'1et.2048' 316 232.4 268.4 280.9 292.4
'1zc.4096' 379 273.8 293.2 307.4 328.5
'2dc.2048' 24 15.6 18.7 19.9 21
")

datos <- read.table(textConnection(texto), header = TRUE)

datos <- datos %>% pivot_longer(c("R", "R2", "R3", "G"),
                                names_to = "algoritmo", values_to = "tiempo")


datos[["algoritmo"]] <- factor(datos[["algoritmo"]])

# Solución:

# Para este caso, se han definido las siguientes hipótesis:
# Hipótesis Nula (H0)
# H0: El tiempo de ejecución promedio es igual para los cuatro algoritmos.

# Hipótesis alternativa (HA)
# HA: El tiempo de ejecución promedio es diferente para al menos un algoritmo. 

# Verificación condiciones para ANOVA muestras correlacionadas:

# 1. La primera condición se verifica, debido a que la medición del tiempo se realiza mediante una escala de 
#    intervalos iguales (escala de razón).
# 2. Como el enunciado indica que los datos fueron recopilados de una memoria de titulo del DIINF, podemos
#    suponer que las mediciones al interior de cada grupo son independientes.
# 3. Se puede observar en el gráfico Q-Q que existen algunos valores atípicos, por lo que se procederá a 
#    utilizar un nivel de significación pequeño (α = 0.01).

# Comprobación de normalidad.
g <- ggqqplot(datos, x = "tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap(~ algoritmo)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# 4. La prueba de esfericidad se realizara a través de la función ezANOVA() la cual incluye la prueba de 
#    esfericidad de Mauchly.

# Procedimiento ANOVA con aov().
prueba <- aov(tiempo ~ algoritmo + Error(Instancia/(algoritmo)),
              data = datos)

print(summary(prueba))

# Procedimiento ANOVA con ezANOVA().
prueba2 <- ezANOVA(data = datos, dv = tiempo, within = algoritmo, 
                   wid = Instancia, return_aov = TRUE)

print(summary(prueba2$aov))
cat("Resultado prueba de esfericidad de Mauchly: \n\n")
print(prueba2[["Mauchly's Test for Sphericity"]])

cat("Factores de correccion para cuando no se cumple la condicion de esfericidad: \n\n")
print(prueba2$"Sphericity Corrections")

# Gráfico del tamaño del efecto.  
g2 <- ezPlot(data = datos, dv = tiempo, wid = Instancia, within = algoritmo,
             y_lab = "Tiempo promedio de ejecucion [ms]", x = algoritmo)

print(g2)
 

# Se puede observar en la prueba de esfericidad de Mauchly que el valor p obtenido es muy chico,
# por lo que se analizara las correcciones realizadas.

# Como los datos de nuestro enunciado no cumplen con la esfericidad, consideraremos p[GG] como 
# nuestro p valor de la prueba, este es igual a 0.0243, por lo que podemos verificar que se cumple 
# la cuarta condición.

# Conclusión:
# El valor p obtenido es menor a cualquier nivel de significación típico, para este problema se trabaja
# con un α = 0.01. Debido a esto rechazamos la hipótesis nula en favor de la hipótesis alternativa. Podemos
# concluir con un 99% de confianza que el tiempo de ejecución promedio es diferente para al menos un algoritmo, 
# esto también se puede apreciar en el gráfico del tamaño del efecto realizado.


# Se procede a realizar el procedimiento Post-Hoc para comparar los resultados obtenidos con la prueba de ANOVA.
# Se realizaron los distintos post-hoc en vez de escoger uno, para poder ver como entregaban los resultados 
# y comparar cada uno. 


# Se establece el nivel de significación (mismo que se uso para ANOVA).
alfa <- 0.01

# Procedimiento post-hoc de Bonferoni.
bonferroni <- pairwise.t.test(datos[["tiempo"]], datos[["algoritmo"]],
                              p.adj = "bonferroni", paired = TRUE)
                               
cat("Correccion de Bonferroni\n")
print(bonferroni)

# Procedimiento post-hoc de Holm.
holm <- pairwise.t.test(datos[["tiempo"]], datos[["algoritmo"]],
                        p.adj = "holm", paired = TRUE)

cat("\n\nCorrecion de Holm\n")
print(holm)

# Procedimiento post-hoc HSD de Tukey.
mixto <- lme(tiempo ~ algoritmo, data = datos, random = ~1|Instancia)
medias <- emmeans(mixto, "algoritmo")
tukey <- pairs(medias, adjust = "tukey")

cat("\n\nPrueba HSD de Tukey\n\n")
print(tukey)

# Procedimiento post-hoc de Scheffé
cat("\n\nComparacion de Scheffé\n")
scheffe <- pairs(medias, adjust = "scheffe")
print(scheffe)  

# Con las múltiples pruebas de Tukey realizadas podemos concluir con 99% de confianza, que los algoritmos
# tienen tiempos de ejecución distintos, a excepción de los algoritmos R2 y R3. que presentan el mismo tiempo
# de ejecución medio.


####################################################################################################################

# Pregunta 2
# Enunciado:
# El siguiente código R carga los datos que se obtuvieron en este estudio. Con estos datos, responda la
# siguiente pregunta de investigación: ¿hay diferencias en los tiempos entre tareas y género?

texto2 <- ("
words colors interfer
13 22 47
21 19 44
18 19 31
17 23 34
18 19 44
21 20 38
21 17 35
19 15 31
15 26 42
16 15 29
12 5 44
15 19 38
21 19 32
9 25 38
16 16 36
13 24 29
")

datos2 <- read.table(textConnection(texto2), header = TRUE)

# Solución:

# Para este caso, se han definido las siguientes hipótesis:
# Hipótesis Nula (H0)
# H0: Todas las tareas presentan los mismos tiempos

# Hipótesis alternativa (HA)
# HA: Al menos una tarea presenta un tiempo distinto

# Verificación de condiciones para ANOVA de de una vía para muestras correlacionadas
# 1. La primera condición se verifica, debido a que la medición del tiempo se realiza mediante una escala de
#    intervalos iguales.
# 2. Se puede asumir que las mediciones de cada grupo son independientes, gracias al enunciado
# 3. Se puede observar en el gráfico Q-Q, que solo en un grupo existe un valor atípico por lo
#    que se podría considerar que se sigue una distribución normal.
# 4. Se supondrá que la matriz de varianzas-covarianzas es esférica, para controlar posibles
#    violaciones se hará uso de la función ezANOVA()

#Se define un nivel de significación.
alpha <- 0.01

#Se definen una enumeración para las distintas instancias.
instancia <- factor(1:nrow(datos2))

#Se agrega la columna a los data.frame de los datos.
datos2 <- cbind(datos2,instancia)

#Se realiza un pivote para colocar todos los tipos de tareas, como variables en una columna.
datos2 <- datos2 %>% pivot_longer(c("words", "colors", "interfer"),
                                  names_to = "tareas", values_to = "tiempo")

#Se define la nueva columna como un factor.
datos2[["tareas"]] <- factor(datos2[["tareas"]])

# Comprobación de normalidad mediante un grafico Q-Q.
g <- ggqqplot(datos2, x = "tiempo", y = "tareas", color = "tareas")
g <- g + facet_wrap(~ tareas)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Tal como se había mencionado se realizará una prueba ANOVA mediante la función ezANOVA
# que incluye medidas en contra de las posibles violaciones dadas por la suposición en la esfericidad.
pruebaEz <- ezANOVA(data = datos2, dv = tiempo, within = tareas,
                    wid = instancia, return_aov =TRUE)

# Se imprime el resumen de la prueba.
print(summary(pruebaEz$aov))

cat("El resultado de la prueba de esfericidad de Mauchly:\n")
print(pruebaEz[["Mauchly's Test for Sphericity"]])

cat("\nY factores de corrección para cuando no se cumple la condición de esfericidad:\n")
print(pruebaEz$"Sphericity Corrections")

# Gráfico del tamaño del efecto.
g2 <- ezPlot(data = datos2, dv = tiempo, wid = instancia, within = tareas, 
             y_lab = "Tiempos entre tareas", x = tareas)

print(g2)
 
#Conclusión
# El valor p obtenido es inferior al nivel de significación definido, por lo que se rechaza 
# la hipótesis nula en favor de la alternativa, indicando que existe al menos 1 grupo con tiempos distintos.


# Debido a que rechazamos la hipótesis nula, esto nos indica que existe al menos 1 grupo con diferencias,
# por lo tanto se procede a realizar el procedimiento Post-Hoc para comparar los resultados obtenidos con 
# la prueba de ANOVA.

# Procedimiento post-hoc de Bonferoni.
bonferroni <- pairwise.t.test(datos2[["tiempo"]],
                              datos2[["tareas"]],
                              p.adj = "bonferroni",
                              paired = TRUE)

print(bonferroni)

# Procedimiento post-hoc de Holm.
holm <- pairwise.t.test(datos2[["tiempo"]],
                        datos2[["tareas"]],
                        p.adj = "holm",
                        paired = TRUE)

print(holm)

# Por lo tanto, lo visto en ambas pruebas (se realizaron dos para verificar) existen diferencias 
# significativas en colors/interfer y words/interfer
 

 


