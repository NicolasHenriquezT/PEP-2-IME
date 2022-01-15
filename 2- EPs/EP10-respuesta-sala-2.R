library(dplyr)
library(tidyverse)





# Integrantes
# - Nicolás Henríquez Turner - 20.730.845-5
# - Iván Zúñiga Quiróz       - 20.003.345-0
# - Bastián Loyola Jara      - 20.552.001-5
# - David Morales Perez      - 19.881.480-6

# Enunciado:

# Pregunta 1:
# Dos artículos reportan el porcentaje de acierto alcanzado por dos algoritmos de clasificación,
# específicamente el Bayes ingenuo (C4) y el Bayes ingenuo oculto (C2), en diferentes conjuntos de prueba
# disponibles en el UCI Machine Learning Repository. ¿Es uno de los algoritmo mejor que el otro?
texto <-("
 Dataset C2 Dataset C4
 'anneal' 98.00 'cmc' 51.05
 'contact-lenses' 68.33 'credit' 86.23
 'ecoli' 80.04 'grub-damage' 47.79
 'kr-s-kp' 92.46 'monks' 62.24
 'monks1' 100.00 'mushroom' 95.83
 'nursery' 94.28 'page-blocks' 93.51
 'pasture-production' 85.83 'postoperatie' 66.67
 'primary-tumor' 48.08 'segment' 91.30
 'solar-flare-C' 88.24 'soybean' 92.08
 'squash-stored' 58.00 'squash-unstored' 61.67
 'tae' 44.38 'waveform' 79.86
 'white-clover' 79.29 -- –-
")
datos <- read.table(textConnection(texto), header = TRUE, na.strings = "--")

# Condiciones para realizar prueba de suma de rangos de Wilcoxon.
# 1- Las observaciones de ambas muestras son independientes de acuerdo a los diferentes conjuntos de prueba 
#    disponibles en el UCI Machine Learning Repository.
# 2- La segunda condición la cual indica que la escala de medición empleada debe ser a lo menos ordinal no se
#    cumple, ya que no podemos asegurar que en ambos casos existe la misma diferencia de porcentaje de acierto.

# Como no se cumple la segunda condición, utilizaremos como alternativa la prueba no paramétrica de Wilcoxon-Mann-Whitney.

# Hipótesis:
# Hipótesis Nula (H0)
# H0: No hay diferencia en el porcentaje de acierto alcanzado por ambos algoritmos (se distribuyen de igual forma).

# Hipótesis alternativa (HA)
# HA: Si hay diferencia en el porcentaje de acierto alcanzado por ambos algoritmos (distribuciones distintas).

# Datos para prueba de Mann-Whitney
datosC2 <- datos[["C2"]]
datosC4 <- datos[["C4"]]
datosC4 <- datosC4[1:11]
datosC4 <- as.numeric(datosC4) 

# Se establece el nivel de significación.
alfa <- 0.05

# Prueba de Mann-Whitney.
prueba <- wilcox.test(datosC2, datosC4, alternative = "two.sided", conf.level = 1 - alfa)
print(prueba)

# Conclusión:
# Como el valor p = 0.6947 obtenido es superior al nivel de significancia escogido α = 0.05, se falla al rechazar la
# hipótesis nula, podemos concluir con un 95% de confianza que no hay diferencia en el porcentaje de acierto alcanzado 
# por ambos algoritmos. En este caso no es necesario realizar un procedimiento post-hoc, ya que la prueba de Mann-Whitney 
# no encontró diferencias estadísticamente significativas. 







# Pregunta 2:
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de interés de
# los créditos en Chile, necesite utilizar una prueba de los rangos con signo de Wilcoxon, debido a
# problemas con la escala de la variable dependiente en estudio. Indiqué cuáles serían las variables/niveles
# involucrados en su ejemplo y las hipótesis nula y alternativa a contrastar. 

# Ejemplo:

# Debido a la alza de interés de los créditos en Chile, los bancos prestamistas de dinero
# tienen que actualizar sus tasas de interés.

# Un banco quiere actualizar su sistema de préstamo de dinero con cierto nivel de interés pero
# evitando perjudicar a sus clientes, de tal forma que quiere emplear esta alza 
# con el menor impacto posible.
# antiguamente su sistema de prestamos, prestaba a sus clientes un monto máximo de
# 3 millones de pesos con un interés anual del 2%.

# ahora para actualizarlo se tienen 2 ideas para evitar el mayor impacto posible en sus clientes:

# El primero, consta de simplemente actualizar el interés de 2% a 2,5% anual
# (talque: El préstamo máximo es de 3 millones de pesos, con un interés anual de 2,5%)

# y el segundo consta en actualizar el sistema, tanto de préstamo como de interés, el cual
# permite aumentar la cantidad de dinero máximo que se pueda prestar a un cliente, pero 
# con un interés un poco mas elevado.
# (por ejemplo: 5 millones de pesos como máximo con un interés anual del 3%)

# Los economistas y accionistas del banco, consideran que es mejor el segundo sistema, ya
# que hay situaciones en los cuales no alcanza con el primer sistema.

# Para saber cual de los 2 sistemas implementar, se toman 10 clientes al azar
# quienes tienen que evaluar los 2 sistemas en diversas situaciones, los
# cuales, pedir un préstamo los pueda sacar de algún apuro.
# Para medir los prestamos y situaciones, se medir? con la escala Likert de 7 puntos,
# donde 1 significa "muy malo" y 7 significa "muy bueno".


# Variables:

# Los 10 clientes del banco y Los dos sistemas de prestamos, los cuales son:

# muestra aleatoria de 10 clientes.
# El primer sistema de prestamos (con máximo de 3 millones, y un interés de 2,5% anual)
# El segundo sistema de prestamos (con un máximo de 5 millones, y un interés de 3% anual)


# Niveles:

# Los niveles serán medidos por la escala Likert de 7 puntos,
# donde 1 significa "muy malo" y 7 significa "muy bueno".
# y estos niveles serán otorgados por clientes escogidos de las 10 muestras aleatorias.


# Hipótesis:

# Hipótesis Nula (H0)
# H0: Las mismas personas no perciben diferencias al usar un sistema u otro.

# Hipótesis alternativa (HA)
# HA: Las mismas personas consideran que usar el segundo sistema es mejor que el primer sistema. 







# Pregunta 3:
# El siguiente texto muestra porcentaje de acierto alcanzado por tres algoritmos de clasificación en
# diferentes conjuntos de prueba disponibles en el UCI Machine Learning Repository. Los algoritmos
# corresponden a C3: averaged one-dependence estimator (AODE), C6: locally weighted naive-Bayes y C7:
# random forest. ¿Existe un algoritmo mejor o peor que los otros?
texto <- ("
 Dataset C3 C6 C7
 'credit' 85.07 85.22 83.33
 'eucalyptus' 58.71 59.52 59.40
 'glass' 73.83 75.69 73.33
 'hepatitis' 83.79 82.50 81.25
 'iris' 92.67 92.00 93.33
 'optdigits' 96.90 94.20 91.80
 'page-blocks' 96.95 94.15 96.97
 'pendigits' 97.82 94.81 95.67
 'pima-diabetes' 75.01 74.75 72.67
 'primary-tumor' 47.49 49.55 38.31
 'solar-flare-C' 88.54 87.92 86.05
 'solar-flare-m' 87.92 86.99 85.46
 'solar-flare-X' 97.84 94.41 95.99
 'sonar' 81.26 80.79 78.36
 'waveform' 84.92 83.62 79.68
 'yeast' 57.74 57.48 56.26
")
datos <- read.table(textConnection(texto), header = TRUE)

# Condiciones para realizar prueba de suma de rangos de Wilcoxon.
# 1- La primera condición se verifica, ya que las variables son categóricas y existen 3 algoritmos.
# 2- Los valores obtenidos son en porcentajes, esto verifica la segunda condición, ya que los porcentajes
#    siguen una escala ordinal, es decir, pueden ser mayores o menores que otros.
# 3- La tercera condición se verifica por el enunciado, ya que los datos provienen de diferentes conjuntos
#    de prueba disponibles en el UCI Machine Learning Repository

# Hipótesis:
# Hipótesis Nula (H0)
# H0: Todos los algoritmos tienen el mismo porcentaje de acierto.

# Hipótesis alternativa (HA)
# HA: Existe al menos un algoritmo que tiene un porcentaje de acierto distinto de los demás.

# Datos para prueba de Friedman.
datos <-datos %>% pivot_longer(cols = 2:4, 
                               names_to = "Algoritmo", 
                               values_to = "Acierto")

datos[, 'Algoritmo'] <- lapply(datos[, 'Algoritmo'], as.factor)
datos$Algoritmo

# Se establece el nivel de significación.
alfa <- 0.05

# Prueba de Friedman.
prueba <- friedman.test(Acierto ~ Algoritmo | Dataset, data = datos)
print(prueba)

# En caso que se encuentren diferencias significativas se realiza el procedimiento post-hoc.
if(prueba$p.value < alfa) {
        post_hoc <- pairwise.wilcox.test(datos$Acierto,
                                         datos$Algoritmo,
                                         p.adjust.methods = "holm",
                                         paired = TRUE)
         
print(post_hoc)
}

# Conclusión:
# Como el valor obtenido p = 0.00633 en la prueba de Friedman es inferior al nivel de significancia 
# escogido α = 0.05, se rechaza la hipótesis nula en favor de la hipótesis alternativa, podemos concluir
# con 95% de confianza que existen diferencias significativas entre  los porcentajes de acierto de los
# algoritmos C3, C6 y C7. 

# Se realiza el procedimiento post-hoc para analizar los resultados obtenidos.

# A partir de los resultados del procedimiento post-hoc, con un nivel de significación α = 0,05, podemos
# concluir con 95% confianza que existen diferencias significativas entre los porcentajes de acierto de los
# algoritmos C3 y C7, para el caso de los demás algoritmos no existen diferencias.







# Pregunta 4:
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de interés de
# los créditos en Chile, necesite utilizar una prueba de Kruskal-Wallis, debido a problemas con la normalidad
# de los datos. Indiqué cuáles serían las variables/niveles involucrados en su ejemplo y las hipótesis nula y
# alternativa a contrastar. 

# Ejemplo:

# Las tasa de intereses del sistema financiero son tasas promedio ponderadas y
# corresponden a operaciones efectivas de otorgamiento de crédito por parte de
# empresas bancarias, as? como la captación de dinero mediante deposito y emisión
# de bonos. En el ultimo tiempo, la tasa a presentado un notorio incremento por lo
# que es necesario su estudio (para ello  se ha investigado las tasas de distintas
# instituciones bancarias) y responder la siguiente pregunta: 
# ¿Esta relacionado el incremento de las tasas de interés con los tres tipos de plazo 
# (Consumo, comerciales e hipotecarios)? 


# Variables:

# Todos los datos de las muestras combinadas.
# Las cuales serán extraídas de las distintas instituciones bancarias.
# La suma de cada rango (Tx).
# La media de cada rango (Mx).
# El tamaño de la muestra (nx).


# Niveles:

# Asignar un rango de cada elemento de las combinaciones.
# Anova para muestras correlacionadas.


# Hipótesis:

# Hipótesis Nula (H0)
# H0: No hay diferencia entre los tres tipos de plazos. 

# Hipótesis alternativa (HA)
# HA: Hay diferencia entre los tres tipos de plazos.

