library("readxl")
library(dplyr)
library(tidyverse)
library(ez)
library(boot)
library("simpleboot")
library(ggpubr)

# Integrantes
# - Nicolás Henríquez Turner - 20.730.845-5
# - Iván Zúñiga Quiróz       - 20.003.345-0
# - Bastián Loyola Jara      - 20.552.001-5
# - David Morales Perez      - 19.881.480-6

# Enunciado
# Pregunta 1:
# Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos
# grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación
# Monte Carlo.

#Se define la semilla 4321.
set.seed(4321)

# Se lee el archivo de entrada.   *** EN ESTE CASO PARA LA LECTURA DEL ARCHIVO SE TRANSFORMO A FORMATO .CSV ***
datos <- read.csv(file.choose(), encoding = "UTF-8")

# Tamaño de la muestra.
n <- 400

# Ejemplo
# Un estudio pretende determinar si el ingreso total del hogar corregido (ytotcorh) es mayor en personas del sexo hombres.
# ¿Se puede considerar que existe una relación entre el ingreso total y el sexo de las personas? 

# Hipótesis:

# Hipótesis Nula (H0)
# H0: Existe una relación entre el ingreso total y el sexo de las personas.

# Hipótesis alternativa (HA)
# HA: No existe una relación entre el ingreso total y el sexo de las personas.

# Se filtran las variables a utilizar.
datos <- datos %>% select(sexo, ytotcorh)
datos <- datos %>% rename(ingresoTotal = ytotcorh)

# Se establece una muestra de 400 hogares.
datos <- sample_n(datos, n)


# Gráfico de cajas.
ggplot(data = datos, aes(x = sexo, y = ingresoTotal, colour = sexo)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.25) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

# Gráfico de densidad.
ggplot(data = datos, aes(x = ingresoTotal, fill = sexo)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  theme_bw() +
  theme(legend.position = "bottom")

# No se observa una diferencia evidente entre el ingreso total y el sexo.

# Se analizan los datos.
datos %>% group_by(sexo) %>% summarise(media = mean(ingresoTotal),
                                        sd = sd(ingresoTotal),
                                        mediana = median(ingresoTotal))

# Las medias de localización (media, dispersión y mediana) son aproximadamente similares
# en ambos grupos.

# Se realiza el test de permutaciones.

# Se inicia calculando la diferencia entre las medias de ambos grupos (diferencia observada).
media_hombres <- datos %>% filter(sexo == "Hombre") %>%
                           pull(ingresoTotal) %>%
                           mean()

media_mujeres <- datos %>% filter(sexo == "Mujer") %>%
                           pull(ingresoTotal) %>%
                           mean()

dif_obs <- media_hombres - media_mujeres
print(dif_obs)

# Para obtener la probabilidad exacta se necesita generar todas las posibles permutaciones 
# en las que los 400 hogares se puedan repartir en dos grupos y calcular la diferencia de 
# medias para cada grupo. Como el número de permutaciones es muy elevado, se recorre a la 
# simulación de Monte Carlo.

# Se obtiene una muestra aleatoria para realizar la simulación de Monte Carlo.
distribucion_permut <- rep(NA, 9999)
n_hombres <- datos %>% filter(sexo == "Hombre") %>% nrow()
n_mujeres <- datos %>% filter(sexo == "Mujer") %>% nrow()

for (i in 1:9999) {
  # mezclado aleatorio de las observaciones
  datos_aleatorizados <- sample(datos$ingresoTotal) 
  mean_hombres <- mean(datos_aleatorizados[1:n_hombres])
  mean_mujeres <- mean(datos_aleatorizados[n_hombres + 1:n_mujeres])
  distribucion_permut[i] <- mean_hombres - mean_mujeres
}

head(distribucion_permut)

# Estos datos obtenidos representan la variación esperada en la diferencia de medias debido
# únicamente a la asignación aleatoria de grupos.

# Gráfico para analizar la distribución de permutaciones vs la diferencia de medias.
ggplot(data = data.frame(permutaciones = distribucion_permut),
       aes(permutaciones)) +
  geom_histogram(aes(y = ..density..), alpha = 0.4, colour = "white") + 
  geom_line(aes(y = ..density..), stat = 'density', colour = "red") +
  geom_vline(xintercept = mean(distribucion_permut)) +
  geom_vline(xintercept = dif_obs, colour = "blue") +
  geom_vline(xintercept = -dif_obs, colour = "blue") +
  labs(title = "Distribución de permutaciones", x = "diferencia de medias") +
  theme_bw()

summary(distribucion_permut)
sd(distribucion_permut)

# Cálculo del p valor sin corrección.
p_value = (sum(abs(distribucion_permut) > abs(dif_obs)))/n
print(p_value)

# Cálculo del p valor con corrección.
p_value_corregido = ((sum(abs(distribucion_permut) > abs(dif_obs))) + 1)/(n + 1)
print(p_value_corregido)

# Conclusión:
# Se realizó una muestra aleatoria con 400 hogares y se utilizó el test de permutación para determinar
# si existía una diferencia significativa entre el ingreso total de hogares y el sexo de las personas. Sé
# calculo el p valor mediante la simulación de Monte Carlo usando la corrección de continuidad de Davison
# and Hikley(1997).

# El p valor obtenido muestra una evidencia muy débil en contra de la hipótesis nula de que no
# existen diferencias, por lo que se rechaza la hipótesis nula en favor de la hipótesis alternativa
# concluyendo que no existe una relación entre el ingreso total y el sexo de las personas.

# A modo de comprobación de los resultados, se realiza el test paramétrico t-test.
t.test(ingresoTotal ~ sexo, data = datos, var.equal = TRUE)

# Se observa que los p valor son simulares:
# Bootstrapping = 0.009975062, t-test = 0.0005372





 

# Pregunta 2:
# Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de
# dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta
# utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping
# aunque este no sea necesario.

# Pregunta de investigación
# ¿Tiene relación las horas trabajadas semanalmente por rango etario en mujeres?.

# Hipótesis:

# Hipótesis Nula (H0)
# H0: No existen diferencias entre los grupos.

# Hipótesis alternativa (HA)
# HA: Existen diferencias entre los grupos.

# Se define la semilla 1234.
set.seed(1234)

# Se leen los datos originales.
datosOriginales <- read_excel("/Datos-Casen-v2.xls")

# Se determina una muestra aleatoria.
datosOriginales <- datosOriginales %>% sample_n(500, replace = FALSE)

# Filtramos por columnas, sexo, edad y horas trabajadas semanalmente.
datos <- subset(datosOriginales, select=c("sexo", "edad","o10"))

# Se filtra por columna, solo individuos de sexo mujer.
datos <- datos %>% filter(sexo == "Mujer")

# En caso de existir un NA en horas trabajadas, se considera 0 horas.
datos[datos == 'NA'] <- '0'

# Generamos 3 muestras por rango de edad en específico (20 a 34, 35 a 49 y 49 a 65).
entre20y34 <- datos %>% filter(edad >= 20, edad < 35)
entre20y34 <- entre20y34$o10
entre20y34 <- as.integer(entre20y34)

entre35y49 <- datos %>% filter(edad >= 35, edad < 50)
entre35y49 <- entre35y49$o10
entre35y49 <- as.integer(entre35y49)

entre50y64 <- datos %>% filter(edad >= 50, edad < 65)
entre50y64 <- entre50y64$o10
entre50y64 <- as.integer(entre50y64)

# Definimos alfa como 0.01.
alfa <- 0.01

# Se crea la función para calcular las medias de una lista.
media <- function(valores,i){
  mean(valores[i])
}

# Se definen la cantidad de iteraciones.
B <- 1000

# Se realiza Bootstrapping para cada una de las muestras y posteriormente se muestra por pantalla.
distribucion_b20y34 <- boot(entre20y34 , statistic = media , R = B )
print(distribucion_b20y34)

print(plot(distribucion_b20y34))

distribucion_b35y49 <- boot(entre35y49 , statistic = media , R = B )
print(distribucion_b35y49)

print(plot(distribucion_b35y49))

distribucion_b50y64 <- boot(entre50y64 , statistic = media , R = B )
print(distribucion_b50y64)

print(plot(distribucion_b50y64))

# Asignamos los valores a unas variables.
t20y34 <- distribucion_b20y34$t
t35y49 <- distribucion_b35y49$t
t50y64 <- distribucion_b50y64$t

# Generamos la instancia para todos los valores.
instancia <- factor(1:B)

# Se forma un dataframe.
datosFinales <- data.frame(instancia,t20y34,t35y49,t50y64)


colnames(datosFinales) <- c("instancia","Entre 20 y 34 años", "Entre 35 y 49 años",
                            "Entre 50 y 64 años")

# Se hace un pivote con los datos.
datos_finales <- datosFinales %>% pivot_longer (c("Entre 20 y 34 años", "Entre 35 y 49 años",
                                                  "Entre 50 y 64 años") ,
                                                names_to = "Edades",
                                                values_to = "Horas")

datos_finales[["Edades"]] <- factor(datos_finales[["Edades"]])

# Tal como se había mencionado se realizará una prueba ANOVA mediante la función ezANOVA
# que incluye medidas en contra de las posibles violaciones dadas por la suposición en la esfericidad.
anova <- ezANOVA(datos_finales, dv = Horas , within = Edades ,
                 wid = instancia , return_aov = TRUE )

cat ("El resultado de la prueba de esfericidad de Mauchly :\n")
print(anova[["Mauchly's Test for Sphericity"]])
# Como el valor p es menor a 0.01 podemos determinar que no cumple con la condición de esfericidad.

aov <- anova$aov
print(aov)

# Conclusión:
# El valor p obtenido es inferior al nivel de significación definido, por lo que se rechaza 
# la hipótesis nula en favor de la alternativa, indicando que existe al menos 1 grupo con tiempos distintos.

# Debido a que rechazamos la hipótesis nulas, esto nos indica que existe al menos 1 grupo con diferencias
# por lo tanto, se procede a realizar el procedimiento Post-Hoc para comparar los resultados obtenidos con 
# la prueba de ANOVA.
holm <- pairwise.t.test (datos_finales[["Horas"]] , datos_finales[["Edades"]] ,
                         p.adj = "holm", paired = FALSE)

print(holm)
# Por lo visto en la prueba todos los grupos poseen diferencias significativas.


 

