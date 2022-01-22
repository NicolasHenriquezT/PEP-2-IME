library(dplyr)
library(ggpubr)
library(ez)
library(tidyverse)
library(pROC)
library(caret)



# Pregunta 1

# Lord Vader desea saber si los niveles de exigencia con que los distintos oficiales evaluadores (instructor,
# capitán, comandante y general) califican a los sandtroopers son similares, por lo que le ha solicitado estudiar si existen
# diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales. El Lord Sith ha sido muy
# claro al solicitar un reporte de aquellos oficiales cuyas evaluaciones presenten diferencias.

# Se leen los datos del excel.
datos <- read.csv2(file.choose(),header=TRUE, sep = ";")

# Se filtra según la división.
datos <- datos%>%filter(division == "Sandtrooper")

# Se filtra según los distintos oficiales evaluadores.
evaluaciones <- datos%>%select(eval_instructor, eval_capitan, eval_comandante, eval_general)

# Se generan las instancias para agregarlas al dataframe de evaluaciones. 
instancia <- factor(1:100)

# Se agregan las instancias al dataframe de evaluaciones.
evaluaciones <- cbind(instancia, evaluaciones)

# Hipótesis:

# Hipótesis Nula (H0)
# H0: No existen diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales.

# Hipótesis alternativa (HA)
# HA: Existen diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales.

# Se transforman los datos del dataframe a numéricos.
evaluaciones <- as.data.frame(apply(evaluaciones, 2, as.numeric))

# Se verifica la correlación de los datos para escoger la prueba de hipótesis a utilizar.
cor(evaluaciones)
# Se observa que todos los datos son muy bajos, por lo que no están correlacionados, debido a esto
# se utilizará el ANOVA para muestras independientes.

# Condiciones para utilizar ANOVA para muestras independientes.
# 1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos iguales.
# Respuesta: Esta condición se verifica, ya que todos los valores observados se encuentran en una escala de 
#            intervalos iguales.

# 2. Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) población(es) de origen.
# Respuesta: Se puede suponer según el enunciado que se verifica esta condición.

# 3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.

# Se transforma el dataframe a formato largo.
evaluaciones <- evaluaciones %>% pivot_longer(c("eval_instructor", "eval_capitan", "eval_comandante", "eval_general"),
                                names_to = "evaluadores", values_to = "evaluacion")

# Comprobación de normalidad.
g <- ggqqplot(evaluaciones, x = "evaluacion", y = "evaluadores", color = "evaluadores")
g <- g + facet_wrap(~ evaluadores)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Respuesta: Se puede observar en el gráfico Q-Q que no existen valores atípicos, por lo que se cumple esta condición.
#            Debido a esto se utilizará un nivel de significancia estándar (α = 0.05). 

# 4. Las k muestras tienen varianzas aproximadamente iguales.
# Respuesta: Esta condición se verificará más adelante con la prueba ezANOVA(), ya que esta realiza la 
#            prueba de homocedasticidad de Levene.

# Se define el nivel de significancia.
alfa <- 0.05

# Procedimiento ANOVA con ezANOVA().
prueba <- ezANOVA(data = evaluaciones, dv = evaluacion, between = evaluadores,  
                   wid = instancia, return_aov = TRUE)
print(prueba)

# La hipótesis nula de la prueba de Levene corresponde a que las varianzas de las k muestras son iguales.
# Como se obtuvo un p valor de 0.77 superior al nivel de significancia establecido, se acepta la h0, por lo 
# que podemos afirmar que se verifica la cuarta condición para utilizar el procedimiento de ANOVA.

# Se realiza un gráfico del tamaño del efecto.
g2 <- ezPlot(data = evaluaciones, dv = evaluacion, wid = instancia, between = evaluadores,
             y_lab = "Evaluacion promedio", x = evaluadores)
print(g2)

# Conclusión:
# El valor p obtenido p = 5.598888e-45 es inferior a nuestro nivel de significación establecido α = 0.05, debido
# a esto rechazamos la hipótesis nula en favor de la hipótesis alternativa. Podemos concluir con un 95% de confianza
# que existen diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales. 
# Esto también se puede verificar en el gráfico del tamaño del efecto realizado.

# Se procede a realizar el procedimiento Post-Hoc para comparar los resultados obtenidos con la prueba de ANOVA. Esto
# debido a que obtuvimos diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales,
# pero necesitamos identificar a aquellos oficiales que presentan diferencias en sus evaluaciones.

# Procedimiento post-hoc de Holm.
holm <- pairwise.t.test(evaluaciones[["evaluacion"]], 
                        evaluaciones[["evaluadores"]],
                        p.adj = "holm", 
                        pool.sd = TRUE, 
                        paired = FALSE, 
                        conf.level = 1 - alfa)

cat("\n\nCorrecion de Holm\n")
print(holm)

# Procedimiento ANOVA.
anova <- aov(evaluacion ~ evaluadores, data = evaluaciones)


# Procedimiento post-hoc HSD de Tukey.
post_hoc <- TukeyHSD(anova,
                     "evaluadores",
                     ordered = TRUE,
                     conf.level = 1 - alfa)
print(post_hoc)

# A través de los procedimientos post-hoc realizados se puede observar que los valores p inferiores a nuestro
# alfa (α = 0.05) corresponden a eval_comandante-eval_capitán, eval_instructor-eval_capitán y eval_general-eval_capitán.
# Los cuales también presentan una diferencia significativa, esto coincide con el análisis post-hoc de holm y con el gráfico
# del tamaño del efecto realizado. Finalmente, para el reporte solicitado por Lord Sith podemos concluir con 95% de confianza 
# que las evaluaciones del capitán respecto al comandante, general e instructor presentan diferencias significativas en el promedio 
# de la evaluación realizada.

################################################################################################################################
################################################################################################################################
################################################################################################################################

# Pregunta 2:

# Se leen los datos del excel.
datos <- read.csv2(file.choose(),header=TRUE, sep = ";")

# Se establece la semilla.
set.seed(3488)

# Tamaño de la muestra.
n <- 400

# Se obtiene la muestra de 400 datos.
set.seed(3488)
datos <- sample_n(datos, size = n)

# Se cambia es_Clon para que sea una variable categórica numérica.
datos$es_clon <- ifelse(datos$es_clon == "S", 1, 0)

# Se obtienen los nombres de las variables del dataframe.
nombresVariables <- colnames(datos[3:16])

# Se seleccionan al azar 3 variables predictoras.
set.seed(3488)
variablesPredictoras <- sample(nombresVariables, size = 3)

# Se obtiene el 80% de los datos para entrenamiento.
entrenamiento <- floor(0.8 * n)
muestra <- sample.int(n = n , size = entrenamiento, replace = FALSE)
entrenamiento <- datos[muestra, ]

# Se obtiene el 20% de los datos para prueba. Esto corresponde a lo que resta de los datos de entrenamiento.
prueba <- datos[-muestra, ]

# Se ajusta el modelo con la primer variable predictora "agilidad".
modelo1 <- glm(es_clon ~ agilidad, family = binomial(link = "logit"), data = entrenamiento)
print(summary(modelo1))

# Se ajusta el modelo con la segunda variable predictora "peso".
modelo2 <- glm(es_clon ~ agilidad + peso, family = binomial(link = "logit"), data = entrenamiento)
print(summary(modelo2))

# Se calcula el AIC1 (modelo1) y AIC2 (modelo2) para hacer la comparación con ANOVA.
AIC1 <- AIC(modelo1)
AIC2 <- AIC(modelo2)

# Se realiza la comparación de los modelos con ANOVA.
comparacion <- anova (modelo1, modelo2, test = "LRT")
print(comparacion)

# Se utilizará un nivel de significancia de 0.05 para analizar los modelos.
# Se observa que el AIC del modelo 1 es menor que el del modelo 2. Al realizar la comparación con ANOVa para
# el modelo 2 obtenemos un p = 0.3213, el cual es mayor a nuestro nivel de significancia, por lo que podemos
# concluir con 95% de confianza que el modelo1 es mejor que el modelo2.

# Utilizando el mejor modelo posible se debería determinar si es necesario un entrenamiento especial para clones o
# reclutas utilizando las variables predictoras.


 

################################################################################################################################
################################################################################################################################
################################################################################################################################

# Pregunta 3:

# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en 
# donde un estudio o experimento, relacionado con las expectativas de los chilenos para el nuevo 
# gobierno, necesite utilizar una prueba de Kruskal-Wallis debido a problemas con la escala de la 
# variable dependiente en estudio. Indiqué cuáles serían las variables involucradas en su 
# ejemplo (con sus respectivos niveles) y las hipótesis nula y alternativa a contrastar.


# Respuesta:
# Para este problema se indica que debe existir un problema con la variable dependiente del estudio
# Puntualmente que la escala con que se mide la variable dependiente, es en intervalos desiguales.
# Aunque sean en intervalos desiguales esta debe ser a lo menos ordinal, de esta forma se plantea
# Que los chilenos evalúen a base de sus expectativas el próximo gobierno utilizando distintas escalas
# para distintos rangos etarios, siendo por ejemplo
# De 10 a 30 años se utilice una escala de 1 a 7
# De 31 a 50 años se utilice una escala de 1 a 5
# De 51 a 70 años se utilice una escala de 1 a 20
# finalmente de 70 años para arriba utilizarán una escala de 1 a 10
# Como supuesto se tendrá que entre mayor el valor es una mejor expectativa sobre el nuevo gobierno
# Para este estudie se buscará si existe un rango etario que difiere en puntuación promedio

# Hipótesis nula:
# Todos los rangos etarios asignaron una puntuación promedio similar.

# Hipótesis alternativa:
# Al menos un rango etario asigno una puntuación promedio distinta.


