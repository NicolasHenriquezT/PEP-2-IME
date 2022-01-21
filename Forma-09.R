library(dplyr)
library(ggpubr)
library(ez)

# Pregunta 1

# Lord Vader desea saber si los niveles de exigencia con que los distintos oficiales evaluadores (instructor,
# capitán, comandante y general) califican a los sandtroopers son similares, por lo que le ha solicitado estudiar si existen
# diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales. El Lord Sith ha sido muy
# claro al solicitar un reporte de aquellos oficiales cuyas evaluaciones presenten diferencias.

# Se leen los datos del excel.
datos <- read.csv2(file.choose(),header=TRUE, sep = ";")

# Se filtra segun la division.
datos <- datos%>%filter(division == "Sandtrooper")

# Se filtra segun los distintos oficiales evaluadores.
evaluaciones <- datos%>%select(eval_instructor, eval_capitan, eval_comandante, eval_general)

# Se generan las instancias para agregarlas al dataframe de evaluaciones. 
instancia <- factor(1:100)

# Se agregan las instancias al dataframe de evaluaciones.
evaluaciones <- cbind(instancia, evaluaciones)

# Hipótesis:

# Hipótesis Nula (H0)
# H0: No existen diferencias significativas en el promedio de la evaluacion realizada por cada uno de los oficiales.

# Hipótesis alternativa (HA)
# HA: Existen diferencias significativas en el promedio de la evaluacion realizada por cada uno de los oficiales.

# Se transformar los datos del dataframe a numericos
evaluaciones <- as.data.frame(apply(evaluaciones, 2, as.numeric))

# Se verifica la correlacion de los datos para escoger la prueba de hipotesis a utilizar.
cor(evaluaciones)
# Se observa que todos los datos son muy bajos, por lo que no estan correlacionados, debido a esto
# se utilizara el ANOVA para muestras independientes.

# Condiciones para utilizar ANOVA para muestras independientes.
# 1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos iguales.
# Respuesta: Esta condicion se verifica, ya que todos los valores observados se encuentran en una escala de 
#            intervalos iguales.

# 2. Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) población(es) de origen.
# Respuesta: Se puede suponer segun el enunciado que se verifica esta condicion.

# 3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.

# Se tranforma el dataframe a formato largo.
evaluaciones <- evaluaciones %>% pivot_longer(c("eval_instructor", "eval_capitan", "eval_comandante", "eval_general"),
                                names_to = "evaluadores", values_to = "evaluacion")

# Comprobación de normalidad.
g <- ggqqplot(evaluaciones, x = "evaluacion", y = "evaluadores", color = "evaluadores")
g <- g + facet_wrap(~ evaluadores)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Respuesta: Se puede observar en el grafico Q-Q que no existen valores atipicos, por lo que se cumple esta condicion.
#            Debido a esto se utilizara un nivel de significancia estandar (α = 0.05). 

# 4. Las k muestras tienen varianzas aproximadamente iguales.
# Respuesta: Esta condicion se verificara mas adelante con la prueba ezANOVA(), ya que esta realiza la 
#            prueba de homocedasticidad de Levene

# Se define el nivel de significancia.
alfa <- 0.05

# Procedimiento ANOVA con ezANOVA().
prueba <- ezANOVA(data = evaluaciones, dv = evaluacion, between = evaluadores,  
                   wid = instancia, return_aov = TRUE)
print(prueba)

# La hipotesis nula de la prueba de Levene corresponde a que las varianzas de las k muestras son iguales.
# Como se obtuvo un p valor de 0.77 superior al nivel de significancia establecido, se acepta la h0, por lo 
# que podemos afirmar que se verifica la cuerta condicion para utilizar el procedemiento de ANOVA.

# Se realiza un grafico del tamaño del efecto.
g2 <- ezPlot(data = evaluaciones, dv = evaluacion, wid = instancia, between = evaluadores,
             y_lab = "Evaluacion promedio", x = evaluadores)
print(g2)

# Conclusion:
# El valor p obtenido p = 5.598888e-45 es inferior a nuestro nivel de significacion establecido α = 0.05, debido
# a esto rechazamos la hipotesis nula en favor de la hipotesis alternativa. Podemos concluir con un 95% de confianza
# que existen diferencias significativas en el promedio de la evaluacion realizada por cada uno de los oficiales. 
# Esto tambien se puede verificar en el grafico del tamaño del efecto realizado.
















