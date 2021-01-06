#Load college data into R
if (!exists("college")){
  library(collegeIncome)
  data(college)
}

#Rápidamente miramos las dimensiones y estructura de los datos
dim(college)
str(college)

#Creamos variables categoricas para la variable major_category
college$major_category <- as.factor(college$major_category)

#boxplot median income by major category
boxplot(median/1000 ~ major_category,data=college, main = "Income vs Major", ylab = "Income (in thousands of dollars)", xlab = "", las = 2)

#Ajustamos el modelo para empezar a analizar los supuestos que son: Linealidad, Homocedasticidad, Independencia y Normalidad (Notar que con este diagnóstico es imposible)
## confirmar Independenica, eso es un supuesto que haremos para enfocarnos en el modelo lineal y sus parámetros.
fit <- lm((median) ~ major_category, data = college)

#graficamos 4 diagnósticos
par(mfrow=c(2,2))
plot(fit, which=1:4)

## Según "Normal Q-Q", los residuales del modelo estan linealmente asociados en su mayoría, dándonos a entender que la distribución de los residuales es normal.
## El gráfico de Residuales vs Ajustados muestra patrones aunque estan relativamente distribuidos al rededor de y = 0. Parece haber cierta linealidad entre las variables,
## aunque no podemos estar muy seguro.
## La gráfica Scale-Location nos informa si los residuales estan igualmente distribuidos en los rangos de los predictores (si la varianza de los errores es homogenea). 
## Esta confirma la suposición de homocedasticidad si la línea 
## roja es aproximadamente horizontal, el cuál no es nuestro caso. Podemos escalar la respuesta con logaritmo natural para intentar mejorar estos valores.

#histograma de la variable 'median'
with(college, hist(median/1000))

## Podemos tomar la decantanción hacia la izquierda de los datos como una justificación para hacer una transformación logaritmica.
fitLog <- lm(log(median) ~ major_category, data = college)

#graficamos 4 diagnósticos
par(mfrow=c(2,2))
plot(fitLog, which=1:4)

## Supondremos que la mejora en Residuals vs Fitted y Scale-Location es sustancial.

## Por último, basado en la gráfica Cook's Distance, investigaremos el punto 63 que tiene potencial de ser alto en influencia en nuestro modelo.

plot(hatvalues(fitLog))

## vemos que el punto 63 no dista de la mayoría por lo tanto no tiene mucho leverage.

## Checkeando para influencia:

summary(influence.measures(fitLog))

## Vemos que el punto 63 es el que tiene más influencia en los coeficientes según la función dfbetas en el 4to parámetro. En el resto de los
## parámetros no parece tener más influencia que los otros outliers. Decidiremos no eliminar el punto ya que no parece presentar una amenaza a nuestro modelo.

summary(fitLog)$coef

## Mirando los p-values de cada categoria, vemos que son insignificantes (>=0.05) en el test de intervalo de confianza de 95% 
## (donde la hipotesis nula es que no existe relacion entre el predictor y la respuesta). En este caso fallamos en rechazar la hipótesis nula.
## Concluimos que no existe realmente asociación entre los college majors y las ganancias (Income).


