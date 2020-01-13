###############
## Ejemplo 1 ##
###############

# Cargando el conjunto de datos de entrenamiento y de prueba

craneo <- read.table("craneo.txt",header=T)


# Análisis descriptiva

library(ggplot2)
library(ggpubr)
p1 <- ggplot(data = craneo, aes(x = Longitud, fill = Tipo)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = craneo, aes(x = Anchura, fill = Tipo)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = craneo, aes(x = Altura, fill = Tipo)) +
  geom_histogram(position = "identity", alpha = 0.5)
p4 <- ggplot(data = craneo, aes(x = Altura_Cara, fill = Tipo)) +
  geom_histogram(position = "identity", alpha = 0.5)
p5 <- ggplot(data = craneo, aes(x = Anchura_Cara, fill = Tipo)) +
  geom_histogram(position = "identity", alpha = 0.5)
ggarrange(p1, p2, p3, p4, p5, nrow = 5, common.legend = TRUE)

# Gráficos de dispersión

pairs(x = craneo[, c("Longitud", "Anchura", "Altura", "Altura_Cara", "Anchura_Cara")],
      col = c("firebrick", "green3")[craneo$Tipo], pch = 19)

table(craneo$Tipo)/nrow(craneo)

# Representación mediante Histograma de cada variable para cada tipo raza
par(mfcol = c(2, 5))
for (k in 1:5) {
  j0 <- names(craneo)[k]
  x0 <- seq(min(craneo[, k]), max(craneo[, k]), le = 1000)
  for (i in 1:2) {
    i0 <- levels(craneo$Tipo)[i]
    x <- craneo[craneo$Tipo == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("Tipo", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}
par(mfcol = c(1, 1))

# Representación de cuantiles normales de cada variable para cada tipo de raza

par(mfcol = c(2, 5))
for (k in 1:5) {
  j0 <- names(craneo)[k]
  x0 <- seq(min(craneo[, k]), max(craneo[, k]), le = 1000)
  for (i in 1:2) {
    i0 <- levels(craneo$Tipo)[i]
    x <- craneo[craneo$Tipo == i0, j0]
    qqnorm(x, main = paste("Tipo", i0, j0), pch = 19, col = i + 1)
    qqline(x)
  }
}
par(mfcol = c(1, 1))

# Contraste de normalidad Shapiro-Wilk para cada variable en cada tipo de raza
library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(craneo, value.name = "valor")
kable(datos_tidy %>% group_by(Tipo, variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))

# Misma operación con aggregate
aggregate(formula = valor ~ Tipo + variable, data = datos_tidy,
          FUN = function(x){shapiro.test(x)$p.value})

# identificando posibles outliers que podrían afectar la normalidad multivariante
library(MVN)
outliers <- mvn(data = craneo[,-6], mvnTest = "hz", multivariateOutlierMethod = "quan")

# Contraste de normalidad multivariante

royston_test <- mvn(data = craneo[,-6], mvnTest = "royston", multivariatePlot = "qq")

royston_test$multivariateNormality

hz_test <- mvn(data = craneo[,-6], mvnTest = "hz")
hz_test$multivariateNormality

# Contraste para la homogeneidad de varianzas

library(biotools)
boxM(data = craneo[, 1:5], grouping = craneo[, 6])

# Análisis de discriminante lineal

modelo_lda <- lda(formula = Tipo ~ Longitud + Anchura + Altura + Altura_Cara + Anchura_Cara,
                  data = craneo)

# Clasificación de nuevos craneos
# Se consideran las medidas de dos nuevos craneos
nuevosdatos <-
  rbind(c(171,140.5,127.0,69.5,137.0),c(179.0,132.0,140.0,72.0,138.5))
# Asigno a los dos nuevos datos los nombres de las variables
colnames(nuevosdatos) <- colnames(craneo[,-6])
nuevosdatos <- data.frame(nuevosdatos)
nuevosdatos
# Se predice el grupo de pertenencia de los nuevos datos
predict(modelo_lda,newdata=nuevosdatos)$class
predict(modelo_lda,newdata=nuevosdatos)

# evaluación de los errores de clasificación

predicciones <- predict(object = modelo_lda, newdata = craneo[, -6],
                        method = "predictive")
table(craneo$Tipo, predicciones$class,
      dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(craneo$Tipo != predicciones$class) * 100
paste("trainig_error=", trainig_error, "%")

# visualización de las clasificaciones
library(klaR)
partimat(Tipo ~ Longitud + Anchura + Altura + Altura_Cara + Anchura_Cara,data=craneo, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1", "skyblue2"),
         col.mean = "firebrick")

# https://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
# https://rpubs.com/Joaquin_AR/233932

###############
## Ejemplo 2 ##
###############

#diabetes=read.csv("DiabetesTrain.csv")

diabetes=read.csv(file.choose())
head(diabetes)

# Analisis descriptivo

library(ggplot2)
library(ggpubr)

plot1 <- ggplot(data = diabetes, aes(x = glucose)) +
  geom_density(aes(colour = class)) + theme_bw()
plot2 <- ggplot(data = diabetes, aes(x = insulin)) +
  geom_density(aes(colour = class)) + theme_bw()
plot3 <- ggplot(data = diabetes, aes(x = sspg)) +
  geom_density(aes(colour = class)) + theme_bw()
# la función grid.arrange del paquete grid.extra permite ordenar
# graficos de ggplot2
ggarrange(plot1, plot2, plot3, common.legend = TRUE, legend = "bottom")

# gráficos de dispersión
pairs(x = diabetes[, -4], col = c("firebrick", "green3", "blue")[diabetes$class],
      pch = 20)

# prior probabilities
table(diabetes[,4])/nrow(diabetes)

#representación mediante histograma de cada variable para cada clase

par(mar = rep(3, 3))
for (k in 1:3) {
  j0 <- names(diabetes)[k]
  x0 <- seq(min(diabetes[, k]), max(diabetes[, k]), le = 1000)
  for (i in 1:3) {
    i0 <- levels(diabetes$class)[i]
    x <- diabetes[diabetes$class == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("class", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}
par(mar = rep(1, 1))

#representación de cuantiles normales de cada variable para cada clase

par(mar = rep(3, 3))
for (k in 1:3) {
  j0 <- names(diabetes)[k]
  x0 <- seq(min(diabetes[, k]), max(diabetes[, k]), le = 1000)
  for (i in 1:3) {
    i0 <- levels(diabetes$class)[i]
    x <- diabetes[diabetes$class == i0, j0]
    qqnorm(x, main = paste(i0, j0), pch = 19, col = i + 1) 
    qqline(x)
  }
}
par(mar = rep(1, 1))

#Contraste de normalidad Shapiro-Wilk para cada variable en cada clase
library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(diabetes, value.name = "valor")
kable(datos_tidy %>% group_by(class, variable) %>% summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5)))

# detección de posibles outliers que podrían afectar la normalidad multivariante
library(MVN)
outliers <- mvn(data = diabetes[,-4], mvnTest = "hz", multivariateOutlierMethod = "quan")

# prueba de hipótesis para la normalidad multivariante
royston_test <- mvn(data = diabetes[,-4], mvnTest = "royston", multivariatePlot = "qq")

royston_test$multivariateNormality

hz_test <- mvn(data = diabetes[,-4], mvnTest = "hz")
hz_test$multivariateNormality

# Contraste para la homogeneidad de varianzas
library(biotools)
boxM(data = diabetes[,-4], grouping = diabetes[,4])

# Análisis de discriminante lineal

library(MASS)
modelo_lda <- lda(class~.,data=diabetes)
modelo_lda

# Predicción y evaluación de los errores de clasificación

predicciones <- predict(object = modelo_lda, newdata = diabetes[-4])
table(diabetes$class, predicciones$class, dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(diabetes$class != predicciones$class) * 100
paste("trainig_error =", trainig_error, "%")

# Gráficos con la clasificación
library(klaR)
partimat(class~.,data=diabetes, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1", "snow2", "skyblue2"),
         col.mean = "firebrick",nplots.hor=3)

# Ejemplo 3

# cargando el conjunto de datos
credit <- read.csv("germancredit.csv")
head(credit,2)

cred1=credit[, c("Default","duration","amount","installment","age")]
head(cred1)

# normalidad multivariante

library(MVN)
outliers <- mvn(data = cred1[,-1], mvnTest = "hz", multivariateOutlierMethod = "quan")

royston_test <- mvn(data = cred1[,-1], mvnTest = "royston", multivariatePlot = "qq")

# contraste para la normalidad multivariante

royston_test$multivariateNormality
hz_test <- mvn(data = cred1[,-1], mvnTest = "hz")
hz_test$multivariateNormality

# Contraste para la homogeneidad de varianzas
library(biotools)
boxM(data = cred1[,-1], grouping = cred1[,1])

# Análisis de discriminante cuadrático

library(MASS)
modelo_qda <- qda(formula = Default~., data = cred1)
modelo_qda


# Clasificación de nuevas observaciones

zqua=qda(Default~.,cred1)
predict(zqua,newdata=data.frame(duration=6,amount=1100,installment=4,age=67))

# Evaluación de los errores de clasificación

predicciones <- predict(object = modelo_qda, newdata = cred1)
table(cred1$Default, predicciones$class,
      dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(cred1$Default != predicciones$class) * 100
paste("trainig_error =", trainig_error, "%")

