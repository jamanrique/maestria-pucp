library(dplyr)
library(car)
library(ggplot2)
library(car)
library(GGally)
library(stargazer)
library(hnp)

setwd("~/Documents/maestria-pucp/2019-2/modelos-lineales-2/clase-7/")

datos_preg4 <- readxl::read_excel("pregunta4_diana_v2.xlsx")
dim(datos_preg4)
head(datos_preg4)
glimpse(datos_preg4)

summary(datos_preg4)

nombres = c("ACTIVIDAd","Asegurados_total","Planilla_total","nsiniestros","nivel_riesgo")
datos_preg4 = subset ( datos_preg4 , select = nombres )
datos_preg4 = na.omit ( datos_preg4 )

datos_preg4 = datos_preg4 %>%
  mutate (  ACTIVIDAd=as.factor(ACTIVIDAd),
            nivel_riesgo=as.factor(nivel_riesgo))

datos_preg4 = datos_preg4[,2:5]
ggpairs(datos_preg4)

scatterplotMatrix(~nsiniestros+Planilla_total+Asegurados_total,smooth = FALSE)

#################
# Modelo 1: Poisson Regression y ~ x
modelo1 <- glm(nsiniestros ~ log(Asegurados_total) + log(Planilla_total) + nivel_riesgo, data=datos_preg4, family=poisson(link = "log"))

summary(modelo1)

exp(coef(modelo1))

##############
###Grafico de leverage y distancia de Cook

influenceIndexPlot(modelo1,vars=c ("Cook", "hat"), id=list(n=3))

###Gr?fico de residuos versus valores ajustados
residualPlot(modelo1,type="rstandard")

###Gr?fico de residuos con bandas de confianza
hnp(modelo1,halfnormal = FALSE)

##quitando los puntos influyentes
#################
# Modelo 2: Poisson Regression y ~ x -c(1296,47)
modelo2 <- glm(nsiniestros ~ log(Asegurados_total) + log(Planilla_total) + nivel_riesgo, data=datos_preg4, family=poisson(link = "log"), subset = -c(1296,47))

summary(modelo2)
#ha cambiado la estimacion de los parametros beta y el log(asegurados_total se ha vuelto significativo)

exp(coef(modelo2))

##############
###Grafico de leverage y distancia de Cook

influenceIndexPlot(modelo2,vars=c ("Cook", "hat"), id=list(n=3))

###Gr?fico de residuos versus valores ajustados
residualPlot(modelo2,type="rstandard")

###Gr?fico de residuos con bandas de confianza
hnp(modelo2,halfnormal = FALSE)
#no sale tan mal el grafico de residuos con bandas de confianza, los puntos no se escapan mucho de la banda
