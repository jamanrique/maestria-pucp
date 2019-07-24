#   Pontificia Universidad Católica del Perú
#   Maestría en Estadística

#   Anexo 1
#   Código en R

#   El presente anexo contiene el código R utilizado para el análisis de regresión lineal utilizado en el informe de Modelos Lineales 1.

rm(list=ls())

#   Paso 1: Abrir la base de datos en R

##  1.a. Apertura de la libreria "foreign", para leer archivos en csv.
library("foreign")

##  1.b. Determinación del working directory en dónde se aloja el archivo
setwd("D:/Dropbox/Maestría en Estadística/2018 - 1/Modelos Lineales 1/Trabajos_/")

##  1.c. Ingreso del archivo en R
BD_fuel <- read.csv("Trabajo1_ML_fuel.csv",header=TRUE,sep="\t")

#   Paso 2: Renombre de columnas para un análisis más fácil

names(BD_fuel) [1] <- "A1" #Impuesto a la Gasolina a A1
names(BD_fuel) [2] <- "A2" #Ingreso per cápita promedio a A2
names(BD_fuel) [3] <- "A3" #Millas de autopista a A3
names(BD_fuel) [4] <- "A4" #Personas con licencia de conducir a A4
names(BD_fuel) [5] <- "B"  #Consumo de gasolina a B

#   Paso 3: Analíticas descriptivas y análisis preliminar
summary(BD_fuel)
plot(BD_fuel)
cor(BD_fuel)

#   Paso 4: Regresión stepwise

##  4.a. Apertura de la librería "leaps", para hacer regresión stepwise.
library("leaps")

## 4.b. Regresión stepwise
step_1 <- leaps(x=BD_fuel[,1:4], y=BD_fuel[,5],int=TRUE,method = c("r2","adjr2","Cp"),nbest=1,names=NULL)



