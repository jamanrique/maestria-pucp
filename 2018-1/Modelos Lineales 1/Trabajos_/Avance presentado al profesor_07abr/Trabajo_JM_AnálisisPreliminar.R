rm(list=ls())
library("foreign")
setwd("D:/Dropbox/Maestría en Estadística/2018 - 1/Modelos Lineales 1/Trabajos_/")
x <- read.csv("Trabajo1_ML_fuel.csv",header=TRUE,sep="\t")

#renombre de columnas
names(x) [1] <- "A1" #Impuesto a la Gasolina a A1
names(x) [2] <- "A2" #Ingreso per cápita promedio a A2
names(x) [3] <- "A3" #Millas de autopista a A3
names(x) [4] <- "A4" #Personas con licencia de conducir a A4
names(x) [5] <- "B"  #Consumo de gasolina a B

#matriz de correlaciones
cor(x)

#scatterplot
plot(x)