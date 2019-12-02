#### Trabajo 1: Técnicas de Análisis Multivariado
#### 20091107 - Justo Manrique Urbina
#### Prof. Rocío Maehara

#### Análisis de componentes principales ####

## Carga de datos y librerías ##
library(FactoMineR)
library(dplyr)

data("mtcars")

## Preprocesamiento de datos ##

mtcars$vs <- factor(mtcars$vs)
mtcars$vs <- recode_factor(mtcars$am, `0`="V-shaped",`1`="Straight")
mtcars$am <- factor(mtcars$am)
mtcars$am <- recode_factor(mtcars$am, `0`="Automatic",`1`="Manual")

## Análisis de PCA ##

mt_pca <- PCA(mtcars,quali.sup = c(8,9),graph = TRUE,scale.unit = TRUE)

## Identificación de componentes ##

barplot(mt_pca$eig[,1], main="Valores propios", names.arg=paste("dim",1:nrow(mt_pca$eig)))

## Identificación de individuos ##
plot.PCA(mt_pca, choix="ind", invisible="ind.sup")
round(mt_pca$ind$contrib[,1:2],2)

## Identificación de las dimensiones
dimdesc(mt_pca)
