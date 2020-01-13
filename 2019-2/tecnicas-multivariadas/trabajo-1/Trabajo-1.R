#### Trabajo 1: Técnicas de Análisis Multivariado
#### 20091107 - Justo Manrique Urbina
#### Prof. Rocío Maehara

#### Análisis de componentes principales ####

## Carga de datos y librerías ##
library(FactoMineR)
library(dplyr)
library(psych)
library(reshape2)
library(knitr)
library(tidyverse)
library(biotools)
library(MASS)
library(caret)
library(klaR)

data("mtcars")

## Preprocesamiento de datos ##

mtcars$vs <- factor(mtcars$vs)
mtcars$vs <- recode_factor(mtcars$vs, `0`="V-shaped",`1`="Straight")
mtcars$am <- factor(mtcars$am)
mtcars$am <- recode_factor(mtcars$am, `0`="Automatic",`1`="Manual")

head(mtcars)

corcars <- cor(mtcars[c(1:7,10:11)])

cortest.bartlett(corcars,n = 32)

mt_pca <- PCA(mtcars,quali.sup = c(8,9),graph = TRUE,scale.unit = TRUE)

barplot(mt_pca$eig[,1], main="Valores propios", names.arg=paste("dim",1:nrow(mt_pca$eig)))

round(mt_pca$ind$contrib[,1:2],2)
round(mt_pca$var$contrib[,1:2],2)

dimdesc(mt_pca,axes = c(1,2))

#### Análisis Discriminante Lineal ####
set.seed(4830201)
data("iris")

index_set <- createDataPartition(iris$Species,times=1,p=0.7,list =F)
train_iris <- iris[index_set,]
test_iris <- iris[-index_set,]

pairs.panels(train_iris[1:4],bg = c("black","yellow","red")[train_iris$Species],pch=21,ellipses = F)

## Validación de supuestos
train_iris_m <- melt(train_iris)
train_iris_m %>% group_by(Species,variable) %>% summarise(pv_shapiro = round(shapiro.test(value)$p.value,5))

MVN::mvn(data = train_iris[,1:4],multivariateOutlierMethod = "quan")
test_mvn_st <- MVN::mvn(data = train_iris[,1:4],mvnTest="hz")
test_box <- boxM(train_iris[,1:4], grouping = train_iris$Species)

## Clasificación

qda_train <- qda(formula = Species ~., data = train_iris)
qda_train

test_predict <- predict(qda_train,test_iris)
table(test_iris$Species,test_predict$class,dnn = c("Clase real","Clase predicha"))

error <- mean(test_iris$Species != test_predict$class) * 100
paste("Error del clasificador: ",error,"%")

## Visualización

partimat(Species~.,data = train_iris,method="qda",prec=200,nplots.hor=3,image.colors = c("darkgoldenrod1", "snow2", "skyblue2"),col.mean = "firebrick")

#### Análisis factorial ####

data(bfi)

## Pre-procesamiento de datos ##

bfi=bfi[complete.cases(bfi),]
bfi$education = as.factor(bfi$education)
bfi$gender = as.factor(bfi$gender)

## Matriz de correlación ##

cor_bfi <- cor(bfi[,-c(26,27)])

## Test de hipótesis

test_bart_af <- cortest.bartlett(cor_bfi,n=dim(bfi)[1])
test_mvn_af <- MVN::mvn(data = bfi[,-c(26,27)],mvnTest="hz")
KMO(cor_bfi)

## Método de máxima verosimilitud 
fit.ml.rot <- fa(cor_bfi,nfactors = 5,rotate = "varimax",fm="ml",n.obs = dim(bfi)[1])

fit.ml.rot

