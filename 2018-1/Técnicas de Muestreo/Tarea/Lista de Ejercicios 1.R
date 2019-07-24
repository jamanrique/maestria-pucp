## Técnicas de Muestreo
## Lista de Ejercicios 1
## Nombre: Justo Manrique Urbina
## Código: 20091107

################################################
################## Pregunta 3 ##################
################################################

#################     3.a     ##################
rm(list = ls())
# Resolución del ejercicio mediante MASs

# 1. Creación del vector poblacional
pob <- c(13.9,11.5,16.7,14.4, 14.6, 15.1)

# 2. Carga de la librería combinat
library(combinat)

# 3.1 Creación de la matriz de la combinatoria, y generación de la media por fila
samples_pob <- t(as.matrix(combn(pob,3)))
spobmean <- cbind(samples_pob,apply(samples_pob,1,mean))

# 4.1 Cálculo de probabilidades mayor a 14 g/dl

sum(spobmean[,4] > 14)/length(spobmean[,4])

# Resolución del ejercicio mediante MASc

#3.2 Cración de la matriz y generación de la media por fila
samples_pob_c <- expand.grid(rep(list(pob),3))
spobmean_c <- cbind(samples_pob_c,apply(samples_pob_c,1,mean))

#4,2 Cálculo de probabilidades mayor a 14g/dl
sum(spobmean_c[,4] > 14)/length(spobmean_c[,4])


#################     3.b     ##################

# Resolución del ejercicio mediante MASs

# 1. Añadir las probabilidades de cada muestra
probs <- rep(1/length(samples_pob[,1]),length(samples_pob[,1]))

# 2. Añadir mediana, mediana, y probabilidad a samples_pob
spobmeanvarmed <- cbind(samples_pob,apply(samples_pob,1,mean),apply(samples_pob,1,median),apply(samples_pob,1,var),probs)

# 3. Determinar la probabilidad de la media, medianta y varianza
meanaggr <- aggregate(spobmeanvarmed[,7],by=list(spobmeanvarmed[,4]),sum)
colnames(meanaggr) <- c("Media Muestral","Probabilidad")
medaggr <-  aggregate(spobmeanvarmed[,7],by=list(spobmeanvarmed[,5]),sum)
colnames(medaggr) <- c("Mediana Muestral", "Probabilidad")
varaggr <- aggregate(spobmeanvarmed[,7],by=list(spobmeanvarmed[,6]),sum)
colnames(varaggr) <- c("Varianza Muestral", "Probabilidad")

# 4. Determinar la media, varianza y mediana
rep3b <- c(sum(meanaggr[,1]*meanaggr[,2]),sum(varaggr[,1]*varaggr[,2]),sum(medaggr[,1]*medaggr[,2]))

# 5, Determinar la varianza de la media/mediana
sum(((meanaggr[,1]-sum(meanaggr[,1]*meanaggr[,2]))^2)*meanaggr[,2])
sum(((medaggr[,1]-sum(medaggr[,1]*medaggr[,2]))^2)*medaggr[,2])

# Rpta: Mediana tiene sesgo pero tiene menor varianza que la media (?)

# Resolución del ejercicio mediante MASc

# 1. Añadir las probabilidades de cada muestra
probs_c <- rep(1/length(samples_pob_c[,1]),length(samples_pob_c[,1]))

# 2. Añadir mediana, mediana, y probabilidad a samples_pob
spobmeanvarmed_c <- cbind(samples_pob_c,apply(samples_pob_c,1,mean),apply(samples_pob_c,1,median),apply(samples_pob_c,1,var),probs_c)

# 3. Determinar la probabilidad de la media, medianta y varianza
meanaggr_c <- aggregate(spobmeanvarmed_c[,7],by=list(spobmeanvarmed_c[,4]),sum)
colnames(meanaggr) <- c("Media Muestral","Probabilidad")
medaggr_c <-  aggregate(spobmeanvarmed_c[,7],by=list(spobmeanvarmed_c[,5]),sum)
colnames(medaggr) <- c("Mediana Muestral", "Probabilidad")
varaggr_c <- aggregate(spobmeanvarmed_c[,7],by=list(spobmeanvarmed_c[,6]),sum)
colnames(varaggr) <- c("Varianza Muestral", "Probabilidad")

# 4. Determinar la media, varianza y mediana
rep3b_c <- c(sum(meanaggr_c[,1]*meanaggr_c[,2]),sum(varaggr_c[,1]*varaggr_c[,2]),sum(medaggr_c[,1]*medaggr_c[,2]))

# 5, Determinar la varianza de la media/mediana
sum(((meanaggr_c[,1]-sum(meanaggr_c[,1]*meanaggr_c[,2]))^2)*meanaggr_c[,2])
sum(((medaggr_c[,1]-sum(medaggr_c[,1]*medaggr_c[,2]))^2)*medaggr_c[,2])

################################################
################## Pregunta 9 ##################
################################################

rm(list=ls())
z = qnorm(1-0.05/2)
p = 2/30
error = 3/100
N = 200

## Intervalo de Wald ##
z^2*p*(1-p)/e^2

## Intervalo de Wilson ##
((z^2*p*(1-p))*N) / ((z^2*p*(1-p)) + error^2*(N-1))




