rm(list = ls())
library(sampling)
library(survey)
data(api)
attach(apipop)
K <- dim(apipop)[1]                 # se determina el total de las unidades secundarias en la poblaci?n
N <- length(table(apipop$dnum))     # se determina el total de cl?sters dentro de la poblaci?n
n <- 15                             # se determina la poblaci?n a muestrear

aux1 <- sampling::cluster(apipop, clustername = c("dnum"), n, method = c("srswor"), description = TRUE)     # se determina la muestra a sacar
samplec1 <- getdata(apipop, aux1) # se extrae la muestra de la poblaci?n
Mhat <- dim(aux1)[1] # la muestra saca 15 distritos con XX colegios (el primer d?gito)
aux = data.frame(fpc = rep(N, Mhat), pw = rep(K / Mhat, Mhat))
samplec1 = cbind(numc = 1:Mhat, samplec1, aux)

dclus1 <- svydesign(ids = ~dnum, fpc = ~fpc, data = samplec1, weights = ~pw) # para identificar que es muestreo sin reemplazamiento, se pone el fpc.
svytotal(~enroll, dclus1)
svymean(~api00, dclus1)

dclus2 <- svydesign(ids = ~dnum + snum,fpc = ~fpc1+fpc2,data = apiclus2, weights = ~pw)
aux2 <- mstage(apipop, stage = list("cluster", "cluster"), varnames = list("dnum", "snum"), size = c(list(757, 40), list(40,rep(5, 40))), method = c("srswor", "srswor"))
svytotal(~enroll, dclus2, na.rm = TRUE)
