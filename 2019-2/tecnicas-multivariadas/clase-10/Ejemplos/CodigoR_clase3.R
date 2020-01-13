########################
## Análisis Factorial ##
########################

###############
## Ejemplo 1 ##
###############

base=read.csv("CalBi.csv")
head(base)
base
summary(base)

library(psych)
# Test de esfericidad de Bartlett
Rcor=cor(base, use ="pairwise.complete.obs")
Rcor
det(Rcor)
print(cortest.bartlett(Rcor,n=nrow(base)))
# Índice KMO
KMO(base)
# Número de factores
sedimentacion=princomp(base, scores=T, cor=T)
plot(sedimentacion, type="lines")
# Componentes principales sin rotación
CPAnorotado1=principal(base, nfactor=12, rotate="none", use=pairwise)
CPAnorotado1
CPAnorotado2=principal(base, nfactor=3, rotate="none", use=pairwise)
CPAnorotado2
# Componentes principales rotacdos
CPArotado2=principal(base, nfactor=3, rotate="varimax")
CPArotado2
fa.diagram(CPArotado2)

###############
## Ejemplo 2 ##
###############

vida <- read.table("vida.txt",header=T)
rownames(vida) <- c("Algeria", "Cameroon", "Madagascar", "Mauritius",
                    "Reunion", "Seychelles", "South Africa(C)", "South Africa(W)", "Tunisia",
                    "Canada", "Costa Rica", "Dominican Rep", "El Salvador", "Greenland",
                    "Grenada", "Guatemala", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                    "Panama", "Trinidad(62)", "Trinidad (67)", "United States (66)", "United States (NW66)", "United States (W66)", "United States (67)", "Argentina",
                    "Chile", "Columbia", "Ecuador")
vida
# Test de esfericidad de Bartlett
Corre <- cor(vida)
det(Corre)
library(psych)
print(cortest.bartlett(Corre,n=nrow(vida)))
# Índice KMO
KMO(vida)
# Número de factores
sedimentacion=princomp(vida, scores=T, cor=T)
# dos factores
vida.fa2 <-factanal(vida,factors=2, method="mle",scores="regression")
vida.fa2
# tres factores
vida.fa3 <-factanal(vida,factors=3, method="mle",scores="regression")
vida.fa3
# biplots
windows()
biplot(vida.fa3$scores[,1:2], vida.fa3$loadings[,1:2])
vida
windows()
biplot(vida.fa3$scores[,c(1,3)], vida.fa3$loadings[,c(1,3)])
plot(sedimentacion, type="lines")
windows()
biplot(vida.fa3$scores[,2:3], vida.fa3$loadings[,2:3])

###############
## Ejemplo 3 ##
###############

# librerias necesarias para realizar el análisis
library(psych)
library(GPArotation)
# cargando el conjunto de datos
data <- read.csv("EFA.csv")
# visualizando una pequeña parte del conjunto de datos
head(data)
# Número de componentes
parallel <- fa.parallel(data, fm = 'minres', fa = 'fa')
# probando con tres factores
threefactor <- fa(data,nfactors = 3,rotate = "oblimin",fm="minres")
print(threefactor)
print(threefactor$loadings,cutoff = 0.3)
# probando 4 factores
fourfactor <- fa(data,nfactors = 4,rotate = "oblimin",fm="minres")
print(fourfactor$loadings,cutoff = 0.3)
# mapeo de los factores
fa.diagram(fourfactor)
fourfactor


