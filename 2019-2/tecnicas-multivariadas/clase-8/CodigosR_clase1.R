###############
## Ejemplo 1 ##
###############

# Cargando la libreria
library(FactoMineR)
# cargando el conjunto de datos
naranja <- read.table("http://factominer.free.fr/libra/naranja.csv",header=TRUE, sep=";", dec=".", row.names=1)
naranja
# Correlaciones entre las variables
cor(naranja[,1:7])
# Componentes principales
res.pca <- PCA(naranja,quanti.sup=8:14,quali.sup=15:16)
# Gráfico de componentes e individuos
# variabes cualitativas suplementarias invisibles
windows()
plot(res.pca, invisible="quali")
# Correlación entre variables y componentes
round(res.pca$var$coord[,1:2],2)
# gráfico de las correlaciones de variables y componentes
# variables suplementarias invisibles
windows()
plot(res.pca,choix="var",invisible="quanti")
# Variabilidad recogida por los componentes
round(res.pca$eig,2)
# Contribuciones de los individuos a los componentes
round(res.pca$ind$contrib[,1:2],2)
# Contribuciones de las variables a los componentes
round(res.pca$var$contrib[,1:2],2)
# gráfico de las correlaciones de variables y componentes
# considerando individuos suplementarios
windows()
plot(res.pca,choix="var")
# Gráfico de componentes e individuos
# con variabes cualitativas suplementarias
windows()
plot(res.pca)
# Descripción automática de los ejes
dimdesc(res.pca)

###############
## Ejemplo 2 ##
###############
# Cargando la libreria
library(FactoMineR)
# cargando el conjunto de datos
gastos <- read.table("http://factominer.free.fr/libra/gastos.csv", header=TRUE, sep=";", row.names=1)
gastos
# Estandarización o no estandarización de variables
variab <- cbind(apply(gastos[1:7,],2,mean),
                apply(gastos[1:7,],2,sd)*sqrt(6/7),
                apply(gastos[1:7,],2,sd)*sqrt(6/7)/apply(gastos[1:7,],2,mean))
colnames(variab) <- c("Media","D.E","CV")
variab
# Componentes principales
res.pca2 <- PCA(gastos,ind.sup=8:18,quanti.sup=27:30)
# Elección del número de dimensiones a estudiar
round(res.pca2$eig,2)
# Gráfico de barras de la variabilidad recogida por componente
windows()
barplot(res.pca2$eig[,1], main="Valores propios", names.arg=paste("dim",1:nrow(res.pca2$eig)))
# Estudio de la nube de los individuos activos* 
windows()
plot.PCA(res.pca2, choix="ind", invisible="ind.sup")
# Contribuciones de cada individuo
round(res.pca2$ind$contrib[,1:3],2)
# Estudio de la nube de las variables
windows()
plot.PCA(res.pca2, choix="var", invisible="quanti.sup")
# Correlación de variables con componentes y sus contribuciones
round(cbind(res.pca2$var$coord[,1:3],res.pca2$var$contrib[,1:3]),2)
# Gráfico de variables y componetes
windows()
plot.PCA(res.pca2, choix="var", invisible="var")
# Correlación de variables suplemenmtarias con componentes
round(res.pca2$quanti.sup$coord[,1:3],2)
# Descripción automática de las dimensiones
dimdesc(res.pca2)
# Estudio de la nube de los individuos activos y suplementarios
windows()
plot.PCA(res.pca2, choix="ind")
# Plano 2-3
# Estudio de la nube de individuos
windows()
plot(res.pca2, choix="ind", invisible="ind.sup", axes=2:3)
# Estudio de la nube de las variables
windows()
plot(res.pca2, choix="var", invisible="quanti.sup", axes=2:3)
# Contribucion de individuos
round(res.pca2$ind$contrib[,1:3],2)
# Correlación de variables y componentes
round(res.pca2$var$coord[,1:3],2)

###############
## Ejemplo 3 ##
###############
# Cargando el conjunto de datos
usair.dat<-source("usair.dat")$value
attach(usair.dat)
usair.dat[,2]<- -usair.dat[,2]
names(usair.dat)=c("SO2","Temp", "Manuf","Pop","Wind","Precip","Days")

# Correlación entre las variables
cor(usair.dat[,-1])
# Componentes principales
usair.pc<-princomp(usair.dat[,-1],cor=TRUE,fix_sign = FALSE)
summary(usair.pc,loadings=TRUE)
# Como obtener los resultados sin la función
S=cor(usair.dat[,-1])
eigen(S)
#Las desviaciones estandar
sqrt(eigen(S)$values)
#La proporción de las varianzas
eigen(S)$values/sum(eigen(S)$values)
#Las varianzas acumuladas
cumsum(eigen(S)$values/sum(eigen(S)$values))
#correlaciones de variables con componentes 
usair.pc$loadings%*%diag(usair.pc$sdev)
# estudio de la nube de individuos considerando tres planos
windows()
par(pty="s")
plot(usair.pc$scores[,1],usair.pc$scores[,2],
     ylim=range(usair.pc$scores[,1]),
     xlab="PC1",ylab="PC2",type="n",lwd=2)
text(usair.pc$scores[,1],usair.pc$scores[,2],
     labels=abbreviate(row.names(usair.dat)),cex=0.7,lwd=2)
#
windows()
par(pty="s")
plot(usair.pc$scores[,1],usair.pc$scores[,3],
     ylim=range(usair.pc$scores[,1]),
     xlab="PC1",ylab="PC3",type="n",lwd=2)
text(usair.pc$scores[,1],usair.pc$scores[,3],
     labels=abbreviate(row.names(usair.dat)),cex=0.7,lwd=2)
#
par(pty="s")
plot(usair.pc$scores[,2],usair.pc$scores[,3],
     ylim=range(usair.pc$scores[,2]),
     xlab="PC2",ylab="PC3",type="n",lwd=2)
text(usair.pc$scores[,2],usair.pc$scores[,3],
     labels=abbreviate(row.names(usair.dat)),cex=0.7,lwd=2)
# Análisis de regresión de variable respuesta con componentes
summary(lm(SO2~usair.pc$scores[,1]+usair.pc$scores[,2]+
             usair.pc$scores[,3]))