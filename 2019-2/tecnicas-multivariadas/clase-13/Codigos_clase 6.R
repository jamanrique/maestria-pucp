
# Códigos para el gráfico del ejemplo

Empresa <- c("E1","E2","E3","E4","E5","E6","E7","E8")
Inversion_publicitaria <- c(16,12,10,12,45,50,45,50)
Ventas <- c(10,14,22,25,10,15,25,27)
datos <- data.frame(Inversion_publicitaria,Ventas)
attach(datos)
row.names(datos)<-Empresa
plot(Inversion_publicitaria,Ventas,pch=16,xlab="Inversión",ylab="Ventas",xlim=c(5,60),ylim=c(5,30))
with(datos, text(Ventas~Inversion_publicitaria, labels = row.names(datos), pos = 4))

# Distancia euclidiana
matriz.dis.euclid <- dist(datos[,1:2],method="euclidian",diag=TRUE)
round(matriz.dis.euclid,2)

# Estandarización de los datos

nombre.empresa2<-c("E1","E2","E3","E4","E5","E6","E7","E8")
activos<-c(10.0e9,10.5e9,10.0e9,10.5e9,20.0e9,20.5e9,20.0e9,20.5e9)
trabajadores<-c(100,90,200,190,200,190,100,90)

Datos_SEST<-data.frame(nombre.empresa2,activos,trabajadores)
Datos_SEST

matriz.dis.euclid2<-dist(Datos_SEST[,c("activos","trabajadores")],method="euclidean",diag=TRUE)
matriz.dis.euclid2

# Usando puntuaciones z
Datos_EST<-scale(Datos_SEST[,c("activos","trabajadores")])
matriz.dis.euclid.norm<-dist(Datos_EST[,c("activos","trabajadores")],method="euclidean",diag=TRUE)
round(matriz.dis.euclid.norm,2)

#####################################
######## Métodos jerárquicos ########
#####################################

# Método del centroide

library(stats)
#calculo de la distancia euclídea
matriz.dis.euclid<-dist(datos[,c("Inversion_publicitaria","Ventas")],method="euclidean",diag=TRUE)

#calculo de la distancia euclidea al cuadrado
matriz.dis.euclid2<-(matriz.dis.euclid)^2
matriz.dis.euclid2

#efectuamos el cluster con método centroide
hclust.centroide<-hclust(matriz.dis.euclid2,method="centroid")

#Saca el historial de aglomeración del objeto hclust.centroide
data.frame(hclust.centroide[2:1])

#dendograma centroide
plot.hclust<-plot(hclust.centroide)
rect.hclust(hclust.centroide, k = 2, border = "red")

#efectuamos el cluster con método centroide con método del vecino más cercano
hclust.cercano<-hclust(matriz.dis.euclid2,method="single")
#Saca el historial de aglomeración del objeto hclust.cercano
data.frame(hclust.cercano[2:1])

#efectuamos el cluster con método vecino más lejano
hclust.lejano<-hclust(matriz.dis.euclid2,method="complete")
#Saca el historial de aglomeración del objeto hclust.lejano
data.frame(hclust.lejano[2:1])

#efectuamos el cluster con método vinculación promedio
hclust.promedio<-hclust(matriz.dis.euclid2,method="average")
data.frame(hclust.promedio[2:1])

#efectuamos el cluster con método de Ward
hclust.ward<-hclust(matriz.dis.euclid2,method="ward.D")
data.frame(hclust.ward[2:1])


### Uso de indicadores para elegir número de clusters

#Generación de la base de datos simulada

set.seed(1)
x<-rbind(matrix(rnorm(100,sd=0.1),ncol=2),
         matrix(rnorm(100,mean=1,sd=0.2),ncol=2),
         matrix(rnorm(100,mean=5,sd=0.1),ncol=2),
         matrix(rnorm(100,mean=7,sd=0.2),ncol=2))

DatosCaso3.2<-data.frame(x)
#Gráfico
library(ggplot2)
ggplot(data=DatosCaso3.2, aes(X1,X2)) + geom_point()

#Pedimos los indicadores para un jerárquico con distancia euclídea y método de ward

library(NbClust)
res<-NbClust(DatosCaso3.2, distance = "euclidean", min.nc=2, max.nc=8, method = "ward.D2", index = "alllong")

res$All.index
res$Best.nc
res$Best.partition

