#### Trabajo 2: Técnicas de Análisis Multivariado
#### 20091107 - Justo Manrique Urbina
#### Prof. Rocío Maehara

#### Escalamiento multidimensional ####
library(smacof)
data(swiss)
dswiss <- dist(swiss,method = "euclidian",diag = T,upper = T)



m <- as.matrix(dswiss)
dist_swiss <- as.dist(m)

svec <- NULL
for (i in 1:dim(swiss)[2]) {
  svec[i] <- mds(delta=dist_swiss, ndim = i, type = "interval")$stress
}

plot(seq(1:dim(swiss)[2]),svec,type="overplotted", pch=16, ylab="Stress",xlab="Número de dimensiones")

plot(fit.mds,plot.type="Shepard",plot.dim=c(1,2),sphere=TRUE,bubscale=0.1,col=1,
     label.conf=list(label=TRUE,pos=3,col=1,cex=0.8),
     shepard.x=NULL,identify=FALSE,
     type="p",pch=20,asp=1,col.hist=NULL)

plot(fit.mds, plot.type = "stressplot")

fit.mds <- mds(delta = dist_swiss,ndim=2,type = "interval")
fit.mds

plot(fit.mds,plot.type="confplot",plot.dim=c(1,2),sphere=TRUE,bubscale=0.1,col=1,
     label.conf=list(label=TRUE,pos=3,col=1,cex=0.8),
     shepard.x=NULL,identify=FALSE,
     type="p",pch=20,asp=1,col.hist=NULL)

#### Análisis de conglomerados ####

data("USArrests")
USA_scale <- as.data.frame(scale(USArrests))

mydata <- USA_scale
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Numero de cluster",
     ylab="Dentro de los grupos suma de cuadrados",
     main="Evaluar el numero optimo de conglomerados con el metodo del codo",
     pch=20, cex=2)

set.seed(350)
USA_means <- kmeans(USA_scale,centers = 4,nstart = 100)

USA_cluster <- as.data.frame(USA_means$cluster)
USA_cluster$ciudad <- rownames(USA_cluster)
USA_cluster <- USA_cluster[order(USA_cluster$`USA_means$cluster`),]
USA_cluster

data(USA)