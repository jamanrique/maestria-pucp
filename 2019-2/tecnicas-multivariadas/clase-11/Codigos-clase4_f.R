# Introducción de la matriz de datos
datos <- matrix(c(
  0.0, 1.0, 2.1, 6.1, 5.2,
  1.0, 0.0, 2.4, 6.9, 5.3,
  2.1, 2.4, 0.0, 5.1, 4.1,
  6.1, 6.9, 5.1, 0.0, 3.1,
  5.2, 5.3, 4.1, 3.1,0.0), ncol=5, nrow=5, byrow=T,
  dimnames=list(c("X1","X2","X3","X4","X5")))

# distancias unicas sin repetición
vec <- c(
  0.0, 1.0, 2.1, 6.1, 5.2,
  1.0, 0.0, 2.4, 6.9, 5.3,
  2.1, 2.4, 0.0, 5.1, 4.1,
  6.1, 6.9, 5.1, 0.0, 3.1,
  5.2, 5.3, 4.1, 3.1,0.0)
sort(unique(vec ))

# Escalamiento multidimensional
library(smacof)
fit2 <- mds(delta=datos,ndim=2, type="interval")
plot(fit2, main = "Datos de la imagen de cadenas de electrodomésticos")

# Solución bidimensional
fit2$conf

# Distancias entre configuraciones
print(fit2$confdist)

# matriz de disparidades
print(fit2$dhat)

# Diagrama de Shepard
plot(fit2,plot.type="Shepard",plot.dim=c(1,2),sphere=TRUE,bubscale=0.1,col=1,
     label.conf=list(label=TRUE,pos=3,col=1,cex=0.8),
     shepard.x=NULL,identify=FALSE,
     type="p",pch=20,asp=1,col.hist=NULL)

# Stress
print(fit2$stress)

# RSQ o coeficiente de determinación
print(1-fit2$rss)

# Stress por punto
print(fit2$spp)

plot(fit2, plot.type = "bubbleplot")
plot(fit2, plot.type = "stressplot")

# Desarrollo educativo de distintas zonas del mundo

datos <- read.table("educa.txt", header=F)
rownames(datos)<- c("Z1", "Z2", "Z3","Z4", "Z5", "Z6", "Z7", "Z8", "Z9", "Z10","Z11","Z12")
colnames(datos) <- c("I1","I2","I3","I4","I5","I6","I7")
# Normalizamos los indicadores
datos_norm <- scale(datos)
# Calculamos la matriz de distancias
datosf <-  dist(datos_norm,method = "euclidian",diag=T,upper=T)
m <- as.matrix(datosf)
datos <- as.dist(m)

library(smacof)
fit <- mds(delta=datos,ndim=2,type="ratio")
# coordenadas
print(fit$conf)
# disparidades
print(fit$dhat)
# distancias entre configuraciones
print(fit$confdist)
# Stress
print(fit$stress)
# Sress por punto
print(fit$spp)

dist <- cbind(c(fit$dhat))
dism <- cbind(c(fit$confdist))
summary(lm(dist~dism))

plot(fit,plot.type="confplot",plot.dim=c(1,2),sphere=TRUE,bubscale=0.1,col=1,
     label.conf=list(label=TRUE,pos=3,col=1,cex=0.8),
     shepard.x=NULL,identify=FALSE,
     type="p",pch=20,asp=1,col.hist=NULL)

plot(fit,plot.type="Shepard",plot.dim=c(1,2),sphere=TRUE,bubscale=0.1,col=1,
     label.conf=list(label=TRUE,pos=3,col=1,cex=0.8),
     shepard.x=NULL,identify=FALSE,
     type="p",pch=20,asp=1,col.hist=NULL)


svec <- NULL
for (i in 1:7) {
  svec[i] <- mds(delta=datos, ndim = i, type = "ordinal")$stress
}

plot(seq(1:7),svec,type="overplotted", pch=16, ylab="Stress",xlab="Número de dimensiones")

hier <- hclust(fit$confdist,method="ward.D2")
plot(hier)

# datos de psicologia clínica

library("MPsychoR")
library("smacof")
data(Wenchuan)
Wdelta <- dist(t(Wenchuan)) ## Euclidean distances
fit.wenchuan1 <- mds(Wdelta, type = "ordinal") ## MDS fit
fit.wenchuan1

plot(fit.wenchuan1, main = "Wenchuan MDS")

set.seed(123)
rsvec <- randomstress(n = attr(Wdelta, "Size"), ndim = 2,
                      nrep = 500, type = "ordinal")
mean(rsvec)

mean(rsvec) - 2*sd(rsvec)

set.seed(123)
permmds <- permtest(fit.wenchuan1, data = Wenchuan,
                    method.dat = "euclidean", nrep = 500,
                    verbose = FALSE)
permmds

n <- attr(Wdelta, "Size")
svec <- NULL
for (i in 1:(n-1)) {
  svec[i] <- mds(Wdelta, ndim = i, type = "ordinal")$stress
}

plot(seq(1:(n-1)),svec,type="overplotted", pch=16, ylab="Stress",xlab="Número de dimensiones", main="MDS Scree Plot")

set.seed(123)
fit.wenchuan <- NULL ## 100 random starts
for(i in 1:100) {
  fit.wenchuan[[i]] <- mds(Wdelta, type = "ordinal",
                           init = "random")
}
## extract the best solution
ind <- which.min(sapply(fit.wenchuan,
                        function(x) x$stress))
fit.wenchuan2 <- fit.wenchuan[[ind]]
fit.wenchuan2$stress ## lowest stress (random start)

fit.wenchuan1$stress ## stress (classical scaling start)

fit.wenchuan3 <- mds(Wdelta, type = "interval")
fit.wenchuan3

plot(fit.wenchuan2, plot.type = "Shepard",
     main = "Shepard Diagram (Ordinal MDS)")

plot(fit.wenchuan2, plot.type = "stressplot",
     main = "Wenchuan Stress-per-Point")





