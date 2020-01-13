#############################################
### Método de los componentes principales ###
#############################################

r <- c(1.00,
       0.439,1.00,
       0.410,0.351,1.000,
       0.288, 0.354,0.164,1.000,
       0.329,0.320,0.190,0.595,1.000,
       0.248,0.329,0.181,0.470,0.464,1.000)
r
# cargamos la libreria lavaan
library(lavaan)
# convertimos el vector r en la matrix R
R <- lav_matrix_lower2full(r)
# Etiquetamos a las variables de la matriz
colnames(R)<-rownames(R)<-
 c("Gae","Eng","His","Ari","Alg","Geo")
# obtenemos los autovalores y autovectores
auto<-svd(R)
auto
# generamos la matriz diagonal de los autovalores
av<-diag(auto$d)
av
# obtenemos la matriz LAMBDA cpn las cargas factoriales
LAMBDA<-auto$u%*%sqrt(av)
LAMBDA
#cargando la libreria para obtener el EFA
library(psych)
#estimación del EFA mediante el método de componentes principales
fit.pca <- principal(R,nfactors=2,rotate="none")
fit.pca
#estimación del EFA por el metodos de componentes principales iteradas o ejes principales
fit.pa <- fa(R,nfactors=2,fm="pa",rotate="none",n.obs=220)
fit.pa
#estimación del EFA mediante el método de máxima verosimilitud
fit.ml <- fa(R,nfactors=2,fm="ml",rotate="none",n.obs=220)
fit.ml
#representación gráfica de las tres estimaciones
plot(fit.pca,labels=row.names(R),cex=0.7,ylim=c(-0.8,0.8))
plot(fit.pa,labels=row.names(R),cex=0.7,ylim=c(-0.8,0.8))
plot(fit.ml,labels=row.names(R),cex=0.7,ylim=c(-0.8,0.8))
#determinación del número de factores
# considerando autovalor>1, sedimentación, paralelo
fa.parallel(R,fm="pa",n.obs=220,ylabel="Eigenvalues")
fa.parallel(R,fm="ml",n.obs=220,ylabel="Eigenvalues")
# cargando libreria para test de Barlett
library(nFactors)
# test de Barlett
nBartlett(R,N=220,alpha=0.01,cor=TRUE, details=TRUE)
# rotación ortogonal Varimax
fit.pa.rot.ort <- fa(R,nfactors=2,fm="pa",rotate="varimax",n.obs=220)
fit.pa.rot.ort
# rotación oblicua
fit.pa.rot.obl <- fa(R,nfactors=2,fm="pa",rotate="oblimin",n.obs=220)
fit.pa.rot.obl
# contraste de esfericidad de Bartlett
cortest.bartlett(R,n=220)
# medidas KMO de adecuación muestral general e individual
KMO(R)
# puntuaciones factoriales
factor.scores(R,fit.pca,method = "Thurstone")$weights
