rm(list=ls())
library(psych)
install.packages("stargazer")
library(stargazer)
set.seed(12430)

M = 10000

n = c(30,50,100)
media = 1
var = 1

T_mat<-matrix(0,M,18)

colnames(T_mat)<-c("Xbar.30.N","Xwin.30.N","Xbar.30.T","Xwin.30.T","Xbar.30.G","Xwin.30.G","Xbar.50.N","Xwin.50.N","Xbar.50.T","Xwin.50.T","Xbar.50.G","Xwin.50.G","Xbar.100.N","Xwin.100.N","Xbar.100.T","Xwin.100.T","Xbar.100.G","Xwin.100.G")

for(h in 1:3){
  for(j in 1:M){
    x = rnorm(n = n[h],mean = media,sd = var)
    y = media + var*rt(n = n[h],df = 4)
    z = rgamma(n = n[h],shape = media, scale = var)
    T_mat[j,(6*h-5):(6*h)] <- c(mean(x),winsor.mean(x = x,trim = 0.2),mean(y),winsor.mean(x = y,trim = 0.2),mean(z),winsor.mean(x = z,trim = 0.2))
  }
}

medias = colMeans(T_mat)

est = rep(c("Media aritmética", "Media winsorisada"),times=9)
dis = rep(c("N","N","t","t","G","G"),times=3)
tam = rep(n,each=4)

sesgo = medias - media

varianza = diag(var(T_mat))

ECM = varianza + sesgo^2

res_ecm = rbind(matrix(ECM[dis=="N"],3,2,byrow=T),matrix(ECM[dis=="t"],3,2,byrow=T),matrix(ECM[dis=="G"],3,2,byrow=T))
res_ecm = cbind(rep(n,3),res_ecm)

colnames(res_ecm) = c("Tamaño de Muestra","Media Aritmética","Media Winsorizada")

rownames(res_ecm) = rep(c("Normal","t","Gamma"),each = 3)

res_ecm = round(res_ecm,4)

res_ses = rbind(matrix(sesgo[dis=="N"],3,2,byrow=T),matrix(sesgo[dis=="t"],3,2,byrow=T),matrix(sesgo[dis=="G"],3,2,byrow=T))
res_ses = cbind(rep(n,3),res_ses)

colnames(res_ses) = c("Tamaño de Muestra","Media Aritmética","Media Winsorizada")

rownames(res_ses) = rep(c("Normal","t","Gamma"),each = 3)

res_ses = round(res_ses,4)

stargazer(res_ses)

