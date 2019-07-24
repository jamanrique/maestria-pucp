# Limpieza de ambiente para ejecución de script
rm(list=ls())

# Carga de datos y seteo de semilla
library(haven)
load("D:/ce2s16Cz.rdata")
set.seed(12329)

#Afijación de muestra según asignación de Neyman
Pop = ce2s16Cz
Pop$Estrato=interaction(Pop$Area,Pop$Gestion)
Pop = Pop[order(Pop$Estrato),]
table(Pop$Estrato)
Nh = as.vector(table(Pop$Estrato))
sigmah = sd(Pop$M500_M[Pop$Estrato=="Urbana.Estatal"][sample(Nh[1],10)])
sigmah[2] = sd(Pop$M500_M[Pop$Estrato=="Rural.Estatal"][sample(Nh[2],10)])
sigmah[3] = sd(Pop$M500_M[Pop$Estrato=="Urbana.No estatal"][sample(Nh[3],10)])
sigmah[4] = sd(Pop$M500_M[Pop$Estrato=="Rural.No estatal"][sample(Nh[4],10)])
ah = Nh*sigmah/sum(Nh*sigmah)
d = dim(Pop)[1]*5/qnorm(0.975)
n = sum(((Nh*sigmah)^2)/ah)/(d^2 + sum(Nh*sigmah^2))
nh = round(ah*n)

# Determinación de la muestra 
library(sampling)
set.seed(12345)
m=strata(Pop,c("Estrato"),size=nh,method="srswor")
me16Am = getdata(Pop,m)
table(is.na(me16Am$M500_M))
me16Am = me16Am[is.na(me16Am$M500_M)==0,]
nh = as.vector(table(me16Am$Estrato))
nh
me16Am = cbind(me16Am,fpc = rep(Nh,nh))
save(me16Am,file="D:/me16Am.RData")

#### Pregunta 12.a ####
library(survey)
dis16MAE = svydesign(id=~1,strata=~Estrato,fpc=~fpc,data=me16Am)
svyby(~M500_M,~Sexo,design=dis16MAE,svymean)

#### Pregunta 12.b ####
svyby(~M500_M,~Gestion+~Sexo,design=dis16MAE,svymean,vartype = "var")
