library(haven)
library(expss)
library(dplyr)
library(ggplot2)
library(car)
library(GGally)
library(glmnet)
library(MASS)
library(stargazer)

salud=read_sav("02_C1_CAPITULOS.sav",user_na=TRUE)
nombres = c("DEPARTAMENTO","INSTITUCION","C1TURNO","C1P77","C1P5","C1P9","C1P23","C1P30","C1P56","C1P80","C1INDICERIQUEZA","C1P12_H","C1P12_M","C1P10_H","C1P10_M")
salud = subset(salud ,select = nombres)
salud = na.omit(salud)
salud = salud  %>% mutate(C1P12_H = as.numeric(C1P12_H),C1P12_M = as.numeric(C1P12_M),C1P10_H = as.numeric(C1P10_H),C1P10_M = as.numeric(C1P10_M)) %>% mutate(tiempo.llegada = C1P10_H + C1P10_M/60,tiempo.espera   = C1P12_H + C1P12_M/60 - C1P10_H - C1P10_M/60)  
salud = salud %>% dplyr::select(-c("C1P12_H","C1P12_M","C1P10_H","C1P10_M"))

salud$DEPARTAMENTO= as.factor(salud$DEPARTAMENTO)
salud$INSTITUCION= as.factor(salud$INSTITUCION)
salud$C1TURNO=as.factor(salud$C1TURNO)
salud$C1P5=as.factor(salud$C1P5)
salud$C1P30=as.factor(salud$C1P30)
salud$C1P56=as.factor(salud$C1P56)
salud$C1P80=as.factor(salud$C1P80)
salud$C1INDICERIQUEZA=as.factor(salud$C1INDICERIQUEZA)
salud$C1P77=as.factor(salud$C1P77)
salud$C1P9=as.numeric(salud$C1P9)
salud$C1P23=as.numeric(salud$C1P23)
salud$tiempo.llegada=as.numeric(salud$tiempo.llegada)
salud$tiempo.espera=as.numeric(salud$tiempo.espera)

# Análisis de variables continuas
salud_continuas= salud %>% dplyr::select(c(C1P9,C1P23,tiempo.llegada,tiempo.espera))
summary(salud)

jpeg("scatterplot.jpg")
ggpairs(salud_continuas)
dev.off()

# Análisis de variables categóricas
par(las=3)
plot(salud$DEPARTAMENTO)
plot(salud$C1INDICERIQUEZA)
plot(salud$INSTITUCION)
plot(salud$C1P80)
summary(salud)

# Selección de variables
salud = na.omit(salud)
salud_m=(matrix(unlist(salud),ncol=13,byrow=T))

# Selección stepwise
lm_salud= lm(tiempo.espera ~ .,data=salud)
st_salud= step(lm_salud)

# Selección LASSO
ls_salud= glmnet(salud_m[,-13],salud_m[,13])
plot(ls_salud)
coef(b,s="lambda.min")

# Modelo según stepwise 
lmf_salud =lm(tiempo.espera ~ DEPARTAMENTO+C1P23+C1P5+INSTITUCION+C1TURNO+tiempo.llegada,data=salud)
summary(lmf_salud)

# Análisis de residuos
jpeg("lm_qqplot.jpg")
qqPlot(lmf_salud,id=list(n=3))
dev.off()

jpeg("lm_residuales.jpg")
residualPlots(lmf_salud,type="rstudent")
dev.off()

jpeg("lm_outliers.jpg")
influenceIndexPlot(lmf_salud)
dev.off()

jpeg("lm_varianza.jpg")
spreadLevelPlot(lmf_salud)
dev.off()

lmf_1_salud =lm(tiempo.espera ~ DEPARTAMENTO+C1P23+C1P5+INSTITUCION+C1TURNO+log(tiempo.llegada),data=salud)
stargazer(lmf_1_salud)
summary(lmf_1_salud)
jpeg("lm_1_residuales.jpg")
residualPlots(lmf_1_salud,type="rstudent")
dev.off()

# Añadir nueva variable
salud$tl12 =((salud$tiempo.llegada-12)*(salud$tiempo.llegada>12))

# Modelo final
lmf_2_salud =lm(tiempo.espera ~ DEPARTAMENTO+C1P23+C1P5+INSTITUCION+C1TURNO+log(tiempo.llegada)+tl12,data=salud)


jpeg("mf_qqplot.jpg")
qqPlot(lmf_2_salud,id=list(n=3))
dev.off()

jpeg("mf_residuales.jpg")
residualPlots(lmf_2_salud,type="rstudent")
dev.off()

jpeg("mf_outliers.jpg")
influenceIndexPlot(lmf_2_salud, vars=c("Cook","hat"))
dev.off()

jpeg("mf_varianza.jpg")
spreadLevelPlot(lmf_3_salud)
dev.off()

