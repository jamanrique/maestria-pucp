setwd("~/Documents/maestria-pucp/2019-2/modelos-lineales-2/clase-4/")
P2 <- read.csv("P2.csv")
library(ggplot2)
library(car)

# Entendimiento de la base de datos
str(P2)
summary(P2)

# Respuesta 2.a)
pairs(P2,col=P2$Educacion)

# Respuesta 2.b)
reg <- lm(Salario ~ Inest.Emocional + Educacion, data=P2)
summary(reg)

# Respuesta 2.c)
# Normalidad
qqPlot(reg)
residualPlots(reg,~Inest.Emocional+Educacion,type="rstudent")
spreadLevelPlot(reg)
influenceIndexPlot(reg,vars=c("Cook","hat"),id=list(n=10))

# Respuesta 2.d)
newdata=data.frame(Inest.Emocional=6.9,Educacion=as.factor("Baja"))
predict(reg,newdata)

