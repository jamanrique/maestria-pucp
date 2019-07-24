# Una posible aleatorización de los tratamientos dentro de los bloques sería:
library(planor)
Design <- data.frame(block=rep(1:4,rep(4,4)),treatment=rep(c("A","B","C","D"),4))
planor.randomize(~block/UNITS, data=Design)

# Cargando conjunto de datos
ensamble <- read.table("ensamble2.txt", header=T)
ensamble

# ANOVA
mod<-lm(Tiempo~Metodo+as.factor(Operador),ensamble)
anva<-anova(mod)
anva

# LSD

 mediat<-tapply(ensamble$Tiempo,ensamble$Metodo,mean)
 
 # valor absoluto de diferencia de medias
 
difAB <- abs(mediat[1]-mediat[2])
difAC <- abs(mediat[1]-mediat[3])
difAD <- abs(mediat[1]-mediat[4])
difBC <- abs(mediat[2]-mediat[3])
difBD <- abs(mediat[2]-mediat[4])
difCD <- abs(mediat[3]-mediat[4])

CME <- anva$`Mean Sq`[3]
t<- qt(0.975,anva$Df[3])
 
LSD <- t*sqrt((2*CME)/4)
vecdif <- c(difAB,difAC,difAD,difBC,difBD,difCD)
nombres <- c("difAB","difAC","difAD","difBC","difBD","difCD")

for(i in 1:6)
{
  if(vecdif[i]>LSD)
    print(paste(nombres[i],"Significativa"))
  else
    print(paste(nombres[i],"No significativa"))
}

  # Tukey

 library(multcomp)
 attach(ensamble)
 amod<-aov(Tiempo~Metodo+as.factor(Operador))
 compmet<-glht(amod,linfct=mcp(Metodo="Tukey"))
 summary(compmet)

  # Dunnet
 
 library(multcomp)
 attach(ensamble)
 amod<-aov(Tiempo~Metodo+as.factor(Operador))
 compmet_control <-glht(amod,linfct=mcp(Metodo="Dunnett"))
 summary(compmet_control)
 
 


