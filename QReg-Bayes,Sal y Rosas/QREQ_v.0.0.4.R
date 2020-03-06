library(car)
library(kyotil)
data(cars);cars$li <- cars$dist-1;cars$si <- cars$dist+1; cars <- cars[-2]

## regresión cuantílica con intervalos
reg_ces_wei <- function(data,linf,lsup,te){
  data <- newsalud_enf
  linf <- 5
  lsup <- 4
  param <- c(rep(0.2,d+1),0.9)
  te <- 0.4
  
  liminf <- data[linf]
  limsup <- data[lsup]
  covar <- data[-c(linf,lsup)]
  d <- ncol(covar)
  n <- nrow(covar)
  
  ## betas: cantidad de betas a estimar
  betas <- paste("b",seq(d),sep="")
  b0 <- 0
  
  for (i in 1:d) {
    assign(x= betas[i],value = 0)
    paste("+",betas,"*covar[",1,"]",sep = "")}
  
  expr <- vector()
  
  for(i in 1:d){
    expr <- append(expr,paste("+",betas[i],"*covar[",i,"]",sep=""))}
  
  expr <- paste(expr,collapse = ""); expr <- paste("b0",expr,sep=""); expr <- paste("exp(",expr,")",sep="")
  rm(list = c(paste("b",1:d,sep= ""),"b0"))
  
  ll <- function(param, l_sup, l_inf, t){
    
    wei_acum <- function(ctau,qtau,alpha,y){1-exp(-ctau*((y/qtau)**alpha))}
    
    pl <- length(param)
    b0 <- param[1]
    for (i in 1:d) {
      assign(x= betas[i],value = param[i+1])}
    
    assign("placeholder",eval(parse(text = expr)))
    
    sum_ll <- 0
    
    
    for (j in 1:n) {
      assign("qt",placeholder[j,])
      sigma <- param[pl]
      ct <- -(log(1-t))
      a <- 1/(log(sigma+1))
      sum_ll <- sum_ll - logDiffExp(log(wei_acum(ct,qt,a,l_sup[j,])),log(wei_acum(ct,qt,a,l_inf[j,])))}
    return(sum_ll)
  }
  
    inicial <- c(rep(0.2,d+1),1)
    
    # esti_mv <- optim(par = inicial,fn = ll,l_sup=limsup, l_inf=liminf,t=te,lower = c(rep(-Inf,d+1),0.1),upper = c(rep(Inf,d+1),Inf),method = "L-BFGS-B")
    esti_mv <- nlminb(start = inicial,objective = ll,l_sup = limsup, l_inf=liminf,t=te,lower =c(rep(-Inf,d+1),0.1),upper = c(rep(Inf,d+1),Inf))
    
    return(esti_mv)
}

#### prueba ####

t_seq <- seq(0.01,0.99,0.01)
t_n <- length(t_seq)

append_final <- vector()
for (i in 1:t_n) {
  final <- reg_ces_wei(cars,2,3,t_seq[i])
  append_final <- rbind(append_final,final$par)
}

plot(t_seq,append_final[,2])

#### replicación del código de lancet ####

library(haven)

temp <- tempfile()
download.file("http://iinei.inei.gob.pe/iinei/srienaho/descarga/SPSS/447-Modulo552.zip",temp)
salud <- read_sav(unz(temp, "447-Modulo552/04_C2_CAPITULOS.sav"))
unlink(temp)
## Eliminating missing values
salud <- salud[salud$C2P28!=7,]

#############################
## DATA MANAGEMENT

myvarsalud <- c("C2P21","C2P27","C2P9","REGION","INSTITUCION","C2P1", "C2P4",
                "C2P7","C2P11","C2P13","C2P23","C2P24","C2P25","C2P26","C2P28")
newsalud <- salud[myvarsalud]
newsalud <- data.frame(newsalud)
## Disaggregating salary ordinal variable into its limits
newsalud$li <- NULL
newsalud$ls <- NULL
newsalud$li <- ifelse(newsalud$C2P28==1,850,
                      ifelse(newsalud$C2P28==2,1000,
                             ifelse(newsalud$C2P28==3,2001,
                                    ifelse(newsalud$C2P28==4,3001,
                                           ifelse(newsalud$C2P28==5,4001,5001)))))
newsalud$lf <- ifelse(newsalud$C2P28==1,999,
                      ifelse(newsalud$C2P28==2,2000,
                             ifelse(newsalud$C2P28==3,3000,
                                    ifelse(newsalud$C2P28==4,4000,
                                           ifelse(newsalud$C2P28==5,5000,Inf)))))
## Deleting "Other" in type of contract
newsalud <- newsalud[newsalud$C2P7!=6,]
## Deleting 8 NA's in dependents
newsalud <- newsalud[!is.na(newsalud$C2P9),]
## Converting to factor and removing unused tag
newsalud$C2P28 <- factor(newsalud$C2P28,
                         labels=names(attributes(newsalud$C2P28)$labels)[1:6])
newsalud$C2P7 <- factor(newsalud$C2P7,
                        labels=names(attributes(newsalud$C2P7)$labels)[1:5])
## Converting to factor the remaining categorical variables
for (j in 4:15) {
  if(j!=8 & j!=15){
    newsalud[,j] <- factor(newsalud[,j],
                           labels = names(attributes(newsalud[,j])$labels))
  }
}
## REGION
levels(newsalud$REGION)[levels(newsalud$REGION)=="Sierra"] <- "Nuevo"
levels(newsalud$REGION)[levels(newsalud$REGION)=="Selva"] <- "Sierra"
levels(newsalud$REGION)[levels(newsalud$REGION)=="Nuevo"] <- "Selva"
## C2P7
levels(newsalud$C2P7)[levels(newsalud$C2P7)=="Locaciòn de servicios (Honorarios profesionales)"
                      | levels(newsalud$C2P7)=="Contrato Administrativo de Servicios (CAS)"] <- "Plazo no fijo"
levels(newsalud$C2P7)[levels(newsalud$C2P7)=="Contrato a plazo fijo (sujeto a modalidad)"
                      | levels(newsalud$C2P7)=="Nombrado, permanente"
                      | levels(newsalud$C2P7)=="Plazo indeterminado o indefinido (D.S.728)"] <- "Plazo fijo"
## C2P23
levels(newsalud$C2P23)[levels(newsalud$C2P23)=="Permanente (Tiene trabajo durante todo el año de
manera continua)?"] <- "Permanente"
levels(newsalud$C2P23)[levels(newsalud$C2P23)=="Temporal o estacional (No permanente)?"] <-
  "Temporal"
## Re-ordering levels
newsalud$C2P4 <- relevel(newsalud$C2P4,ref="Mujer")
newsalud$C2P13 <- relevel(newsalud$C2P13,ref="No")
newsalud$C2P23 <- relevel(newsalud$C2P23,ref="Temporal")
newsalud$C2P24 <- relevel(newsalud$C2P24,ref="No")
newsalud$C2P25 <- relevel(newsalud$C2P25,ref="No")
newsalud$C2P26 <- relevel(newsalud$C2P26,ref="No")
## Generating separated physicians and nurses data
newsalud <- subset(newsalud,select=-c(C2P28))
newsalud_med <- newsalud[newsalud$C2P1 == "Médico",]
newsalud_med <- subset(newsalud_med,select=-C2P1)
newsalud_enf <- newsalud[newsalud$C2P1 != "Médico",]
newsalud_enf <- subset(newsalud_enf,select=-C2P1)
## Scaling
newsalud_med$li <- newsalud_med$li/1000;newsalud_med$lf <- newsalud_med$lf/1000
newsalud_enf$li <- newsalud_enf$li/1000;newsalud_enf$lf <- newsalud_enf$lf/1000

newsalud_enf <- newsalud_enf[c(1,2,3,length(newsalud_enf),length(newsalud_enf)-1)]
newsalud_enf <- subset(newsalud_enf,(newsalud_enf$lf == Inf)==FALSE)

reg_ces_wei(newsalud_enf,linf = 5,lsup = 4,te = 0.5)
