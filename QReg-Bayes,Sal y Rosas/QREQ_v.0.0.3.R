## Función de distribución weibull ##

library(BB)
library(numDeriv)
library(car)

data("cars")
data("Angell")
pba <- Angell[,1:3]
### Creación de función de regresión ###
### Argumentos:
###   1 -> datos: objeto dataframe al que hacer la regresión
###   2 -> target: integer que indica en dónde está la columna en la que hacer la regresión

reg_wei <- function(datos, target, te) {
  
  ### Variables auxiliares
  ## covar: set de datos que contienen las covariables
  ## var: vector columna que contiene la variable respuesta
  ## expr: objeto que contendrá el conjunto de log(qt)
  ## d: variable auxiliar que contiene la cantidad de columnas de covar
  covar <- datos[-target]
  var <- datos[target]
  expr <- vector()
  d <- dim(covar)[2]
  n <- dim(covar)[1]
  
  ## betas: cantidad de betas a estimar
  betas <- paste("b",seq(d),sep="")
  b0 <- 0
  
  for (i in 1:d) {
    assign(x= betas[i],value = 0)
    paste("+",betas,"*covar[",1,"]",sep = "")}
  
  for(i in 1:d){
    expr <- append(expr,paste("+",betas[i],"*covar[",i,"]",sep=""))}
  
  expr <- paste(expr,collapse = ""); expr <- paste("b0",expr,sep=""); expr <- paste("exp(",expr,")",sep="")
  
  # fdist_wb <- function(x,param){
  #   b0 <- param[1]
  #   for (i in 1:d) {
  #     assign(x= betas[i],value = param[i+1])}
  #   qt <- vector()
  #   assign(qt,as.formula(expr))
  #   sigma <- param[d+1]
  #   t <- param[d+2]
  #   ct <- -(log(1-t))
  #   a <- 1/(log(sigma+1))
  #   val <- (a) * ((ct/qt)^(a-1))*exp((-ct)*(x/qt)^(a))
  #   return(val)}
  
  rm(list = c(paste("b",1:d,sep= ""),"b0"))
  
  ll=function(param,x,t){
    b0 <- param[1]
    
    for (i in 1:d) {
      assign(x= betas[i],value = param[i+1])}
    
    assign("placeholder",eval(parse(text = expr)))
    sum_ll <- 0
    pl <- length(param)
    for (j in 1:n) {
        assign("qt",placeholder[j,])
        sigma <- param[pl]
        ct <- -(log(1-t))
        a <- 1/(log(sigma+1))
        sum_ll <- sum_ll - sum(log(a)+log(ct)-a*log(qt)+(a-1)*log(x[j,])-ct*(x[j,]/qt)**a)  
        }
    return(sum_ll)
  }

  inicial <- rep(0.1,d+2)

  esti_mv = optim(par = inicial,fn = ll,x=var, t= te,lower = c(rep(-Inf,d+1),0.1),upper = c(rep(Inf,d+1),Inf),method = "L-BFGS-B")
  
  return(esti_mv)
}

