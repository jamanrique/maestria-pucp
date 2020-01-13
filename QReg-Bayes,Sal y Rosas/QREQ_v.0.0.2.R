library(gamlss)
library(gamlss.dist)
library(ggplot2)

rtweibull <- function(n,qt,alpha,t){
  # n = número de observaciones
  # qt > 0; cuantil
  # alpha = parámetro weibull
  # t = entre 0 y 1
  if (t > 1 | t < 0) {
    stop("parámetro t solo puede contener valores entre 0 y 1")
  }
  if (qt < 0){
    stop("parámetro qt solo puede ser superior a 0")
  }
  if (alpha < 0){
    stop("parámetro alpha solo puede ser superior a 0")
  }
  U = runif(n)
  qm = qt * (-((log(U+1))/(log(1-t))))^(1/alpha)
  return(qm)
}

rtweibull2 <- function(n,qt,sigma,t){
  # n = número de observaciones
  # qt > 0; cuantil
  # alpha = parámetro weibull
  # t = entre 0 y 1
  if (t > 1 | t < 0) {
    stop("parámetro t solo puede contener valores entre 0 y 1")
  }
  if (qt < 0){
    stop("parámetro qt solo puede ser superior a 0")
  }
  if (sigma < 0){
    stop("parámetro sigma solo puede ser superior a 0")
  }
  alpha = 1/(log(sigma+1))
  beta = qt/(-log(1-t))^(alpha)
  qm = rweibull(n = n,scale = beta,shape = alpha)
  return(qm)
}


library(BB)
library(numDeriv)

muestra <- rtweibull2(10000,2,2,0.1)
hist(muestra)

n <- length(muestra)

few <- function(x,param){
  qt <- param[1]
  sigma <- param[2]
  t <- param[3]
  ct <- -(log(1-t))
  a <- 1/(log(sigma+1))
  val <- (a) * ((ct/qt)^(a-1))*exp((-ct)*(x/qt)^(a))
  return(val)
}

lfew <- function(param){
  val = 0
  for(i in 1:n){
    val = val + log(few(muestra[i],param))
  }
  val = - val
  return(val)
}

inicial = c(2,2,0.5)

esti_mv = nlminb(inicial,lfew)
  ## Control: qt = 0.9, sigma = 0.75, t = 0.5
esti_mv$par


hist(rtweibull2(10000,esti_mv$par[1],esti_mv$par[2],esti_mv$par[3]))

dtweibull <- function(value,qt,alpha,t){
  if (t > 1 | t < 0) {
    stop("parámetro t solo puede contener valores entre 0 y 1")
  }
  if (qt < 0){
    stop("parámetro qt solo puede ser superior a 0")
  }
  if (alpha < 0){
    stop("parámetro alpha solo puede ser superior a 0")
  }
  ct=(-log(1-t))^(1/alpha)
  b= qt/ct
  a=alpha
  dt = dweibull(x = value,a,b)
  return(dt)
}

ptweibull <- function(value,qt,alpha,t){
  if (t > 1 | t < 0) {
    stop("parámetro t solo puede contener valores entre 0 y 1")
  }
  if (qt < 0){
    stop("parámetro qt solo puede ser superior a 0")
  }
  if (alpha < 0){
    stop("parámetro alpha solo puede ser superior a 0")
  }
  ct=(-log(1-t))^(1/alpha)
  b= qt/ct
  a= alpha
  pt = pweibull(x = value,a,b)
}

vtweibull <- function(qt,alpha,t){
  if (t > 1 | t < 0) {
    stop("parámetro t solo puede contener valores entre 0 y 1")
  }
  if (qt < 0){
    stop("parámetro qt solo puede ser superior a 0")
  }
  if (alpha < 0){
    stop("parámetro alpha solo puede ser superior a 0")
  }
  ct=(-log(1-t))^(1/alpha)
  var=((qt^2)/(ct)^(1/alpha))*(gamma(1+(2/alpha))-(gamma(1+(1/alpha)))^2)
  return(var)
}

mtweibull <- function(qt,alpha,t){
  if (t > 1 | t < 0) {
    stop("parámetro t solo puede contener valores entre 0 y 1")
  }
  if (qt < 0){
    stop("parámetro qt solo puede ser superior a 0")
  }
  if (alpha < 0){
    stop("parámetro alpha solo puede ser superior a 0")
  }
  ct=(-log(1-t))^(1/alpha)
  mt=((qt)/(ct^(1/alpha)))*gamma(1+1/alpha)
  return(mt)
}

mtweibull(1,2,0.5)

qtseq = seq(1,10,0.1)

e_x1=data.frame(a=mtweibull(qtseq,1,0.5))
e_x1=cbind(e_x1,data.frame(b=mtweibull(qtseq,2,0.5)))
e_x1=cbind(e_x1,data.frame(c=mtweibull(qtseq,3,0.5)))
e_x1=cbind(e_x1,data.frame(d=mtweibull(qtseq,4,0.5)))
e_x1=cbind(e_x1,qtseq)

## Valor esperado; eje horizontal es qt

ggplot(e_x1)+
  geom_line(aes(x=qtseq,y=a),colour="red",size=1)+ #alpha =1
  geom_line(aes(x=qtseq,y=b),colour="blue",size=1)+ #alpha =2
  geom_line(aes(x=qtseq,y=c),colour="green",size=1)+ #alpha =3
  geom_line(aes(x=qtseq,y=d),colour="black",size=1) # alpha=4

e_x2=data.frame(a=mtweibull(1,qtseq,0.5))
e_x2=cbind(e_x2,data.frame(b=mtweibull(2,qtseq,0.5)))
e_x2=cbind(e_x2,data.frame(c=mtweibull(3,qtseq,0.5)))
e_x2=cbind(e_x2,data.frame(d=mtweibull(4,qtseq,0.5)))
e_x2=cbind(e_x2,qtseq)

## Valor esperado; eje horizontal es alpha

ggplot(e_x2)+
  geom_line(aes(x=qtseq,y=a),colour="red",size=1)+ #qt =1
  geom_line(aes(x=qtseq,y=b),colour="blue",size=1)+ #qt =2
  geom_line(aes(x=qtseq,y=c),colour="green",size=1)+ #qt =3
  geom_line(aes(x=qtseq,y=d),colour="black",size=1) #qt= 4

v_x1=data.frame(a=vtweibull(qtseq,1,0.5))
v_x1=cbind(v_x1,data.frame(b=vtweibull(qtseq,2,0.5)))
v_x1=cbind(v_x1,data.frame(c=vtweibull(qtseq,3,0.5)))
v_x1=cbind(v_x1,data.frame(d=vtweibull(qtseq,4,0.5)))
v_x1=cbind(v_x1,qtseq)

## Varianza; eje horizontal es qt

ggplot(v_x1)+
  geom_line(aes(x=qtseq,y=a),colour="red",size=1)+ #alpha =1
  geom_line(aes(x=qtseq,y=b),colour="blue",size=1)+ #alpha =2
  geom_line(aes(x=qtseq,y=c),colour="green",size=1)+ #alpha =3
  geom_line(aes(x=qtseq,y=d),colour="black",size=1) # alpha=4

v_x2=data.frame(a=vtweibull(1,qtseq,0.5))
v_x2=cbind(v_x2,data.frame(b=vtweibull(2,qtseq,0.5)))
v_x2=cbind(v_x2,data.frame(c=vtweibull(3,qtseq,0.5)))
v_x2=cbind(v_x2,data.frame(d=vtweibull(4,qtseq,0.5)))
v_x2=cbind(v_x2,qtseq)

## Varianza; eje horizontal es alpha

ggplot(v_x2)+
  geom_line(aes(x=qtseq,y=a),colour="red",size=1)+ #qt =1
  geom_line(aes(x=qtseq,y=b),colour="blue",size=1)+ #qt =2
  geom_line(aes(x=qtseq,y=c),colour="green",size=1)+ #qt =3
  geom_line(aes(x=qtseq,y=d),colour="black",size=1) #qt= 4


qrv=data.frame(a=dtweibull(qtseq,qt=1,alpha=5,t=0.5))
qrv=cbind(qrv,data.frame(b=dtweibull(qtseq,qt=2,alpha=5,t=0.5)))
qrv=cbind(qrv,data.frame(c=dtweibull(qtseq,qt=3,alpha=5,t=0.5)))
qrv=cbind(qrv,data.frame(d=dtweibull(qtseq,qt=4,alpha=5,t=0.5)))
qrv=cbind(qrv,data.frame(e=dtweibull(qtseq,qt=5,alpha=5,t=0.5)))

#plot de densidad, alpha = 5, t=0.5

ggplot(qrv)+
  geom_line(aes(y=a,x=qtseq),colour="red")+ #qt =1
  geom_line(aes(y=b,x=qtseq),colour="blue")+ #qt = 2
  geom_line(aes(y=c,x=qtseq),colour="green")+ #qt = 3
  geom_line(aes(y=d,x=qtseq),colour="black")+# qt=4
  geom_line(aes(y=e,x=qtseq),colour="yellow") #qt=5

arv=data.frame(a=dtweibull(qtseq,qt=3,alpha=1,t=0.5))
arv=cbind(arv,data.frame(b=dtweibull(qtseq,qt=3,alpha=2,t=0.5)))
arv=cbind(arv,data.frame(c=dtweibull(qtseq,qt=3,alpha=3,t=0.5)))
arv=cbind(arv,data.frame(d=dtweibull(qtseq,qt=3,alpha=4,t=0.5)))
arv=cbind(arv,data.frame(e=dtweibull(qtseq,qt=3,alpha=5,t=0.5)))

#plot de densidad, qt = 3, t=0.5

ggplot(arv)+
  geom_line(aes(y=a,x=qtseq),colour="red")+ #alpha =1
  geom_line(aes(y=b,x=qtseq),colour="blue")+ #alpha = 2
  geom_line(aes(y=c,x=qtseq),colour="green")+ #alpha = 3
  geom_line(aes(y=d,x=qtseq),colour="black")+# alpha=4
  geom_line(aes(y=e,x=qtseq),colour="yellow") # alpha=5


