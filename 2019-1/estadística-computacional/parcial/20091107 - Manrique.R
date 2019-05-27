## Definimos los parámetros ##
rm(list=ls())

set.seed(50000)
eps = 70
sig = 20
alpha = 2
beta = -2

## Definimos la función de interés ##

FSGN = function(x,eps, sig, alpha, beta){
  2 * dnorm(x=x,mean=eps, sd=sig) * pnorm(alpha*(x-eps)/sig+beta*(x-eps)^3/sig^3,mean = 0,sd = 1)
}

## Definimos el algoritmo ##

# Valor Inicial
M=10000
z=1
# Metropolis Hastings
for(h in 2:M){
  x = z[h-1]
  y = rnorm(1, mean = eps,sd = sig^2*2.4)
  u = runif(1)
  abc= min(1,(FSGN(y,eps,sig,alpha,beta)/FSGN(x,eps,sig,alpha,beta)))
  if(u<=abc){z[h]<-y} #acepta al candidato
  if(u>abc){z[h]<-x} #rechaza al candidato
}

par(mfrow=c(1,1))

hist(z[50:length(z)],prob=T)
lines(density(z))

ts.plot(z)

rm(list=ls())

data("faithful")

library(pracma)
log.like = function(theta){
  eps = theta[1]
  sig = theta[2]
  alpha = theta[3]
  beta = theta[4]
  ftwt = faithful$waiting
  -(-length(ftwt)/2 *log(pi*2*sig^2)+((-1/(2*sig^2))*sum((ftwt-eps)^2))+sum(log(1+erf((alpha*(ftwt-eps)/sig+beta*(ftwt-eps)^3/sig^3))/(sig*sqrt(2)))))
}

res.L = optim(c(70,20,0,0),log.like,method = "L-BFGS-B",hessian = T)
round(res.L$par,3)
res.L$hessian

fisher_info = solve(-res.L$hessian)
fisher_info

sigma = sqrt(diag(fisher_info))
sigma = sigma[4]

ftwt = faithful$waiting

up = res.L$par[4] +1.96*(sigma/sqrt(length(ftwt)))
low = res.L$par[4] -1.96*(sigma/sqrt(length(ftwt)))
ci = c(low, res.L$par[4], up)
ci
