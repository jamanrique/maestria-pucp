qtweibull <- function(n,qt,alpha,t){
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

hist(qtweibull(10000,5,4,0.1))

# Evaluación: qt y t fijo -- t=0.5; Qt= 1; alpha de 1 a 10, en intervalos de 0.5

a <- seq(0.4,10,0.2)
n <- 10000
## Leyenda eje vertical = valor de alpha

plot(density(qtweibull(n,1,0.2,0.5)),xlab=0.2)
for (i in 1:length(a)) {
  
  d <- matrix(nrow = n,ncol = length(a))
  d[,i] <- qtweibull(n,1,a[i],0.5)
  plot(density(d[,i]),xlab=a[i])
}

# Evaluación: alpha y qt fijo -- Qt = 1; alpha = 1
a <- seq(0.1,1,0.05)
plot(density(qtweibull(n,1,1,0.5)),xlab=0.05)
for (i in 1:length(a)) {
  d <- matrix(nrow = n,ncol = length(a))
  d[,i] <- qtweibull(n,1,1,a[i])
  plot(density(d[,i]),xlab=a[i])
}

# Evaluación: alpha y t fijo -- alpha = 1; t = 0.5

a <- seq(0.4,10,0.2)
plot(density(qtweibull(n,0.5,1,0.5)),xlab=0.2)
for (i in 1:length(a)) {
  d <- matrix(nrow = n,ncol = length(a))
  d[,i] <- qtweibull(n,a[i],1,0.5)
  plot(density(d[,i]),xlab=a[i])
}
