rm(list=ls())
#### Justo Manrique - 20091107
#### Respuesta en comentarios

#### << Carga y procesamiento de los datos >>

setwd("C:/Users/jaman/Documents/GitHub/maestria-pucp/2019-1/estadística-computacional/final")
pba = read.csv("cancer.csv")

library(dplyr)

tot_hab = pba %>% group_by(region) %>% summarise(tot_hab = sum(habitantes))
tot_mte = pba %>% group_by(region) %>% summarise(tot_mte = sum(muertes))

sum_y = as.numeric(tot_mte[1,2])
sum_w = as.numeric(tot_mte[2,2])
sum_n = as.numeric(tot_hab[1,2])
sum_m = as.numeric(tot_hab[2,2])

set.seed(239429)

#### << 5.b. Algoritmo de Gibbs >>

a = b = c = d = 0.01

l = numeric()
g = numeric()

l[1] = 1
g[1] = 1

M = 10000

# Rpta: Implementación del algoritmo

for (i in 1:M) {
  l[i+1] = rgamma(1,sum_y+sum_w+a,g[i]*sum_n+sum_m+b)
  g[i+1] = rgamma(1,sum_y+c,l[i]*sum_n+d)
}

# Rpta: Gráfico de la cadena

ts.plot(g)
ts.plot(l)

# Se observa convergencia en ambos parámetros.

# Rpta: Gráfico de autocorrelación

acf(g)
acf(l)

# Se observa que la cadena es de orden 1.

# Rpta: Histograma

hist(g)
hist(l)

#### 5.c. Estimación en forma puntual y por intervalo

p_lambda = c(quantile(l,0.025),mean(l),quantile(l,0.975));p_lambda

# La tasa lambda tiene como media 2.22 y el intervalo de credibilidad es de [2.10,2.33]

p_gamma = c(quantile(g,0.025),mean(g),quantile(g,0.975));p_gamma

# La tasa gamma tiene como media 1.21 y el intervalo de credibilidad es de [1.21,1.38]

#### 5.d. Calcule la probabilidad que la tasa de muertes en los distritos cercanos al reactor sea mayor que la tasa de los distritos que se encuentran alejados

mean(l[order(l)]*g[order(g)] > g[order(g)])

# Rpta: Se observa que es casi seguro (probabilidad del 0.9999) que la tasa de muertes en los distritos cercanos al reactor es mayor a las que se encuentran alejados.

#### 5.e. Considere un nuevo distrito a ser analizado con una población de 20 mil habitantes que está cercano al reactor y otro nuevo distrito también con 20 mil habitantes alejado del reactor. Calcule la probabilidad que haya más muertes por cáncer en el distrito cercano al reactor. Interprete.

O = 10000
sim_y = rpois(O,20*l*g)
sim_w = rpois(O,20*g)

mean(sim_y[order(sim_y)]>sim_w[order(sim_w)])

# Rpta: Se observa que es un evento seguro (probabilidad de 1) que la tasa de muertes, para una población de 20 mil habitantes, será mayor si está cerca del reactor que lejos.

