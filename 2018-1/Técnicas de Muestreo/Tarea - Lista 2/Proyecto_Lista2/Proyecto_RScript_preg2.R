# Limpieza del ambiente en R para correr el flujo
rm(list=ls())

# Carga de librería y excel que contiene la tabla del ejercicio
library(readxl)
pba <- read_xlsx("D:/Justo Andrés/Dropbox/Maestría en Estadística/2018 - 1/Técnicas de Muestreo/Tarea - Lista 2/Proyecto_Lista2/Inputs/P2_db.xlsx")

# Identificación del número de la muestra por cada estrato (Obreros, Técnicos, Administradores)
z <- qnorm(1-0.05/2)
n_ob <- ceiling(z^2 * pba[1,1]*pba[2,1]/((z^2*pba[1,1])+(100/pba[2,1])^2*pba[2,1]))
n_tec <- ceiling(z^2 * pba[1,2]*pba[2,2]/((z^2*pba[1,2])+(100/pba[2,2])^2*pba[2,2]))
n_adm <- ceiling(z^2 * pba[1,3]*pba[2,3]/((z^2*pba[1,3])+(100/pba[2,3])^2*pba[2,3]))

# Identificación individual de los errores por cada estrato
e_ob <- (1-(n_ob/pba[2,1]))*(pba[2,1])^2*(pba[1,1]/n_ob)
e_tec <- (1-(n_tec/pba[2,2]))*(pba[2,2])^2*(pba[1,2]/n_tec)
e_adm <- (1-(n_adm/pba[2,3]))*(pba[2,3])^2*(pba[1,3]/n_adm)

# Identificación del error estándar global
sub_et <- (e_ob+e_tec+e_adm)^(0.5)
et <- z * sub_et
et