rm(list = ls())

#### Importación de data ####
library(readr)
library(readxl)
DATAEX <- read_excel("D:/DATAEX.xlsx", 
										 sheet = "DATAEX")
library(survey)
library(dplyr)
library(sampling)
library(sqldf)

#### Limpieza de data ####
DATAEX_m <- mutate(DATAEX, BIM_fix=
			 											case_when(DATAEX$BIM=="SI" ~ 1,
														 					DATAEX$BIM=="NO" ~ 0)
									 )

#### Creación del objeto survey ####
disenoM <- svydesign(ids=~CONG+NUM, 
										 strata = ~ESTRATO+NULL, 
										 data = DATAEX_m,
										 fpc = ~pop_estrato+no_sector,
										 nest=T)

#### Pregunta 1 - Proporción Lima Metropolitana ####
svyciprop(formula = ~BIM_fix,
					design = disenoM,
					method = c("mean"),
					level = 0.95,
					na.rm=T)

#### Pregunta 2 - Proporción Lima Top ####
svyby(svymean,
			by = ~ESTRATO,
			design = disenoM,
			formula = ~BIM_fix)

#### Pregunta 3 ####

DATAEX_marco <- read_excel("D:/DATAEX.xlsx", 
													 sheet = "Marco Muestral")

td_1 <- aggregate(DATAEX_marco$`Obras por sector`[DATAEX_marco$`Sector Urbano`=="LIMA TOP"],
									by=list(DATAEX_marco$CONG[DATAEX_marco$`Sector Urbano`=="LIMA TOP"]),
									sum)

pik <- inclusionprobabilities(td_1$x,
															4)
set.seed(2230)
pik_select <- UPpoisson(pik = pik)
pik_select
which(pik_select==1)
index <- which((DATAEX_m$CONG=="MIRAFLORESB"&DATAEX_m$ESTRATO=="LIMA TOP")|
							 	(DATAEX_m$CONG=="SAN BORJAB"&DATAEX_m$ESTRATO=="LIMA TOP")|
							 	(DATAEX_m$CONG=="SAN ISIDROA"&DATAEX_m$ESTRATO=="LIMA TOP")|
							 	(DATAEX_m$CONG=="SURCOA"&DATAEX_m$ESTRATO=="LIMA TOP")
							 )
DATAEX_p3 <- DATAEX_m[index,]

disenoM_limatop <- svydesign(ids=~CONG,data = DATAEX_p3,fpc=~no_sector)
svymean(~BIM_fix,disenoM_limatop)
