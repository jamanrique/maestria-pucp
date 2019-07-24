    rm(list=ls())
    library(readxl)
    IMDb_Work <- read_excel("D:/Justo Andrés/Dropbox/Maestría en Estadística/2018 - 1/Técnicas de Muestreo/Tarea - Lista 2/Proyecto_Lista2/Inputs/IMDb_Work.xlsx", col_types = c("text", "text", "numeric", "numeric", "blank"))
    IMDb_WorkingDB <- IMDb_Work
    IMDb_WorkingDB <- IMDb_WorkingDB[order(IMDb_WorkingDB$Estrato),]
    set.seed(9875)
    IMDb_Summary <- as.vector(table(IMDb_WorkingDB$Estrato))
    IMDb_PreSample <- sample(IMDb_Summary[1],5)
    IMDb_PreSample <- append(IMDb_PreSample,sample(IMDb_Summary[2],5))
    IMDb_PreSample <- append(IMDb_PreSample,sample(IMDb_Summary[3],5))
    IMDb_PreSample <- append(IMDb_PreSample,sample(IMDb_Summary[4],5))
    
    library(xlsx)
    write.xlsx (IMDb_PreSample,"D:/Justo Andrés/Dropbox/Maestría en Estadística/2018 - 1/Técnicas de Muestreo/Tarea - Lista 2/Proyecto_Lista2/Outputs/PreSample.xlsx")
    write.xlsx (IMDb_WorkingDB,"D:/Justo Andrés/Dropbox/Maestría en Estadística/2018 - 1/Técnicas de Muestreo/Tarea - Lista 2/Proyecto_Lista2/Outputs/IMDb_WorkingDB.xlsx")
    
    library(readxl)
    sigmah <- read_excel("D:/Justo Andrés/Dropbox/Maestría en Estadística/2018 - 1/Técnicas de Muestreo/Tarea - Lista 2/Proyecto_Lista2/Outputs/3. Strata_Piloto.xlsx")
    sigmah <- as.vector(sigmah$SD)
    
    ah <- IMDb_Summary*sigmah/sum(IMDb_Summary*sigmah)
    d = dim(IMDb_WorkingDB)[1]*0.1/qnorm(0.975)
    n = sum(((IMDb_Summary*sigmah)^2)/ah)/(d^2 + sum(IMDb_Summary*sigmah^2))
    nh = ceiling(ah*n)
    ## Se suma +1 a todos los estratos para obtener variabilidad
    nh = nh+1
    
    
    library(sampling)
    m <- strata(IMDb_WorkingDB,c("Estrato"),size = nh,method = "srswor")
    mdata <- getdata(IMDb_WorkingDB,m)
    
    write.xlsx(mdata,"D:/Justo Andrés/Dropbox/Maestría en Estadística/2018 - 1/Técnicas de Muestreo/Tarea - Lista 2/Proyecto_Lista2/Outputs/FinalSample.xlsx")
  
    FinalSample <- read_excel("D:/Justo Andrés/Dropbox/Maestría en Estadística/2018 - 1/Técnicas de Muestreo/Tarea - Lista 2/Proyecto_Lista2/Outputs/4. FinalSample.xlsx")
    mn_FS <- mean(FinalSample$SD)
    FinalSample_1 <- FinalSample[FinalSample$Estrato=="1",]
    V_Y1 <- (dim(IMDb_WorkingDB[IMDb_WorkingDB$Estrato=="1",])[1]/dim(IMDb_WorkingDB)[1])^2*(1-dim(FinalSample[FinalSample$Estrato=="1",])[1]/dim(IMDb_WorkingDB[IMDb_WorkingDB$Estrato=="1",])[1])*(var(FinalSample_1$SD)/(dim(FinalSample[FinalSample$Estrato=="1",])[1]))
    
    FinalSample_2 <- FinalSample[FinalSample$Estrato=="2",]
    V_Y2 <- (dim(IMDb_WorkingDB[IMDb_WorkingDB$Estrato=="2",])[1]/dim(IMDb_WorkingDB)[1])^2*(1-dim(FinalSample[FinalSample$Estrato=="2",])[1]/dim(IMDb_WorkingDB[IMDb_WorkingDB$Estrato=="2",])[1])*(var(FinalSample_2$SD)/(dim(FinalSample[FinalSample$Estrato=="2",])[1]))
    
    FinalSample_3 <- FinalSample[FinalSample$Estrato=="3",]
    V_Y3 <- (dim(IMDb_WorkingDB[IMDb_WorkingDB$Estrato=="3",])[1]/dim(IMDb_WorkingDB)[1])^2*(1-dim(FinalSample[FinalSample$Estrato=="3",])[1]/dim(IMDb_WorkingDB[IMDb_WorkingDB$Estrato=="3",])[1])*(var(FinalSample_3$SD)/(dim(FinalSample[FinalSample$Estrato=="3",])[1]))
    
    FinalSample_4 <- FinalSample[FinalSample$Estrato=="4",]
    V_Y4 <- (dim(IMDb_WorkingDB[IMDb_WorkingDB$Estrato=="4",])[1]/dim(IMDb_WorkingDB)[1])^2*(1-dim(FinalSample[FinalSample$Estrato=="4",])[1]/dim(IMDb_WorkingDB[IMDb_WorkingDB$Estrato=="4",])[1])*(var(FinalSample_4$SD)/(dim(FinalSample[FinalSample$Estrato=="4",])[1]))
    
    sd_est <- (V_Y1+V_Y2+V_Y3+V_Y4)^(0.5)
    
    z <- qnorm(1-0.05/2)
    
    IC <- sd_est*z
    