#Andres Navarro
#Nabil J. Márquez
#Laboratorio 1
#Estadistica Intensivo 2016

datos = read.table("IndiceCrim.dat", header=T)
attach(datos)
keeps <- c("R", "W", "NW")
datos <- datos[ , keeps, drop = FALSE]
attach(datos)
#Mean, Max, Min, 1st Qu, 3rd Qu., median
mys <- summary(datos)
#Calculando Desviación estandar
sds <- c(sd(R),sd(W),sd(NW))
sds<-paste("Sd.    :",sds)
out<- rbind(mys,sds)
#Calculando Varianza
vars <- c(var(R),var(W),var(NW))
vars <-paste("Var.  :",vars)
out<- rbind(out,vars)
#Calculando Rango Intercuartil
IQRS <- c(IQR(R),IQR(W),IQR(NW))
IQRS <-paste("IQR.  :",IQRS)
out<- rbind(out,IQRS)


rownames(out)<-rep("",each=dim(out)[1])
print(out)


histR <- hist(R, ylab="Nro. Delitos", xlab="NIIDEA", main="Delitos por 1M de Hab.")