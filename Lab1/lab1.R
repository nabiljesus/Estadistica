##Andres Navarro   11-10688
##Nabil J. Márquez 11-10683
##Laboratorio 1
##Estadistica Intensivo 2016

#Leyendo los datos
datos = read.table("IndiceCrim.dat", header=T)
#attach(datos)
#Como se encontró que es una mala practica, se decidio optar por:
R<-datos$R
W<-datos$W
NW<-datos$NW
#Liberando memoria
keeps <- c("R", "W", "NW")
datos <- datos[ , keeps, drop = FALSE]

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

#Imprimiendo resultados
rownames(out)<-rep("",each=dim(out)[1])
print("==========Análisis descriptivo==========")
print(out)

#Generando los graficos

#Grafico 1
jpeg('TodosGraficos.jpg')
par(mfrow=c(2, 3)) #6 graficos, 3 por fila
histR <- hist(R, ylab="Frecuencia", xlab="Ind. Criminalidad", main="Delitos/ M. Hab.",xlim=c(0,200), ylim= c(0, 14))
histW <- hist(W, ylab="Frecuencia", xlab="Ingreso familiar", main="Riqueza", ylim= c(0, 12))
histNW <- hist(NW, ylab="Frecuencia", xlab="Nro de Per. No blancas", main="No-Blanc. / 1000 hab.", ylim= c(0, 25))
boxplot(R, col= "Red", horizontal= F, xlab="Ind. Crim")
boxplot(W, col= "Yellow", horizontal= F, xlab="Riqueza")
boxplot(NW, col= "White", horizontal= F, xlab="Nro de Per. No blancas")
dev.off()

#Grafico 2
jpeg('boxplotComparar.jpg', width=800, height=600)
par(mfrow=c(1, 3)) #3 graficos, 3 por fila
boxplot(R, col= "Red", horizontal= F, xlab="Ind. Crim",ylim= c(0, 700))
boxplot(W, col= "Yellow", horizontal= F, xlab="Riqueza",ylim= c(0, 700))
boxplot(NW, col= "White", horizontal= F, xlab="Nro de Per. No blancas",ylim= c(0, 700))
dev.off()

#Imprimiento histogramas
print("==========Histograma R==========")
print(histR)
print("==========Histograma W==========")
print(histW)
print("==========Histograma NW==========")
print(histNW)

#detach(datos)
