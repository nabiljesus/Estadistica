##Andres Navarro   11-10688
##Nabil J. Marquez 11-10683
##Laboratorio 3
##Estadistica Intensivo 2016

datos <- read.table("Notas13-14.txt", header=T, fill = T)
datos2 <- read.table("Notas15-16.txt", header=T, fill = T)

#Dibujamos los diagramas de caja
par(mfrow=c(1,2))
boxplot(datos,main="boxplot 13-14",col = "orange",ylab="Notas de los examenes")
boxplot(datos2,main="boxplot 15-16",col = "orange",ylim=range(0:70),ylab="Notas de los examenes")


##### 13-14
# Evaluamos si s6, s8 y s9 13-14 se distribuye normal
par(mfrow=c(2,3))
hist(datos$S6,main="Notas S6 13-14", col="orange",xlab = "Notas de los examenes",ylab = "Frecuencia")
hist(datos$S8,main="Notas S8 13-14", col="blue",xlab = "Notas de los examenes",ylab = "Frecuencia")
hist(datos$S9,main="Notas S9 13-14", col="green",xlab = "Notas de los examenes",ylab = "Frecuencia")
qqnorm(datos$S6,main="Normal Q-Q S6 13-14", col="orange")
qqline(datos$S6, col="orange")
qqnorm(datos$S8,main="Normal Q-Q S8 13-14", col="blue")
qqline(datos$S8, col="blue")
qqnorm(datos$S9,main="Normal Q-Q S9 13-14", col="green")
qqline(datos$S9, col="green")


# Evaluamos si S2, s8 y s9 15-16 se distribuye normal
par(mfrow=c(2,3))
hist(datos2$S2,main="Notas S2 15-16", col="orange",xlab = "Notas de los examenes",ylab = "Frecuencia")
hist(datos2$S8,main="Notas S8 15-16", col="blue",xlab = "Notas de los examenes",ylab = "Frecuencia")
hist(datos2$S9,main="Notas S9 15-16", col="green",xlab = "Notas de los examenes",ylab = "Frecuencia")
qqnorm(datos2$S2,main="Normal Q-Q S2 15-16", col="orange")
qqline(datos2$S2, col="orange")
qqnorm(datos2$S8,main="Normal Q-Q S8 15-16", col="blue")
qqline(datos2$S8, col="blue")
qqnorm(datos2$S9,main="Normal Q-Q S9 15-16", col="green")
qqline(datos2$S9, col="green")


# Intervalos de Confianza
# Funcion que calcula el IDC del 100(1-alfa)% para la media de una muestra x
intervalo.med = function(x,alfa){
  n = length(x)
  z = qnorm(alfa/2,lower.tail = F)
  limS = mean(x) + z*sqrt(var(x)/n) # L???mite superior del IDC
  limI = mean(x) - z*sqrt(var(x)/n) # L???mite inferior del IDC
  return (c(limI,limS,mean(x)))
}

## Para el 13-14 (a1)
# IDC del 97% para S6
IDCs6a1 <- intervalo.med(datos$S6,0.03)
# IDC del 97% para S8
IDCs8a1 <-intervalo.med(datos$S8[1:35],0.03)
# IDC del 97% para S9
IDCs9a1 <-intervalo.med(datos$S9[1:35],0.03)

## Para el 15-16 (a2)
# IDC del 97% para S6
IDCs2a2 <- intervalo.med(datos2$S2,0.03)
# IDC del 97% para S8
IDCs8a2 <-intervalo.med(datos2$S8[1:33],0.03)
# IDC del 97% para S9
IDCs9a2 <-intervalo.med(datos2$S9[1:32],0.03)
