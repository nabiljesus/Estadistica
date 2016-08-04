# Laboratorio 3 de Estadística
# Hecho por: María Victoria Jorge 11-10495

# Variables utilizadas: BPIV (Cantidad de preguntas buenas en física 0-5), 
#                       BPV (Cantidad de preguntas buenas en química 0-5), 
#                       NT_EX (Nota del examen 0-90),
#                       MPIV (Cantidad de preguntas malas en física 0-5) y
#                       MPV (Cantidad de preguntas malas en química 0-5)

datos = read.table("CLargas.txt", header=T)
attach(datos)
names(datos)

# Evaluamos si BPIV se distribuye normal
par(mfrow=c(1,1))
hist(datos$BPIV,main="Histograma de BPIV",xlab = "N° preguntas buenas en física",ylab = "Frecuencia")
boxplot(datos$BPIV,main="Diagrama de caja de BPIV",col = "orange",ylab="N° preguntas buenas en física")
qqnorm(datos$BPIV)
qqline(datos$BPIV, col="orange")

# Evaluamos si MPIV se distribuye normal
par(mfrow=c(1,1))
hist(datos$MPIV,main="Histograma de MPIV",xlab = "N° preguntas malas en física",ylab = "Frecuencia")
boxplot(datos$MPIV,main="Diagrama de caja de MPIV",col = "orange",ylab="N° preguntas malas en física")
qqnorm(datos$MPIV)
qqline(datos$MPIV, col="orange")

# Evaluamos si BPV se distribuye normal
par(mfrow=c(1,1))
hist(datos$BPV,main="Histograma de BPV",xlab = "N° preguntas buenas en química",ylab = "Frecuencia")
boxplot(datos$BPV,main="Diagrama de caja de BPV",col = "orange",ylab="N° preguntas buenas en química")
qqnorm(datos$BPV)
qqline(datos$BPV, col="orange")

# Evaluamos si MPV se distribuye normal
par(mfrow=c(1,1))
hist(datos$MPV,main="Histograma de MPV",xlab = "N° preguntas malas en química",ylab = "Frecuencia")
boxplot(datos$MPV,main="Diagrama de caja de MPV",col = "orange",ylab="N° preguntas malas en química")
qqnorm(datos$MPV)
qqline(datos$MPV, col="orange")

# Evaluamos si NT_EX se distribuye normal
par(mfrow=c(1,1))
hist(datos$NT_EX ,main="Histograma de NT_EX",xlab = "Notas del examen",ylab = "Frecuencia")
boxplot(datos$NT_EX,main="Diagrama de caja de NT_EX",col = "orange",ylab="Notas del examen")
qqnorm(datos$NT_EX)
qqline(datos$NT_EX, col="orange")


# Intervalos de Confianza
# Función que calcula el IDC del 100(1-alfa)% para la media de una muestra x
intervalo.med = function(x,alfa){
  n = length(x)
  z = qnorm(alfa/2,lower.tail = F)
  limS = mean(x) + z*sqrt(var(x)/n) # Límite superior del IDC
  limI = mean(x) - z*sqrt(var(x)/n) # Límite inferior del IDC
  return (c(limI,limS))
}

# IDC del 95% para BPIV
intervalo.med(datos$BPIV,0.05)

# IDC del 95% para MPIV
intervalo.med(datos$MPIV,0.05)

# IDC del 95% para BPV
intervalo.med(datos$BPV,0.05)

# IDC del 95% para MPV
intervalo.med(datos$MPV,0.05)

# IDC del 95% para NT_EX
intervalo.med(datos$NT_EX,0.05)
