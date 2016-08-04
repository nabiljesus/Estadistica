# Laboratorio 3 de Estad√?stica
# Hecho por: Mar√?a Victoria Jorge 11-10495

# Variables utilizadas: BPIV (Cantidad de preguntas buenas en f√?sica 0-5), 
#                       BPV (Cantidad de preguntas buenas en qu√?mica 0-5), 
#                       NT_EX (Nota del examen 0-90),
#                       MPIV (Cantidad de preguntas malas en f√?sica 0-5) y
#                       MPV (Cantidad de preguntas malas en qu√?mica 0-5)
datos <- read.table("Notas13-14.txt", header=T, fill = T)
attach(datos)

#Dibujamos los diagramas de caja
boxplot(datos,main="Diagrama de caja de S6, S7, S8",col = "orange",ylab="Notas de los examenes")

# Evaluamos si s6 se distribuye normal
par(mfrow=c(1,1))
hist(datos$S6,main="Histograma de notas S6", col="orange",xlab = "Notas de los examenes",ylab = "Frecuencia")
qqnorm(datos$S6, col="orange")
qqline(datos$S6, col="orange")

# Evaluamos si S8 se distribuye normal
par(mfrow=c(1,1))
hist(datos$S8,main="Histograma de notas S8", col="blue",xlab = "Notas de los examenes",ylab = "Frecuencia")
qqnorm(datos$S8, col="blue")
qqline(datos$S8, col="blue")

# Evaluamos si S9 se distribuye normal
par(mfrow=c(1,1))
hist(datos$S9,main="Histograma de notas S9", col="green",xlab = "Notas de los examenes",ylab = "Frecuencia")
qqnorm(datos$S9, col="green")
qqline(datos$S9, col="green")
