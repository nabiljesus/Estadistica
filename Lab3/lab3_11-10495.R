# Laboratorio 3 de Estad???stica
# Hecho por: Mar???a Victoria Jorge 11-10495

# Variables utilizadas: BPIV (Cantidad de preguntas buenas en f???sica 0-5), 
#                       BPV (Cantidad de preguntas buenas en qu???mica 0-5), 
#                       NT_EX (Nota del examen 0-90),
#                       MPIV (Cantidad de preguntas malas en f???sica 0-5) y
#                       MPV (Cantidad de preguntas malas en qu???mica 0-5)
datos <- read.table("Notas13-14.txt", header=T, fill = T)
datos2 <- read.table("Notas15-16.txt", header=T, fill = T)

#Dibujamos los diagramas de caja
boxplot(datos,main="Diagrama de caja de S6, S7, S8 13-14",col = "orange",ylab="Notas de los examenes")

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