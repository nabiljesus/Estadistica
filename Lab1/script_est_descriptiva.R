#Estadística
#Práctica 1: Estadística descriptiva

#### Estadística descriptiva con RStudio #######

## Introduciendo datos

edad = c(2, 30, 23, 33, 28, 45, 34, 55, 67, 28, 35, 35, 36, 98)
sexo = c("F", "F", "M", "M","F", "M", "M","F", "F","F", "F","M","F", "M")

#### Análisis descriptivo de una variable cuantitativa ####
summary(edad)
var(edad)
sd(edad)
#si se desean otros atributos
sort(edad)
length(edad)
#Gráficos más utilizados para una variable cuantitativa
par(mfrow=c(1, 2)) #para dibujar dos graficos en una misma p?gina: 1 fila y 2 columnas
hist(edad, main = "Histograma de edasddes", 
     xlab= "Edad", ylab = "Frecuencia", ylim= c(0, 10))

boxplot(edad, col= "orange", horizontal= F, xlab="Edad")

hist(edad,plot=F) #"Tabla de frecuencia"

dev.off() #para cerrar la ventana gráfica

#### An?lisis descriptivo de una variable cualitativa #####

s=factor(sexo)
class(s)
summary(s)
plot(s)

### Cruzando una variable cuantitativa con otra cualitativa #####

edad[sexo== "F"]
edad[edad>50]
edad[edad>30 & sexo=="M"]
sexo[edad<= 30 | edad >80]


boxplot(split(edad,sexo), ylab="Edad", col= c("pink","purple"))

boxplot(split(edad,sexo),plot=F)
boxplot.stats(edad)

#### Leyendo datos desde archivos externos ####

datos = read.table("datos_rest.txt", header=T)
attach(datos)
names(datos)
s=factor(sexo)

par(mfrow=c(2, 2))
plot(s, main="Genero del cliente")
plot(plato, main= "Tipo de plato")
plot(postre, main= "Tipo de postre")
plot(llegada, main= "Llegada al restaurant")

par(mfrow=c(1,2))
hist(consumo, ylab="Frecuencia", xlab="Consumo (Bs.)", main="")
boxplot(consumo,horizontal=F,xlab="Consumo (Bs)")
par(mfrow=c(1,1))

#Preguntas de interes

boxplot(split( consumo, postre), main= "Monto del consumo y postre")
boxplot(split( consumo, plato), main = "Monto del consumo y plato")
boxplot(split( consumo, llegada), main= "Monto del consumo y llegada")
boxplot(split( consumo, sopa), main= "Monto del consumo y sopa")
boxplot(split( consumo, s), main= "Monto del consumo y sexo")

datos$plato
datos$plato[5]