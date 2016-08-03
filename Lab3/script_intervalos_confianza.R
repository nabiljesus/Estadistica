### Intervalos de Confianza  #########

#contenido medio de plomo (miligramos por litro) de muestras de agua 
#recolectadas diariamente durante 22 días del sistema de agua de Boston

muestras=c(0.035, 0.060, 0.055, 0.035, 0.039, 0.038, 0.049, 0.073, 0.047, 0.031, 0.016, 0.015, 0.015, 0.022, 0.043, 0.030, 0.019,0.021, 0.036, 0.016, 0.010, 0.020)

#Hallar el promedio diario de las cantidades de plomo encontradas en el
#agua, y el intervalo de confianza del mismo.
length(muestras)
summary(muestras)
var(muestras)

## Evaluamos si los datos provienen de una dist. normal ###

par(mfrow=c(1,1))

hist(muestras)
boxplot(muestras)
qqnorm(muestras)
qqline(muestras)

qt(0.025,21)

## Escribimos una función que evalúe un vector de datos "x" y un
## intervalo de confianza alfa de dos colas

intervalo=function(x,alfa) {
n=length(x)
nu=n-1 #grados de libertad
cuantil= qt(1-alfa/2,df=nu)
LS=mean(x)+ cuantil*sqrt(var(x)/n)
LI=mean(x)- cuantil*sqrt(var(x)/n)
return(c(mean(x),LI,LS))}

intervalo(muestras,0.05)

a=qchisq(0.975,21)
b=qchisq(0.025,21)

(Linf=((length(muestras)-1)*var(muestras))/a)
(Lsup=((length(muestras)-1)*var(muestras))/b)

### Intervalo de confianza para la diferencia de medias ####

#Esperanza de Vida al Nacer (años) en Venezuela, 1993-2002. Fuente: I.N.E.

Hombres=c(68.95, 69.15, 69.35, 69.56, 69.75, 69.90, 70.03, 70.45,70.6, 70.8)
Mujeres=c(74.73, 74.93, 75.13, 75.34, 75.53, 75.70, 75.84, 76.23,76.4, 76.6)

par(mfrow=c(1,1))
boxplot(Hombres,Mujeres,names=c("Hombres", "Mujeres"),ylab="Esperanza de vida al nacer (años)")

## Intervalo de confianza para diferencia de medias de datos "x" y
## "y". Intervalo de tamaño alfa de dos colas. Muestras grandes.

#Comparaciones de las varianzas
n1=length(Hombres)
n2=length(Mujeres)

var_hombres=var(Hombres)
var_mujeres=var(Mujeres)
cuantil_inf=qf(0.975,n1-1,n2-1)
cuantil_sup=qf(0.025,n1-1,n2-1)
L_I=(var_hombres/var_mujeres)*(1/cuantil_inf)
L_I
L_S=(var_hombres/var_mujeres)*(1/cuantil_sup)
L_S
### Verificamos con la función de R
var.test(Hombres,Mujeres)

## Comando de R
t.test(Hombres,Mujeres,var.equal=TRUE)


x=seq(30,20000,20)
plot(qt(0.975,x),xlab="grados de libertad")
qnorm(0.975)


# A veces las encuestas proporcionan información importante sobre temas que 
# parecen ajenos a sus objetivos. El New York Times aplicó una encuesta para 
# determinar las preferencias de los votantes en las elecciones presidenciales
# de 1992. Se realizaron 2374 entrevistas por vía telefónica con ciudadanos 
# estadounidenses adultos. 1912 ciudadanos indicaron que contaban con registro
# de elector. Con los datos anteriores, genere un intervalo de confianza del 
# 99% para la proporción de ciudadanos con registro de elector.

prop.test(1912,2374,conf.level=0.95)
