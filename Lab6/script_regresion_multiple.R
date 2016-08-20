
#MODELOS LINEALES 
#Tipos de relaciones entre variables
# Modelo: y=ax+b+eps

par(mfrow = c(2, 3)) 


#parámetros (inventados por mi)
a=2
b=3

desv=0.2 #desviación estándar

eps=rnorm(100,sd=desv) # epsilon (valor aleatorio)
x=runif(100)
y=a+b*x+eps

# Grafico de dispersion 1
plot(x,y, main="Relación lineal perfecta", sub="pendiente positiva")
(cor(x,y))
# Grafico de dispersion 2
b=-3 # con otro parámetro
y=a+b*x+eps
plot(x,y, main="Relación lineal perfecta",sub="pendiente negativa")
(cor(x,y))

# Grafico de dispersion 3
b=3 # con otro parámetro
desv=1.2 #desviación estándar
eps=rnorm(100,sd=desv) # epsilon (valor aleatorio)

y=a+b*x+eps
plot(x,y, main="Relación lineal",sub="Mucho ruido")
(cor(x,y))

# Grafico de dispersion 4 (heterocedasticidad)
#en este caso las varianzas no son constantes


eps=c(rnorm(50,sd=0.2),rnorm(50,sd=0.8)) # epsilon (valor aleatorio)


y=a+b*sort(x)+eps
plot(sort(x),y, main="Heterocedasticidad",sub="varianzas no son constantes",xlab="x")

(cor(x,y))

## Grafico de dispersion 5 (relación no lineal)

y=a+b*log(x)+eps
plot(x,y, main="Relación no lineal")
(cor(x,y))



##### Ajuste de modelos lineales para los datos de ozono

datos=read.table("ozono.txt",header=TRUE)
datos
attach(datos) 
names(datos) 

#diagrama de dispersi??n
pairs(datos)

#correlaci??n
cor(datos)

#modelos lineales simples:

#modelo 1:
m1=lm(Ozono ~Temp) 
names(m1)
m1


#modelo 2:
m2=lm(Ozono ~ Rad.S)
m2

#modelo 3:
m3=lm(Ozono ~ Viento) 
m3

#gr??ficos (para observar como se ajustaron los modelos)

par(mfrow = c(1, 3))

plot(Temp, fitted.values(m1), sub="modelo 1",type="l")
points(Temp,Ozono)

plot(Rad.S,fitted.values(m2), sub="modelo 2",type="l")
points(Rad.S,Ozono)

plot(Viento,fitted.values(m3),sub="modelo 3",type="l")
points(Viento,Ozono)


#PRUEBA DE HIP??TESIS SOBRE LOS PAR??METROS y COEF. DE DETERMINACI??N

summary(m1)
summary(m2)
summary(m3)

#CRITICA DEL MODELO

#Chequeo de la independencia de los residuos y var de los residuos constante.

#independencia respecto a los valores ajustados.
plot(fitted.values(m1),rstandard(m1),xlab="Valores ajustados",ylab="Residuos estandarizados")
cor(fitted.values(m1),rstandard(m1)) #esto confirma la independencia numericamente
#independencia de los residuos respecto a Temp
plot(Temp,rstandard(m1), xlab= "Temperatura", ylab="Residuos estandarizados")
cor(Temp, rstandard(m1))

names(m1)
cor(m1$residuals,m1$fitted.values)
cor(rstandard(m1),fitted.values(m1))
plot(m1)
mean(m1$residuals)
mean(rstandard(m1))
#chequeo de la normalidad de los residuos
qqnorm(rstandard(m1))
qqline(rstandard(m1))

#media de los residuos igual a cero
mean(rstandard(m1))
boxplot(rstandard(m1),main="Residuales")


#modelo 4:
m4=lm(Ozono ~ Temp + Rad.S + Viento) 
summary(m4)

#Modelo 5:
m5=lm(Ozono ~ Temp + Rad.S +Viento -1) 
summary(m5)
plot(m5)

#Modelo 6: 
m6=lm(Ozono ~ Temp +Viento -1)
summary(m6)

#Modelo 7: 
m7=lm(Ozono ~ Temp -1)
summary(m7)
plot(m7)

#PREDICCIÓN 

#Procedamos ahora a graficar las bandas de intervalos de confianza y de predicci??n, para ello se debe escribir en R:

new=data.frame(Temp=seq(60, 100, 5))# valores con los que se van a predecir la concentraci??n de Ozono

Temp1=predict(m7,new,interval="prediction")# Intervalo de prediccion
Temp2=predict(m7,new,interval='confidence')# Intervalo de confianza del 95%
matplot(new$Temp,cbind(Temp1, Temp2[,-1]), lty=c(1,2,2,3,3), type="l", ylab="predicted Ozono")


#Los datos representan el ingreso per capita de 20 pa??ses de Europa para el a??o 1960. 
#Se presenta tambi??n el porcentaje de la fuerza laboral empleada en 
#agricultura, industria y servicios de cada pa??a

#El nombre de las variables es:
  # PCINC = Per capita income , 1960 ($) 
  #AGR = Percent of labor force in agriculture, 1960
  #IND = Percent of labor force in industry, 1960
  #SER = Percent of labor force in service occupations, 1960

# Leemos los datos ####
datos=read.table("ingreso_EUR.txt",header=T)
attach(datos)

#Hacemos los diagramas de dispersi??n

plot(AGR,PCINC,xlab="Porcentaje de la fuerza laboral dedicado a Agrigultura", ylab="Ingreso per capita ($)")
title("Relaci??n entre AGR e PCINC")
a=cor(AGR,PCINC)
text(70,1500,labels=as.character(a),cex=1,col="red")

plot(IND,PCINC,xlab="Porcentaje de la fuerza laboral dedicado a la Industria", ylab="Ingreso per capita ($)")
title("Relaci??n entre IND e PCINC")
a=cor(IND,PCINC)
text(20,1500,labels=as.character(a),cex=1,col="red")

plot(SER,PCINC,xlab="Porcentaje de la fuerza laboral dedicado a los Servicios", ylab="Ingreso per capita ($)")
title("Relaci??n entre SER e PCINC")
a=cor(SER,PCINC)
text(17,1500,labels=as.character(a),cex=1,col="red")

#Hacemos el ajuste del modelo por m??nimos cuadrados
mods=lm(PCINC~AGR+IND+SER)
summary(mods)

mod2=lm(PCINC~IND+SER)
summary(mod2)

mod3=lm(PCINC~ IND)
summary(mod3)

mod4=lm(PCINC~IND -1)
summary(mod4)


#Trazamos la recta de regresi??n sobre el diagrama de puntos
plot(IND,PCINC)
abline(mod4)
beta1=mod4$coeff[1]
(beta0=mod4$coeff[2])
text(20,1000,bquote(hat(y)==.(beta)+.(beta1)*x))


##### An??lisis de los residuos ####

#Para chequear la normalidad 
par(mfrow=c(1,3))
hist(rstandard(mod4))
boxplot(rstandard(mod4),main="Diagrama de cajas de los residuos")
qqnorm(rstandard(mod4))
qqline(rstandard(mod4))

# Para chequear la homocedasticidad
par(mfrow=c(1,2))
plot(rstandard(mod4))
abline(h=0,col=2)
plot(mod4$fitted,rstandard(mod4))
title("Residuos versus valores ajustados")
abline(h=0,col=2)

#Para chequear la independencia
par(mfrow=c(1,1))
plot(IND,rstandard(mod4),main="Residuos estandarizados versus variable independiente")
abline(h=0,col=2)

###### Inferencia ####
summary(IND)

# Calculamos el intervalo de confianza para la media y
(valores=data.frame(IND=c(22,40,50)))
predict(mod4,newdata=valores,interval=c("confidence"),level=.95)

#Calculo del intervalo de confianza para la predicci??n de y
predict(mod4,newdata=valores,interval=c("prediction"),level=.95)

# Graficamos las bandas de confianza y de predicci??n
porc.ind=data.frame(IND=seq(0,100,0.5))
limc=predict(mod4,newdata=porc.ind,interval=c("confidence"),level=.95)
limp=predict(mod4,newdata=porc.ind,interval=c("prediction"),level=.95)
limites=cbind(porc.ind,limc[,2:3],limp[,2:3])
plot(limites[,1],limites[,2],xlab="Porcentaje de fuerza en la industria",ylab=" Ingreso per capita",ylim=c(0,2000),type="l",col=2)
title("Bandas de confianza y predicci??n")
points(limites[,1],limites[,3],type="l",col=2)
points(limites[,1],limites[,4],type="l",col=2)
points(limites[,1],limites[,4],type="l",col=4)
points(limites[,1],limites[,5],type="l",col=4)
points(limites[,1],limites[,4],type="l",col=4)
points(datos$IND,datos$PCINC)
abline(mod4)
bandas=expression("Intervalo de la media", "Intervalo de predicci??n")
legend(0,1500,bandas,lty=1,col=c(2,4))
 
