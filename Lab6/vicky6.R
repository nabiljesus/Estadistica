# Laboratorio 6 de Estadística
# Hecho por: María Victoria Jorge 11-10495
#
# Ejercicio 9. Archivo 'CPU.dat'
datos = read.table("CPU.dat", header=T)
attach(datos)
names(datos)
rendRelativo = datos[-8] # Variables para el modelo del rendimiento relativo
rendEsperado = datos[-7] # Variables para el modelo del rendimiento esperado relativo
# Diagrama de dispersión
pairs(rendRelativo)
pairs(rendEsperado)
# Matriz de correlación
cor(rendRelativo)
cor(rendEsperado)
# Modelo donde |ro| > 0.5
m1RR = lm(RELRENDIM ~ MINMEM + MAXMEM + CACHE + MINCANAL + MAXCANAL)
summary(m1RR)
# Gráficos para ver cómo se ajustó el modelo
par(mfrow = c(1, 2))
plot(MINMEM, fitted.values(m1RR), sub="modelo 1",type="l")
points(MINMEM,RELRENDIM)
plot(MAXMEM, fitted.values(m1RR), sub="modelo 1",type="l")
points(MAXMEM,RELRENDIM)
plot(CACHE, fitted.values(m1RR), sub="modelo 1",type="l")
points(CACHE,RELRENDIM)
par(mfrow = c(1, 2))
plot(MINCANAL, fitted.values(m1RR), sub="modelo 1",type="l")
points(MINCANAL,RELRENDIM)
plot(MAXCANAL, fitted.values(m1RR), sub="modelo 1",type="l")
points(MAXCANAL,RELRENDIM)
# Crítica de m1RR
#independencia respecto a los valores ajustados.
plot(fitted.values(m1RR),rstandard(m1RR),xlab="Valores ajustados",ylab="Residuos estandarizados")
cor(fitted.values(m1RR),rstandard(m1RR)) #esto confirma la independencia numericamente
#chequeo de la normalidad de los residuos
qqnorm(rstandard(m1RR))
qqline(rstandard(m1RR))
#media de los residuos igual a cero
mean(rstandard(m1RR))
boxplot(rstandard(m1RR),main="Residuales")
par(mfrow = c(1, 2))
#independencia de los residuos respecto a MINMEM
plot(MINMEM,rstandard(m1RR), xlab= "Memoria mínima", ylab="Residuos estandarizados")
cor(MINMEM, rstandard(m1RR))
#independencia de los residuos respecto a MAXMEM
plot(MAXMEM,rstandard(m1RR), xlab= "Memoria máxima", ylab="Residuos estandarizados")
cor(MAXMEM, rstandard(m1RR))
par(mfrow = c(1, 2))
#independencia de los residuos respecto a MINCANAL
plot(MINCANAL,rstandard(m1RR), xlab= "Mínimo canales", ylab="Residuos estandarizados")
cor(MINCANAL, rstandard(m1RR))
#independencia de los residuos respecto a MAXCANAL
plot(MAXCANAL,rstandard(m1RR), xlab= "Máximo canales", ylab="Residuos estandarizados")
cor(MAXCANAL, rstandard(m1RR))
#independencia de los residuos respecto a CACHE
plot(CACHE,rstandard(m1RR), xlab= "Caché", ylab="Residuos estandarizados")
cor(CACHE, rstandard(m1RR))
# Modelo donde |ro| > 0.5
m1RER = lm(ESTRENDIM ~ MINMEM + MAXMEM + CACHE + MINCANAL + MAXCANAL)
summary(m1RER)
# Crítica de m1RER
# Gráficos para ver cómo se ajustó el modelo
par(mfrow = c(1, 2))
plot(MINMEM, fitted.values(m1RER), sub="modelo 1",type="l")
points(MINMEM,RELRENDIM)
plot(MAXMEM, fitted.values(m1RER), sub="modelo 1",type="l")
points(MAXMEM,RELRENDIM)
plot(CACHE, fitted.values(m1RER), sub="modelo 1",type="l")
points(CACHE,RELRENDIM)
par(mfrow = c(1, 2))
plot(MINCANAL, fitted.values(m1RER), sub="modelo 1",type="l")
points(MINCANAL,RELRENDIM)
plot(MAXCANAL, fitted.values(m1RER), sub="modelo 1",type="l")
points(MAXCANAL,RELRENDIM)
#independencia respecto a los valores ajustados.
plot(fitted.values(m1RER),rstandard(m1RER),xlab="Valores ajustados",ylab="Residuos estandarizados")
cor(fitted.values(m1RER),rstandard(m1RER)) #esto confirma la independencia numericamente
#chequeo de la normalidad de los residuos
qqnorm(rstandard(m1RER))
qqline(rstandard(m1RER))
#media de los residuos igual a cero
mean(rstandard(m1RER))
boxplot(rstandard(m1RER),main="Residuales")
par(mfrow = c(1, 2))
#independencia de los residuos respecto a MINMEM
plot(MINMEM,rstandard(m1RER), xlab= "Memoria mínima", ylab="Residuos estandarizados")
cor(MINMEM, rstandard(m1RER))
#independencia de los residuos respecto a MAXMEM
plot(MAXMEM,rstandard(m1RER), xlab= "Memoria máxima", ylab="Residuos estandarizados")
cor(MAXMEM, rstandard(m1RER))
par(mfrow = c(1, 2))
#independencia de los residuos respecto a MINCANAL
plot(MINCANAL,rstandard(m1RER), xlab= "Mínimo canales", ylab="Residuos estandarizados")
cor(MINCANAL, rstandard(m1RER))
#independencia de los residuos respecto a MAXCANAL
plot(MAXCANAL,rstandard(m1RER), xlab= "Máximo canales", ylab="Residuos estandarizados")
cor(MAXCANAL, rstandard(m1RER))
#independencia de los residuos respecto a CACHE
plot(CACHE,rstandard(m1RER), xlab= "Caché", ylab="Residuos estandarizados")
cor(CACHE, rstandard(m1RER))
# Modelos para el rendimiento relativo paso a paso
m2RR = lm(RELRENDIM ~ CICLOTIME + MINMEM + MAXMEM + CACHE + MINCANAL + MAXCANAL)
summary(m2RR)
m3RR = lm(RELRENDIM ~ CICLOTIME + MINMEM + MAXMEM + CACHE + MAXCANAL)
summary(m3RR)
#Crítica para el modelo m3RR
# Gráficos para ver cómo se ajustó el modelo
par(mfrow = c(1, 2))
plot(MINMEM, fitted.values(m3RR), sub="modelo 3",type="l")
points(MINMEM,RELRENDIM)
plot(MAXMEM, fitted.values(m3RR), sub="modelo 3",type="l")
points(MAXMEM,RELRENDIM)
plot(CACHE, fitted.values(m3RR), sub="modelo 3",type="l")
points(CACHE,RELRENDIM)
par(mfrow = c(1, 2))
plot(CICLOTIME, fitted.values(m3RR), sub="modelo 3",type="l")
points(CICLOTIME,RELRENDIM)
plot(MAXCANAL, fitted.values(m3RR), sub="modelo 3",type="l")
points(MAXCANAL,RELRENDIM)
#independencia respecto a los valores ajustados.
plot(fitted.values(m3RR),rstandard(m3RR),xlab="Valores ajustados",ylab="Residuos estandarizados")
cor(fitted.values(m3RR),rstandard(m3RR)) #esto confirma la independencia numericamente
#chequeo de la normalidad de los residuos
qqnorm(rstandard(m3RR))
qqline(rstandard(m3RR))
#media de los residuos igual a cero
mean(rstandard(m3RR))
boxplot(rstandard(m3RR),main="Residuales")
par(mfrow = c(1, 2))
#independencia de los residuos respecto a MINMEM
plot(MINMEM,rstandard(m3RR), xlab= "Memoria mínima", ylab="Residuos estandarizados")
cor(MINMEM, rstandard(m3RR))
#independencia de los residuos respecto a MAXMEM
plot(MAXMEM,rstandard(m3RR), xlab= "Memoria máxima", ylab="Residuos estandarizados")
cor(MAXMEM, rstandard(m3RR))
par(mfrow = c(1, 2))
#independencia de los residuos respecto a CICLOTIME
plot(CICLOTIME,rstandard(m3RR), xlab= "Ciclos", ylab="Residuos estandarizados")
cor(CICLOTIME, rstandard(m3RR))
#independencia de los residuos respecto a MAXCANAL
plot(MAXCANAL,rstandard(m3RR), xlab= "Máximo canales", ylab="Residuos estandarizados")
cor(MAXCANAL, rstandard(m3RR))
#independencia de los residuos respecto a CACHE
plot(CACHE,rstandard(m3RR), xlab= "Caché", ylab="Residuos estandarizados")
cor(CACHE, rstandard(m3RR))
# Modelos para el rendimiento esperado relativo paso a paso
modRER1 = lm(ESTRENDIM ~ CICLOTIME + MINMEM + MAXMEM + CACHE + MINCANAL + MAXCANAL)
summary(modRER1)
modRER2 = lm(ESTRENDIM ~ CICLOTIME + MINMEM + MAXMEM + CACHE + MAXCANAL)
summary(modRER2)
#Crítica para el modelo modRER2
# Gráficos para ver cómo se ajustó el modelo
par(mfrow = c(1, 2))
plot(MINMEM, fitted.values(modRER2), sub="modelo 3",type="l")
points(MINMEM,RELRENDIM)
plot(MAXMEM, fitted.values(modRER2), sub="modelo 3",type="l")
points(MAXMEM,RELRENDIM)
plot(CACHE, fitted.values(modRER2), sub="modelo 3",type="l")
points(CACHE,RELRENDIM)
par(mfrow = c(1, 2))
plot(CICLOTIME, fitted.values(modRER2), sub="modelo 3",type="l")
points(CICLOTIME,RELRENDIM)
plot(MAXCANAL, fitted.values(modRER2), sub="modelo 3",type="l")
points(MAXCANAL,RELRENDIM)
#independencia respecto a los valores ajustados.
plot(fitted.values(modRER2),rstandard(modRER2),xlab="Valores ajustados",ylab="Residuos estandarizados")
cor(fitted.values(modRER2),rstandard(modRER2)) #esto confirma la independencia numericamente
#chequeo de la normalidad de los residuos
qqnorm(rstandard(m3RR))
qqline(rstandard(m3RR))
#media de los residuos igual a cero
mean(rstandard(m3RR))
boxplot(rstandard(m3RR),main="Residuales")
par(mfrow = c(1, 2))
#independencia de los residuos respecto a MINMEM
plot(MINMEM,rstandard(modRER2), xlab= "Memoria mínima", ylab="Residuos estandarizados")
cor(MINMEM, rstandard(modRER2))
#independencia de los residuos respecto a MAXMEM
plot(MAXMEM,rstandard(modRER2), xlab= "Memoria máxima", ylab="Residuos estandarizados")
cor(MAXMEM, rstandard(modRER2))
par(mfrow = c(1, 2))
#independencia de los residuos respecto a CICLOTIME
plot(CICLOTIME,rstandard(modRER2), xlab= "Ciclos", ylab="Residuos estandarizados")
cor(CICLOTIME, rstandard(modRER2))
#independencia de los residuos respecto a MAXCANAL
plot(MAXCANAL,rstandard(modRER2), xlab= "Máximo canales", ylab="Residuos estandarizados")
cor(MAXCANAL, rstandard(modRER2))
#independencia de los residuos respecto a CACHE
plot(CACHE,rstandard(modRER2), xlab= "Caché", ylab="Residuos estandarizados")
cor(CACHE, rstandard(modRER2))