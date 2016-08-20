##Andres Navarro   11-10688
##Nabil J. Marquez 11-10683
##Estadistica Intensivo 2016 - Laboratorio 6
datos = read.csv(file="ventas.csv", header=T,sep=";")
variables = datos[31,2:7]
datos = datos[1:30,1:7]
attach(datos)
names(datos)
plot(datos)
# Matriz de Correlación
mcor= cor(datos)
print(mcor)

##### Modelo 1
# Modelo donde |ro| > 0.5
m1 = lm(JOBPER ~ AG + AM + HC + IN)
summary(m1)
# Gráficos para ver cómo se ajustó el modelo
layout(matrix(c(1,1,1,1,2,3,4,5), 2, 4, byrow = TRUE))
qqnorm(resid(m1), main = "Modelo inicial (JOBPER ~ AG + AM + HC + IN)")
qqline(resid(m1))
plot(AG, fitted.values(m1), sub="modelo 1",type="l")
points(AG,JOBPER)
plot(AM, fitted.values(m1), sub="modelo 1",type="l")
points(AM,JOBPER)
plot(HC, fitted.values(m1), sub="modelo 1",type="l")
points(HC,JOBPER)
plot(IN, fitted.values(m1), sub="modelo 1",type="l")
points(IN,JOBPER)

##### Modelo 2 
# Modelo 2 eliminando HC
m2 = lm(JOBPER ~ AG + AM + IN)
summary(m2)
# Gráficos para ver cómo se ajustó el modelo
layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))
qqnorm(resid(m2), main = "Modelo 2 (JOBPER ~ AG + AM + IN)")
qqline(resid(m2))
plot(AG, fitted.values(m2), sub="modelo 2",type="l")
points(AG,JOBPER)
plot(AM, fitted.values(m2), sub="modelo 2",type="l")
points(AM,JOBPER)
plot(IN, fitted.values(m2), sub="modelo 2",type="l")
points(IN,JOBPER)

##### Modelo 3 
# Modelo 3 eliminando AG
m3 = lm(JOBPER ~ AM + IN)
summary(m3)
# Gráficos para ver cómo se ajustó el modelo
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
qqnorm(resid(m3), main = "Modelo 3 (JOBPER ~ AM + IN)")
qqline(resid(m3))
plot(AM, fitted.values(m3), sub="modelo 3",type="l")
points(AM,JOBPER)
plot(IN, fitted.values(m3), sub="modelo 3",type="l")
points(IN,JOBPER)

###### Prediccion sobre los modelos
predi = data.frame(AG=variables["AG"],EN=variables["EN"],AM=variables["AM"],HC=variables["HC"],SI=variables["SI"],IN=variables["IN"])
p1 <- predict(m1, newdata = predi, interval = "prediction")
predf = data.frame(AM=variables["AM"],IN=variables["IN"])
p2 <- predict(m3, newdata = predf, interval = "prediction")
