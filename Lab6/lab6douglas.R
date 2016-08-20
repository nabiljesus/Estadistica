datos = read.csv("datosdoug.csv", header=T,sep=";")
attach(datos)
correlacion = cor(datos)
correlacion
plot(datos)

modelo_o = lm(JTS ~ING+EDU+DIS+SEM+EMP)
summary(modelo_o)
qqnorm(resid(modelo_o), main = "Modelo inicial (JTS ~ING+EDU+DIS+SEM+EMP)")
qqline(resid(modelo_o))

modelo = lm(JTS ~ING+EDU+SEM+EMP)
summary(modelo)
qqnorm(resid(modelo), main = "Modelo actualizado (JTS ~ING+EDU+SEM+EMP)")
qqline(resid(modelo))

modelo = lm(JTS ~ING+EDU+EMP)
summary(modelo)
qqnorm(resid(modelo), main = "Modelo actualizado (JTS ~ING+EDU+EMP)")
qqline(resid(modelo))

modelo = lm(JTS ~ING+EDU)
summary(modelo)
qqnorm(resid(modelo), main = "Modelo actualizado (JTS ~ING+EDU)")
qqline(resid(modelo))

predicciones = data.frame(ING=25,EDU=5)
predict(modelo, newdata = predicciones, interval = "prediction")

predicciones = data.frame(ING=25,EDU=5,DIS=7,SEM=26,EMP=3)
predict(modelo_o, newdata = predicciones, interval = "prediction")

