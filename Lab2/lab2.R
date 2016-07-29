##Andres Navarro   11-10688
##Nabil J. Marquez 11-10683
##Laboratorio 2
##Estadistica Intensivo 2016

### Metodo de Maxima Verosimilitud  #########

#Leyendo los datos 
datosColas = scan("colas.txt")
summary(datosColas)

# Estimar el error para lambda
#METODO DE MAXIMA VEROSIMILITUD MLE
#PARA EL CASO DE LA DIST. DE POISSON
library(stats4) ## Cargar la libreria stats4.
x=NULL
#FUNCION DE VEROSIMILITUD
ll=function(lambda) {
  n<-length(datosColas)
  x<-datosColas
  -(-lambda*n+sum(x)*log(lambda))
}
est1=mle(minuslog=ll, start=list(lambda=133))
summary(est1)

# Calcular la probabilidad de que x >= 440 es 3 horas utilizando lambda= 3*133=399
ppois(440, lambda=399, lower.tail=FALSE)

# Calcular el numero minimo de personas
qpois(0.9, 133)
