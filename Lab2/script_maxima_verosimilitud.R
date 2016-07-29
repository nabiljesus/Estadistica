### Método de Máxima Verosimilitud  #########

#DESCARGAR LA LIBRERÍA MASS


#########################################################################
#MÉTODO DE MÁXIMA VEROSIMILITUD MLE


#PARA EL CASO DE LA DIST. DE POISSON

(xpoi=rpois(1000,4.5))

library(stats4) ## Cargar la liberia stats4.

x=NULL
#FUNCIN DE VEROSIMILITUD
ll=function(lambda) {
  n<-length(xpoi)
  x<-xpoi
  -(-lambda*n+sum(x)*log(lambda))
}

est1=mle(minuslog=ll, start=list(lambda=2))
summary(est1)

#usando otra librería

library(MASS) ## loading package MASS
fitdistr(xpoi,"poisson")

###############################################################
#EJEMPLO PARA UNA DIST. NORMAL

x.norm<-rnorm(1000,4,2)
hist(x.norm)


#opción 1
library(stats4) ## loading package stats4
## -log-likelihood function

#la función de verosimilitud calculada en clases
ll<-function(mu,sigma2) {
  n<-1000
  x<-x.norm
  -(-(n/2)*log(sigma2)-(1/(2*sigma2))*(sum((x-mu)^2))-(n/2)*log(2*pi))
}

est<-mle(minuslog=ll, method = "Nelder-Mead",start=list(mu=2,sigma2=1))

summary(est)
 
? mle


#opción 2

#usando otra librería

library(MASS) ## loading package MASS
fitdistr(x.norm,"normal")
y=fitdistr(x.norm,"normal")
? fitdistr
(s2 = y$estimate[2]^2)