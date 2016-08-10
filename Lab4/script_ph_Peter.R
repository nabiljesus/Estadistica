### Pruebas de hipótesis

## PROPORCIONES

## Un politólogo afirma que la fracción p_1 de republicanos es mayor que la 
## fracción p_2 de demócratas que están a favor de la pena de muerte. El 
## investigador tomó muestras aleatorias independientes de 200 republicanos y 
## 200 demócratas, y encontró que 46 republicanos y 34 demócratas apoyaban la 
## pena de muerte. ¿Hay evidencia estadística que apoye la opinión del investigador?
## utilice alpha = 0.05.

x = c(46,34)
n = c(200,200)

(y=prop.test(x,n,conf.level=0.95))

(p1=y$estimate[1])
(p2=y$estimate[2])

(Z1= (p1-p2)/sqrt((p1*(1-p1))/200 + (p2*(1-p2))/200))
(Z2= (p1-p2)/sqrt((0.5*(1-0.5))/200 + (0.5*(1-0.5))/200))

(pvalor1=2*pnorm(Z1,0,1,lower.tail=FALSE))
(pvalor2=2*pnorm(Z2,0,1,lower.tail=FALSE))

## MEDIAS

## Los operadores de vehículos que funcionan con gasolina se quejan del precio
## al que la expenden las gasolineras. En EE.UU. el impuesto federal sobre el
## galón de gasolina es constante (14.45 centavos), no obstante, los impuestos
## estatales y locales varían de 0.5 a 25.45 centavos en 18 áreas metropolitanas
## clave en el país. Dados los datos, ¿hay suficiente evidencia para afirmar que
## el impuesto promedio por galón de gasolina es menor de 45 centavos? Indique
## el valor p de la prueba.

datos=c(42.89,40.45,38.09,53.91,39.65,35.04,48.55,38.65,34.95,47.90,37.95,33.45,47.73,36.80,28.99,46.61,35.95,27.45)
datos
(n=length(datos)) 

(T = (mean(datos)-45)/(sd(datos)/sqrt(n)))

(pvalor = pt(T,17)) 

t.test(datos,alternative="less",mu=45,conf.level=0.99)


## VARIANZA

## Un instrumento de precisión garantiza una exactitud menor a 2 unidades. Una
## muestra de cuatro lecturas del instrumento del mismo objeto arrojó las 
## mediciones en "med". Determine el nivel de significancia alcanzado para la
## prueba de hipótesis nula en la que la desviación es 0.7 frente a la hipótesis
## alternativa de que la desviación es mayor a 0.7.

med = c(353,351,351,355)

(est= 3*var(med)/(0.7)^2)

(pvalor= pchisq(est,3,lower.tail=F))


## COMPARACIÓN DE VARIANZAS

## Los siguientes datos representan los tiempos de duración de las películas 
## que producen dos compañías cinematográficas. ¿Hay suficiente evidencia para 
## afirmar que la variabilidad de los tiempos son distintas a nivel 0.03?

c1 = c(103,94,110,87,98)
c2 = c(97,82,123,92,175,88,118)

var.test(c1,c2,conf.level=0.97)



