##Andres Navarro   11-10688
##Nabil J. Marquez 11-10683
##Laboratorio 4
##Estadistica Intensivo 2016

#Leyendo los datos
e11 <- read.table("Notas13-14.txt", header=T, fill = T)
e12 <- read.table("Notas15-16.txt", header=T, fill = T)
e21 <- read.table("Notas213-14.txt", header=T, fill = T)
e22 <- read.table("Notas215-16.txt", header=T, fill = T)

#Longitudes de cada sección
lengthNNA = function(x){
  return (length(x[!is.na(x)]))
}
ne11 <- c(lengthNNA(e11$S6),lengthNNA(e11$S8),lengthNNA(e11$S9))
ne12 <- c(lengthNNA(e12$S2),lengthNNA(e12$S8),lengthNNA(e12$S9))
ne21 <- c(lengthNNA(e21$S6),lengthNNA(e21$S8),lengthNNA(e21$S9))
ne22 <- c(lengthNNA(e22$S2),lengthNNA(e22$S8),lengthNNA(e22$S9))

#Medias y Desviaciones por examen (para cada sección)
m11 <- c(mean(e11$S6[1:ne11[1]]),mean(e11$S8[1:ne11[2]]),mean(e11$S9[1:ne11[3]]))
m12 <- c(mean(e12$S2[1:ne12[1]]),mean(e12$S8[1:ne12[2]]),mean(e12$S9[1:ne12[3]]))
m21 <- c(mean(e21$S6[1:ne21[1]]),mean(e21$S8[1:ne21[2]]),mean(e21$S9[1:ne21[3]]))
m22 <- c(mean(e22$S2[1:ne22[1]]),mean(e22$S8[1:ne22[2]]),mean(e22$S9[1:ne22[3]]))

s11 <- c(sd(e11$S6[1:ne11[1]]),sd(e11$S8[1:ne11[2]]),sd(e11$S9[1:ne11[3]]))
s12 <- c(sd(e12$S2[1:ne12[1]]),sd(e12$S8[1:ne12[2]]),sd(e12$S9[1:ne12[3]]))
s21 <- c(sd(e21$S6[1:ne21[1]]),sd(e21$S8[1:ne21[2]]),sd(e21$S9[1:ne21[3]]))
s22 <- c(sd(e22$S2[1:ne22[1]]),sd(e22$S8[1:ne22[2]]),sd(e22$S9[1:ne22[3]]))

# Determinar si las medias poblacionales de las notas son iguales o no entre el primer y segundo examen,
# con significancia de 3%.
# Usando una prueba de hipÃ³tesis
# Ho: mE1 - mE2 = 0, Ha: mE1 != mE2
r16 <- t.test(e11$S6[1:ne11[1]],e21$S6[1:ne21[1]],mu=0,conf.level=0.97)
r18 <- t.test(e11$S8[1:ne11[2]],e21$S8[1:ne21[2]],mu=0,conf.level=0.97)
r19 <- t.test(e11$S9[1:ne11[3]],e21$S9[1:ne21[3]],mu=0,conf.level=0.97)

r22 <- t.test(e12$S2[1:ne12[1]],e22$S2[1:ne22[1]],mu=0,conf.level=0.97)
r28 <- t.test(e12$S8[1:ne12[2]],e22$S8[1:ne22[2]],mu=0,conf.level=0.97)
r29 <- t.test(e12$S9[1:ne12[3]],e22$S9[1:ne22[3]],mu=0,conf.level=0.97)

# Una manera alterna usando esta funcion: 
#r1za.z.p = function(m1,m2,s1,s2,n1,n2){
#r1#  alfa = 0.03
#  Zalfa = qnorm(alfa/2,lower.tail = F)
#  Z = (m1 - m2)/sqrt((s1^2/n1) + (s2^2/n2))
#  # Calculando el p-valor
#  p_valor = 2*pnorm(Z,lower.tail = F)
#  bool1 = Z > Zalfa
#  bool2 = Z < (-1*Zalfa)
#  return (c(Zalfa,Z,p_valor,bool1 || bool2))
#}
# r16 = za.z.p(m11[1],m21[1],s11[1],s21[1],ne11[1],ne21[1])
# r18 = za.z.p(m11[2],m21[2],s11[2],s21[2],ne11[2],ne21[2])
# r19 = za.z.p(m11[3],m21[3],s11[3],s21[3],ne11[3],ne21[3])
#No utilizadas debido a ciertas discordancias, por ejemplo para el 13-14
#Sec 6  2.170090 -2.267857  1.976662  1.000000       #Z está en Zona de rechazo pero P-valor > 0.03 (Rechaza y Acepta H0)
#Sec 8  2.1700904 1.2736507 0.2027872 0.0000000      #Z no está en la ZR pero P-valor < 0.03        (Acepta y Rechaza H0)
#Sec 9  2.1700904 1.0262804 0.3047594 0.0000000      #Z no está en la ZR y P-valor > 0.03           (Acepta Ho)