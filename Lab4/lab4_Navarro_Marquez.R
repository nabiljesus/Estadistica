##Andres Navarro   11-10688
##Nabil J. Marquez 11-10683
##Laboratorio 4
##Estadistica Intensivo 2016

miranda = scan('mirmus.txt') # Guardamos en un vector las muestras del Edo. Miranda
zulia = scan('zulmus.txt')   # Guardamos en un vector las muestras del Edo. Zulia

nM = length(miranda)
nZ = length(zulia)

# Medias y desviación estándar muestrales para ambos estados
muM = mean(miranda)
sM = sd(miranda)

muZ = mean(zulia)
sZ = sd(zulia)

# Determinar si las medias poblacionales del número de hijos son iguales o no entre los estados Miranda y Zulia,
# con significancia de 3%.
# Usando una prueba de hipótesis
# Ho: muZ - muM = 0, Ha: muM != muZ

alfa = 0.03
Zalfa = qnorm(alfa/2,lower.tail = F)

Z = (muZ - muM)/sqrt((var(miranda)/nM) + (var(zulia)/nZ))

# Calculando el p-valor

p_valor = 2*pnorm(Z,lower.tail = F)


# Determinar si el número medio de hijos para una mujer en Miranda y Zulia es mayor a dos, con significancia de 7%.
# Usando una prueba de hipótesis
# Ho: mu = 2, Ha: mu > 2

alfa2 = 0.07 # Nivel de significancia
Z2alfa = qnorm(alfa2,lower.tail = F)

Z2Miranda = (muM - 2)/(sM/sqrt(nM))
Z2Zulia = (muZ - 2)/(sZ/sqrt(nZ))

# Calculando el p-valor

p_valor2Miranda = pnorm(Z2Miranda,0,1,lower.tail = F)