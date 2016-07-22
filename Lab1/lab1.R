#Andres Navarro
#Nabil J. Márquez
#Laboratorio 1
#Estadistica Intensivo 2016

datos = read.table("IndiceCrim.dat", header=T)
attach(datos)
keeps <- c("R", "W", "NW")
datos <- datos[ , keeps, drop = FALSE]