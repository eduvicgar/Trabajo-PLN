# Para obtener las funciones y el texto es importante establecer la carpeta
# "pruebas" como directorio de trabajo con setwd()
library(rvest)
library(spacyr)

# Obtenemos las funciones de la carpeta 
source("../fuentes/buscar_definicion_diccionario.R")
source("../fuentes/filtrar_enfermedades.R")

# Leemos el texto a analizar
texto <- readLines("../datos/texto 120.txt")

# Obtenemos el dataframe
resultado <- filtrar_enfermedades(texto)
resultado

# Opcionalmente se puede exportar el resultado como un csv
# library(readr)
# write.csv(resultado, "resultado.csv")