#
# Laboratorio 6 - Analisis de Datos
#
# Integrantes:  Pablo Caceres Luzanto
#               Benjamin Hernandez Cortes
#
# Fecha: 13 de Septiembre de 2018
# Profesor: Max Chacon
# Ayudante: Adolfo Guzman
#

#
# ======= Descarga de bibliotecas =======
#
# El presente archivo, utiliza las bibliotecas 'MTS', 'signal', 'TSA' y 'oce' para realizar algunos estudios
# respecto a los datos que se emplean. Si no posee las bibliotecas, descomente la siguiente linea para instalarlos.

#install.packages(c("MTS", "signal", "TSA", "oce"), dependencies = TRUE)

# Carga de las bibliotecas.
library(MTS)
library(signal)
library(TSA)
library(oce)

#
# ========== Lectura de datos ============
#
# Se define el directorio donde se ubican los archivos a leer. Si no esta usando RStudio, asigne de forma manual la
# cadena de caracteres (string) en donde se ubican los archivos.
directorio <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/data")

# Se genera un vector con los nombres de cada uno de los archivos a leer.
nombres_archivos <- c("DY000.txt", "DY001.txt",
                      "H2000.txt", "H2001.txt",
                      "RP000.txt", "RP001.txt",
                      "SH000.txt", "SH001.txt",
                      "TJ000.txt", "TJ001.txt",
                      "VH000.txt", "VH001.txt")

# Se genera un vector que contiene los identificadores de cada uno de los sujetos.
nombres_sujetos <- rep(c("DY", "H2", "RP", "SH", "TJ", "VH"), each = 2)

# Se genera un vector con los nombres de los atributos (columnas) para cada conjunto de datos.
nombres_atributos <- c("PAM", "CO2", "VFSC")

# Se obtiene un vector con las rutas de cada uno de los archivos a leer.
rutas_archivos <- sapply(nombres_archivos, function(i) file.path(directorio, i))

# Se procede a leer cada uno de los correspondientes archivos.
datos_archivos <- lapply(rutas_archivos, function(i) read.table(file = i,
                                                                sep = "\t",
                                                                header = FALSE))

# Se asignan los datos obtenidos previamente, a variables representativas y separados por 'normocapnia' e 'hipercapnia'.
# Ademas, se aprovecha de reemplazar los nombres de los atributos (columnas) para cada uno de los conjuntos de datos.
for (i in 1:length(nombres_archivos)) {
  if (i %% 2 == 1) {
    nombre_variable <- paste0("datos_normocapnia_", nombres_sujetos[i])
  } else {
    nombre_variable <- paste0("datos_hipercapnia_", nombres_sujetos[i])
  }
  datos_tmp <- datos_archivos[[i]]            # Se obtiene el conjunto de datos
  colnames(datos_tmp) <- nombres_atributos    # Se reemplazan los nombres de los atributos (columnas)
  assign(nombre_variable, datos_tmp)          # Se asignan los datos a una variable representativa
}

