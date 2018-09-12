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

# Se genera un vector que identifica si el estado es normocapnia o hipercapnia.
nombres_estados <- rep(c("Normocapnia", "Hipercapnia"), 6)

# Se genera un vector con los nombres de los atributos (columnas) para cada conjunto de datos.
nombres_atributos <- c("PAM", "CO2", "VFSC")

# Se obtiene un vector con las rutas de cada uno de los archivos a leer.
rutas_archivos <- sapply(nombres_archivos, function(i) file.path(directorio, i))

# Se procede a leer cada uno de los correspondientes archivos.
datos_archivos <- lapply(rutas_archivos, function(i) read.table(file = i,
                                                                sep = "\t",
                                                                header = FALSE))

# Se genera una variable que contendra el nombre de todas las demas variables que se generaran.
nombres_variables <- vector(mode = "character", length = length(nombres_sujetos))

# Se asignan los datos obtenidos previamente, a variables representativas y separados por 'normocapnia' e 'hipercapnia'.
# Ademas, se aprovecha de reemplazar los nombres de los atributos (columnas) para cada uno de los conjuntos de datos.
for (i in 1:length(nombres_sujetos)) {
  if (i %% 2 == 1) {
    variable <- paste0("datos_normocapnia_", nombres_sujetos[i])
  } else {
    variable <- paste0("datos_hipercapnia_", nombres_sujetos[i])
  }
  datos_tmp <- datos_archivos[[i]]            # Se obtiene el conjunto de datos
  colnames(datos_tmp) <- nombres_atributos    # Se reemplazan los nombres de los atributos (columnas)
  assign(variable, datos_tmp)          # Se asignan los datos a una variable representativa
  nombres_variables[i] <- variable     # Se almacena el nombre de la variable creada
}

#
# ========= Pre - Procesamiento ==========
#
# Dado que se trabajara con señales muestreadas a 5Hz, resulta necesario determinar si dentro de algun conjunto de datos
# existe datos u observaciones incompletas que podrian producir errores dentro del analisis.
cat("\n== Verificando que los 'datasets' no presenten observaciones nulas ==\n")
for (i in nombres_variables) {
  datos_tmp <- get(i)
  filas_nulas <- datos_tmp[!complete.cases(datos_tmp),]
  if (nrow(filas_nulas) == 0) {
    cat(paste("El 'dataset'", i, "no posee datos incompletos\n"))
  } else {
    cat(paste("El 'dataset'", i, "posee datos incompletos\n"))
  }
}
# Como se logra apreciar, ningun conjunto de datos posee datos nulos y se puede proceder con el procesamiento de los datos.

#
# ============ Procesamiento =============
#
# Dado que cada uno de los conjuntos de datos corresponden a un muestro de señales, es necesario generar una serie de tiempo
# que represente a cada conjunto de datos. Para esto, se emplea la funcion ts() que se encargara de crear un objeto
# 'serie de tiempo' a partir de la frecuencia a la que fueron muestreadas las señales (5Hz).
series_de_tiempo_datos <- lapply(nombres_variables, function(i) ts(get(i), frequency = 5))

# Una vez obtenidas las series de tiempo, se procede a graficarlas para realizar un analisis a las mismas.
titulos <- paste("Señal de", nombres_estados, "del sujeto", nombres_sujetos)
lowercase_nombres_estados <- tolower(nombres_estados)
lowercase_nombres_sujetos <- tolower(nombres_sujetos)
for (i in 1:length(series_de_tiempo_datos)) {
  datos_tmp <- series_de_tiempo_datos[[i]]
  png(filename = paste0("plot_", lowercase_nombres_estados[i], "_", lowercase_nombres_sujetos[i], ".png"),
      width = 813,
      height = 457,
      res = 80)
  plot(datos_tmp, main = titulos[i], xlab = "Tiempo")
  dev.off() # Se guardan los plot hechos.
}

#
# >> Calculo de la funcion de correlacion cruzada entre PAM y VFSC para cada sujeto <<
#
correlaciones_cruzadas <- list()
titulos <- paste("CCF entre PAM y VFSC del sujeto", nombres_sujetos, "en", nombres_estados)
for (i in 1:length(nombres_variables)) {
  datos_tmp <- get(nombres_variables[i])
  correlaciones_cruzadas[[i]] <- ccf(x = datos_tmp[,"PAM"], y = datos_tmp[,"VFSC"], plot = FALSE)
  png(filename = paste0("plot_ccf_", lowercase_nombres_estados[i], "_", lowercase_nombres_sujetos[i], ".png"),
      res = 80)
  plot(correlaciones_cruzadas[[i]], main = titulos[i], ylab = "CCF")
  dev.off()
}

#
# >> Calculo de la funcion de autocorrelacion de PAM para cada sujeto <<
#
autocorrelaciones <- list()
titulos <- paste("ACF de PAM del sujeto", nombres_sujetos, "en", nombres_estados)
for (i in 1:length(nombres_variables)) {
  datos_tmp <- get(nombres_variables[i])
  autocorrelaciones[[i]] <- acf(x = datos_tmp[,"PAM"], plot = FALSE)
  png(filename = paste0("plot_acf_", lowercase_nombres_estados[i], "_", lowercase_nombres_sujetos[i], ".png"),
      res = 80)
  plot(autocorrelaciones[[i]], main = titulos[i])
  dev.off()
}

#
# >> Obtencion de las funciones de transferencia <<
#
funciones_transferencia <- list()
for (i in 1:length(nombres_variables)) {
  datos_tmp <- get(nombres_variables[i])
  fft_entrada <- fft(datos_tmp[, "PAM"])
  fft_salida <- fft(datos_tmp[, "VFSC"])
  funciones_transferencia[[i]] <- fft_salida / fft_entrada
}

#
# >> Obtencion de las funciones escalon para PAM <<
#
funciones_escalon <- list()
largo_vectores <- c()
for (i in 1:length(nombres_variables)) {
  datos_tmp <- get(nombres_variables[i])
  pam_tmp <- datos_tmp[["PAM"]]
  funciones_escalon[[i]] <- stepfun(2:length(pam_tmp), pam_tmp)
  largo_vectores <- append(largo_vectores, length(pam_tmp))
}

#
# >> Obtencion de las funciones escalon inverso para PAM <<
#
funciones_escalon_inverso <- list()
for (i in 1:length(funciones_escalon)) {
  funcion_escalon <- funciones_escalon[[i]]
  valores_escalon_inverso <- sapply(1:largo_vectores[i], function(i) 1-funcion_escalon(i))
  funcion_escalon_inverso <- stepfun(2:largo_vectores[i], valores_escalon_inverso)
  funciones_escalon_inverso[[i]] <- funcion_escalon_inverso
}