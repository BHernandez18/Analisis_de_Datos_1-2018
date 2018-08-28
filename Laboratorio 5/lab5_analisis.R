#
# Laboratorio 5 - Analisis de Datos
#
# Integrantes:  Pablo Caceres Luzanto
#               Benjamin Hernandez Cortes
#
# Fecha: 01 de Septiembre de 2018
# Profesor: Max Chacon
# Ayudante: Adolfo Guzman
#

#
# ======= Descarga de bibliotecas =======
#
# El presente archivo, utiliza las bibliotecas 'C50' y 'tidyverse' para realizar algunos estudios respecto
# a los datos que se emplean. Si no posee las bibliotecas, descomente la siguiente linea para instalarlos.

#install.packages(c("C50", "tidyverse"), dependencies = TRUE)

# Carga de las bibliotecas 'C50' y 'tidyverse'.
library(C50)
library(tidyverse)

#
# ========== Lectura de datos ============
#
# Lectura del conjunto de datos a analizar.
conjunto_datos <- read.table(file = "/home/benjamin/Escritorio/Análisis de Datos/zoo/zoo.data",
                             sep = ",",
                             header = FALSE)

# Dado que el archivo con el conjunto de datos a analizar no posee los nombres para cada uno de los
# atributos definidos, se asignaran los nombres correspondientes a los datos ya leidos, de acuerdo al
# archivo 'zoo.names' que contiene informacion resumida acerca del conjunto de datos.
nombres_atributos <- c("animal_name",
                       "hair",
                       "feathers",
                       "eggs",
                       "milk",
                       "airborne",
                       "aquatic",
                       "predator",
                       "toothed",
                       "backbone",
                       "breathes",
                       "venomous",
                       "fins",
                       "legs",
                       "tail",
                       "domestic",
                       "catsize",
                       "type")

# Se reemplazan los nombres de las columnas de la tabla 'conjunto_datos'.
colnames(conjunto_datos) <- nombres_atributos

#
# ========= Pre - Procesamiento ==========
#
# Antes de extraer conocimiento por medio de un Clasificador Bayesiano Ingenuo, es necesario preparar los datos
# que se poseen para el conjunto de datos 'Zoo'.

# Por ejemplo, el atributo 'animal_name' solo registra los nombres de los animales presentes en la muestra
# y por lo tanto, es un dato no-numerico. Dada la naturaleza del atributo, resulta util eliminarlo del
# proceso, aunque este se realizara posterior a la obtencion de los conjuntos de entrenamiento y prueba para
# el clasificador.

# En cuanto a las observaciones, se sabe que existen dos instancias de 'frog' y una de 'girl'. La unica
# diferencia que se da en las instancias de 'frog' esta definida por el atributo 'venomous' (venenoso), en
# donde solo una de ellas posee el atributo, por lo tanto, resulta adecuado eliminar una de ellas.
# Para el caso de la instancia 'girl', tampoco resulta adecuado mantenerlo dentro del conjunto de datos,
# debido a que el mismo conjunto fue destinado y confeccionado para detallar aspectos de los distintos
# ANIMALES pertenecientes a un zoologico.

conjunto_datos_filtrado <- conjunto_datos[-c(27, 30),] # Se elimina una observacion 'frog' y 'girl'

#
# ============ Procesamiento =============
#
# Se obtiene el conjunto de entrenamiento.
conjunto_datos_entrenamiento <- conjunto_datos_filtrado %>% group_by(type) %>% sample_frac(0.7)

# Se crea un vector que contiene todas las filas que no deberan considerarse dentro del conjunto de entramiento.
remover <- rownames(conjunto_datos_entrenamiento)

# Se obtiene el conjunto de prueba.
conjunto_datos_prueba <- anti_join(conjunto_datos_filtrado, conjunto_datos_entrenamiento, by = 'animal_name')

# Se elimina el atributo 'animal_name' para ambos conjuntos obtenidos previamente, por los motivos explicados en la
# fase de pre-procesamiento.
conjunto_datos_entrenamiento <- conjunto_datos_entrenamiento[, -c(1)]
conjunto_datos_prueba <- conjunto_datos_prueba[, -c(1)]

# Se define como factores el atributo 'type'
conjunto_datos_entrenamiento$type <- as.factor(conjunto_datos_entrenamiento$type)
conjunto_datos_prueba$type <- as.factor(conjunto_datos_prueba$type)

#
# =========== Obtencion del Arbol de Decision =========
#

# Reglas del arbol
reglas_modelo <- C50::C5.0(x = conjunto_datos_entrenamiento[, -17],
                           y = conjunto_datos_entrenamiento$type,
                           rules = TRUE)

# Resumen del modelo resultante
resumen_reglas <- summary(reglas_modelo)


# Arbol de decision
arbol_modelo <- C50::C5.0(x = conjunto_datos_entrenamiento[, -17],
                          y = conjunto_datos_entrenamiento$type)

# Resumen del modelo resultante
resumen_arbol <- summary(arbol_modelo)

# Plot del arbol de decision resultante.
plot(arbol_modelo)
