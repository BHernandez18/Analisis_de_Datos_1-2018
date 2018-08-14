#
# Laboratorio 4 - Analisis de Datos
#
# Integrantes:  Pablo Caceres Luzanto
#               Benjamin Hernandez Cortes
#
# Fecha: 17 de Agosto de 2018
# Profesor: Max Chacon
# Ayudante: Adolfo Guzman
#

#
# ======= Descarga de bibliotecas =======
#
# El presente archivo, utiliza las bibliotecas 'arules' y 'arulesViz' para realizar algunos estudios respecto
# a los datos que se emplean. Si no posee las bibliotecas, descomente la siguiente linea para instalarlos.

#install.packages(c("e1071"), dependencies = TRUE)

# Carga de las bibliotecas "arules" y "arulesViz".
library(e1071)

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
# proceso.

conjunto_datos <- conjunto_datos[, -c(1)] # Se elimina el atributo 'animal_name'

# En cuanto a las observaciones, se sabe que existen dos instancias de 'frog' y una de 'girl'. La unica
# diferencia que se da en las instancias de 'frog' esta definida por el atributo 'venomous' (venenoso), en
# donde solo una de ellas posee el atributo, por lo tanto, resulta adecuado eliminar una de ellas.
# Para el caso de la instancia 'girl', tampoco resulta adecuado mantenerlo dentro del conjunto de datos,
# debido a que el mismo conjunto fue destinado y confeccionado para detallar aspectos de los distintos
# ANIMALES pertenecientes a un zoologico.

conjunto_datos <- conjunto_datos[-c(27, 30),] # Se elimina una observacion 'frog' y 'girl'

# Siguiendo con el atributo 'type', en este se convertira cada una de las representaciones numericas a sus
# respectivos nombres con los cuales se caracterizaron, para luego definirlos como factores.
tipos_animales <- c("mamifero",
                    "ave",
                    "reptil",
                    "pez",
                    "anfibio",
                    "insecto",
                    "otro")

conjunto_datos$type <- factor(conjunto_datos$type)
levels(conjunto_datos$type) <- factor(tipos_animales)

#
# ============ Procesamiento =============
#
conjunto_datos_filtrado <- conjunto_datos[,-c(17)]

modelo_bayesiano <- naiveBayes(type ~ ., conjunto_datos)
prediccion <- predict(modelo_bayesiano, conjunto_datos)

tabla_contingencia <- table(prediccion, conjunto_datos$type)
print(tabla_contingencia)
