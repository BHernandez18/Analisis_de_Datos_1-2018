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
# El presente archivo, utiliza las bibliotecas 'e1071', 'dplyr' y 'caret' para realizar algunos estudios respecto
# a los datos que se emplean. Si no posee las bibliotecas, descomente la siguiente linea para instalarlos.

#install.packages(c("e1071", "dplyr", "tidyverse"), dependencies = TRUE)

# Carga de las bibliotecas 'e1071', 'dplyr' y 'caret'.
library(e1071)
library(dplyr)
library(caret)

#
# ========== Lectura de datos ============
#
# Lectura del conjunto de datos a analizar.
conjunto_datos <- read.table(file = "C:/Users/Familia Hernandez/Desktop/Analisis_de_Datos_1-2018/zoo/zoo.data",
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

# Luego, para aquellos atributos que estan definidos como binarios {0,1}, solo basta con identificar y reemplazar
# aquellos valores definidos como 1 por TRUE, y los definidos como 0 por FALSE.

for(i in nombres_atributos[-c(1, 14, 18)]) {
  conjunto_datos_filtrado[, i] <- as.logical(conjunto_datos_filtrado[, i])
}

# Siguiendo con el atributo 'type', en este se convertira cada una de las representaciones numericas a sus
# respectivos nombres con los cuales se caracterizaron, para luego definirlos como factores.
tipos_animales <- c("mamifero",
                    "ave",
                    "reptil",
                    "pez",
                    "anfibio",
                    "insecto",
                    "otro")

conjunto_datos_filtrado$type <- factor(conjunto_datos_filtrado$type)
levels(conjunto_datos_filtrado$type) <- factor(tipos_animales)

# En cuanto al atributo 'legs', se convertira en factores cada numero de patas definido en el conjunto de datos.
conjunto_datos_filtrado$legs <- factor(conjunto_datos_filtrado$legs)

#
# ============ Procesamiento =============
#

# Se configura una 'seed' para obtener siempre las mismas muestras en los conjuntos.
set.seed(1337)

# Para empezar a diseñar un Clasificador Bayesiano Ingenuo, se separá el conjunto de datos en otros 2 conjuntos: uno de
# de entrenamiento ('training') y otro de prueba ('test'). El conjunto de entrenamiento contendra un 70% de las
# observaciones originales y por lo tanto, el conjunto de prueba tendrá el 30% restante. Para asegurar que el conjunto
# de entrenamiento contiene al menos una observacion de cada tipo de animal presente en el conjunto de datos, se obtendra
# una muestra que contenga el 70% de cada tipo de animal.
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

#
# =========== Obtencion del Clasificador =========
#
# A continuacion, se obtendra el Clasificador Bayesiano Ingenuo a partir del conjunto de datos de entrenamiento, empleando
# la funcion 'naiveBayes()' de la biblioteca 'e1071'.
modelo_bayesiano <- naiveBayes(type ~ ., conjunto_datos_entrenamiento)

# Se pone a prueba el modelo obtenido con el conjunto de datos de prueba.
prediccion <- predict(modelo_bayesiano, conjunto_datos_prueba)

#
# ========= Resultados ==========
#
# Se obtiene una matriz de confusion respecto a los resultados obtenidos con el clasificador conseguido, para poder
# evaluar la precision del clasificador.
matriz_confusion <- table(conjunto_datos_prueba$type, prediccion)

# Se guarda la tabla obtenida en un archivo '.csv'
write.csv(matriz_confusion,
          file = "matriz_confusion.csv",
          row.names = TRUE)

cat("=== Matriz de Confusion ===\n\n")
resultados <- confusionMatrix(matriz_confusion)
print(resultados)
