# ---------------------------------
# Laboratorio 2 - Analisis de Datos
# ---------------------------------
# Integrantes:  Pablo Caceres Luzanto
#               Benjamin Hernandez Cortes
#
# Fecha: 18 de Mayo de 2018
# Profesor: Max Chacon
# Ayudante: Adolfo Guzman
# ---------------------------------

# El presente archivo, utiliza las bibliotecas "ggplot2", "cluster" y "factoextra" para realizar algunos estudios
# respecto a los datos que se emplean. Si no posee las bibliotecas, descomente la siguiente linea para instalarlos.
#install.packages(c("ggplot2", "cluster", "factoextra"), dependencies = TRUE)

# Carga de las bibliotecas "ggplot2", "cluster" y "factoextra"
library(ggplot2)
library(cluster)
library(factoextra)

# Lectura del conjunto de datos a analizar
conjunto_datos <- read.delim(file = "/home/benjamin/Escritorio/Lab 2 - Analisis de Datos/zoo/zoo.data",
                             sep = ",",
                             header = FALSE)

# Dado que el archivo con el conjunto de datos a analizar, no posee los nombres para cada uno de los
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

# Se reemplazan los nombres de las columnas.
colnames(conjunto_datos) <- nombres_atributos

# ========================================
# ========= Pre - Procesamiento ==========

# Antes de realizar un Clustering del conjunto de datos, se vuelve util analizar el conjunto de datos y
# eliminar columnas o registros no relevantes.

# Por ejemplo, el atributo 'animal_name' solo registra los nombres de los animales presentes en la muestra
# y por lo tanto, es un dato no-numerico. Dada la naturaleza del atributo, resulta util eliminarlo del
# proceso de Clustering.

# Asi mismo, el atributo 'type' no debiera formar parte del Clustering, dado que el atributo ya registra
# las clases a las que pertenece cada observaci?n, y esto podria generar un Cluster desconfiable.

# En cuanto a las observaciones, se sabe que existen dos instancias de 'frog' y una de 'girl'. La unica
# diferencia que se da en las instancias de 'frog' esta definida por el atributo 'venomous' (venenoso), en
# donde solo una de ellas posee el atributo, por lo tanto, resulta adecuado eliminar una de ellas.
# Para el caso de la instancia 'girl', tampoco resulta adecuado mantenerlo dentro del conjunto de datos,
# debido a que el mismo conjunto fue destinado y confeccionado para detallar aspectos de los distintos
# ANIMALES pertenecientes a un zoologico.

conjunto_datos_filtrado <- conjunto_datos[, -c(1,18)] # Se elimina los atributos 'animal_name' y 'type'
conjunto_datos_filtrado <- conjunto_datos_filtrado[-c(27, 30),] # Se elimina una observacion 'frog' y 'girl'

conjunto_datos_analisis <- conjunto_datos[, -c(1)]
conjunto_datos_analisis <- conjunto_datos_analisis[-c(27, 30),]

# ========================================
# ======== Obtencion del Cluster =========

# Primero hay que entender que la funci?n 'pam()' del paquete 'cluster', hace referencia al m?todo de
# Particionamiento Alrededor de Medianas (Partitioning Around Medoids), el cual es una versi?n m?s
# robusta del m?todo de agrupamiento de 'K-medias'.

# Este, al ser un metodo de agrupamiento no-jerarquico, requiere que se defina a priori un valor para k
# (numero de particiones). Una forma de calcular el valor de k mas adecuado, es a traves del m?todo
# de 'promedio de siluetas'. Considerese que la medida 'silueta' hace referencia a que tan similar
# es un objeto con su propio cluster, comparado frente a los demas.

# Visualizacion del valor de k adecuado.
plot_valor_k <-
  fviz_nbclust(conjunto_datos_filtrado,
               pam,
               method = "silhouette") +
  theme_classic()

plot(plot_valor_k)

# Se guarda el 'plot' en un archivo.
dev.copy(png, "plot_k_optimo.png")
dev.off()

# Se logra observar en el grafico, que segun el metodo anterior, el valor adecuado debiera ser k = 4.
# Se realiza el Cluster con k = 4.
cluster_k4 <- pam(x = conjunto_datos_filtrado,
               metric = "manhattan",
               diss = FALSE,
               k = 4,
               stand = TRUE)

# Resumen del proceso de Clustering
show( summary(cluster_k4) )

# Visualizacion del cluster
plot_cluster_k4 <- fviz_cluster(cluster_k4, 
             ellipse.type = "norm",
             repel = TRUE,
             ggtheme = theme_classic(),
             main = "Clustering con k = 4")

plot(plot_cluster_k4)

# Se guarda el 'plot' en un archivo.
dev.copy(png, "plot_cluster_k4.png")
dev.off()

# Adem?s del Cluster obtenido anteriormente, resulta interesante saber que sucede con el Cluster,
# al momento de definir un valor de k = 7, a fin de comparar los resultados de este con el conjunto
# de datos original.

# Se realiza el Cluster con k = 7, para comparar su resultado con el conjunto de datos original.
cluster_k7 <- pam(x = conjunto_datos_filtrado,
                  metric = "manhattan",
                  diss = FALSE,
                  k = 7,
                  stand = TRUE)

# Resumen del proceso de Clustering
show( summary(cluster_k7) )

# Visualizacion del cluster
plot_cluster_k7 <- fviz_cluster(cluster_k7, 
                                ellipse.type = "norm",
                                repel = TRUE,
                                ggtheme = theme_classic(),
                                main = "Clustering con k = 7")

plot(plot_cluster_k7)

# Se guarda el 'plot' en un archivo.
dev.copy(png, "plot_cluster_k7.png")
dev.off()


# ========================================
# ========= Analisis posterior ===========

# Se genera una matriz de coincidencias, para comparar la distribucion del conjunto de datos original
# frente al Clustering. Esto se realiza para el Cluster con valor de k = 4 y k = 7.

# Las filas representan las clasificaciones originales de las observaciones, mientras que las columnas
# las clasificaciones dadas por el proceso de Clustering.
matriz_resultados_k4 <- as.data.frame(matrix(0, nrow = 7, ncol = 4))
matriz_resultados_k7 <- as.data.frame(matrix(0, nrow = 7, ncol = 7))


colnames(matriz_resultados_k4) <- c("type.1", "type.2", "type.3", "type.4")
rownames(matriz_resultados_k4) <- c("type.1", "type.2", "type.3", "type.4", "type.5", "type.6", "type.7")

colnames(matriz_resultados_k7) <- c("type.1", "type.2", "type.3", "type.4", "type.5", "type.6", "type.7")
rownames(matriz_resultados_k7) <- c("type.1", "type.2", "type.3", "type.4", "type.5", "type.6", "type.7")

for(i in 1:length(cluster_k4$clustering)) {
  clasificacion_original <- conjunto_datos_analisis[i, 17]
  clasificacion_cluster_k4 <- cluster_k4$clustering[i]
  
  if(clasificacion_original == clasificacion_cluster_k4) {
    matriz_resultados_k4[clasificacion_original, clasificacion_original] <-
      matriz_resultados_k4[clasificacion_original, clasificacion_original] + 1
  }
  
  else {
    matriz_resultados_k4[clasificacion_original, clasificacion_cluster_k4] <-
      matriz_resultados_k4[clasificacion_original, clasificacion_cluster_k4] + 1
  }
}

for(i in 1:length(cluster_k7$clustering)) {
  clasificacion_original <- conjunto_datos_analisis[i, 17]
  clasificacion_cluster_k7 <- cluster_k7$clustering[i]
  
  if(clasificacion_original == clasificacion_cluster_k7) {
    matriz_resultados_k7[clasificacion_original, clasificacion_original] <-
      matriz_resultados_k7[clasificacion_original, clasificacion_original] + 1
  }
  
  else {
    matriz_resultados_k7[clasificacion_original, clasificacion_cluster_k7] <-
      matriz_resultados_k7[clasificacion_original, clasificacion_cluster_k7] + 1
  }
}



# Guardamos la informacion de la matriz de coindencias, en un archivo '.csv'
write.csv(matriz_resultados_k4, "matriz_coincidencias_k4.csv", row.names = TRUE)
write.csv(matriz_resultados_k7, "matriz_coincidencias_k7.csv", row.names = TRUE)

cat("\n\n === Matriz de Coincidencias == \n")
show(matriz_resultados_k4)
cat("\n\n")
show(matriz_resultados_k7)

