#
# Laboratorio 3 - Analisis de Datos
#
# Integrantes:  Pablo Caceres Luzanto
#               Benjamin Hernandez Cortes
#
# Fecha: 1 de Junio de 2018
# Profesor: Max Chacon
# Ayudante: Adolfo Guzman
#

#
# ======= Descarga de bibliotecas =======
#
# El presente archivo, utiliza las bibliotecas 'arules' y 'arulesViz' para realizar algunos estudios respecto
# a los datos que se emplean. Si no posee las bibliotecas, descomente la siguiente linea para instalarlos.

#install.packages(c("arules", "arulesViz", "igraph"), dependencies = TRUE)

# Carga de las bibliotecas "arules" y "arulesViz".
library(arules)
library(arulesViz)

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
# Antes de extraer conocimiento por medio de reglas de asociacion, es necesario preparar los datos que se
# poseen para el conjunto de datos 'Zoo'.

# Primero, y en cuanto a las observaciones, se sabe que existen dos instancias de 'frog' y una de 'girl'. La unica
# diferencia que se da en las instancias de 'frog' esta definida por el atributo 'venomous' (venenoso), en
# donde solo una de ellas posee el atributo, por lo tanto, resulta adecuado eliminar una de ellas.
# Para el caso de la instancia 'girl', tampoco resulta adecuado mantenerlo dentro del conjunto de datos,
# debido a que el mismo conjunto fue destinado y confeccionado para detallar aspectos de los distintos
# ANIMALES pertenecientes a un zoologico.

conjunto_datos <- conjunto_datos[-c(27, 30),] # Se elimina una observacion 'frog' y 'girl'

# Para poder trabajar con el conjunto de datos, es necesario convertir los datos en "logicos" (logical) o
# en "factores" (factor) segun corresponda, dado que las funciones contenidas en la biblioteca 'arulesViz',
# requieren de un 'dataset' presentado de tal manera.

# Para aquellos atributos que estan definidos como binarios {0,1}, solo basta con identificar y reemplazar
# aquellos valores definidos como 1 por TRUE, y los definidos como 0 por FALSE. En el caso particular del
# atributo 'legs', se binarizara sus datos de forma tal, que aquellos animales sin patas quedaran definidos
# como FALSE y en el caso contrario, como TRUE.

for(i in nombres_atributos[-c(1, 14, 18)]) {
  conjunto_datos[, i] <- as.logical(conjunto_datos[, i])
}

# Luego, en el atributo 'legs' se convertira en factores cada numero de patas definido en el conjunto de datos
conjunto_datos$legs <- factor(conjunto_datos$legs)

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

# Finalmente, se obtiene un 'data.frame' cuyos identificadores para cada una de las observaciones estara dado
# por los nombres de los mismos animales. Seguido de este paso, se procede a eliminar el atributo 'animal_name'
# definido previamente.

conjunto_datos <- as.data.frame(x = conjunto_datos,
                                row.names = as.character(conjunto_datos$animal_name), # Identificadores por nombre
                                col.names = nombres_atributos[-c(1)])

conjunto_datos <- conjunto_datos[, -c(1)] # Se elimina el atributo 'animal_name'

#
# ============ Procesamiento =============
#
# Los datos almacenados en el 'data.frame' deben ser transformados a un conjunto de transacciones, donde cada
# una de las filas representara una transaccion y cada columna sera convertida en "items". Lo anterior, quiere
# decir que se considerara cada animal como una transaccion, mientras que los diferentes atributos presentes
# en el 'data.frame' seran los "items" que cada animal tiene.
transacciones <- as(conjunto_datos, "transactions")

# A continuacion, se muestra a traves de una matriz binaria las caracteristicas que presentan cada uno de los
# animales.
plot_matriz_binaria <- image(transacciones, xlab = "Items (Columnas)", ylab = "Transacciones (Filas)")
plot(plot_matriz_binaria)

dev.copy(png, "plot_matriz_binaria.png")
dev.off()

# Como apoyo, se agrega una grafico que presenta las frecuencias relativas de los 20 "items" con mayor presencia
# en el conjunto de datos.
plot_freq_relativa <- itemFrequencyPlot(transacciones, topN = 20, ylab = "Frecuencia del item (relativa)")

dev.copy(png, "plot_freq_relativa.png")
dev.off()

#
# ========== Obtencion de reglas =========
#
# Para la obtencion de reglas de asociacion, se emplea el algoritmo 'a priori' para encontrar un conjunto
# de items mas frecuentes sobre nuestra 'base de datos transaccional'. Asi mismo, se define un soporte minimo
# del 5% y una confianza minima del 90%, dado que el conjunto de datos a estudiar es muy pequeño. Tambien se
# ajusta la busqueda de reglas de modo que el consecuente de las mismas (RHS -> Right-Hand Side) sea alguno de
# los tipos de animales que se estudian del conjunto de datos.
reglas <- apriori(transacciones,
                  parameter = list(support = 0.05, confidence = 0.9),
                  appearance = list(rhs = c("type=mamifero",
                                            "type=ave",
                                            "type=reptil",
                                            "type=pez",
                                            "type=anfibio",
                                            "type=insecto",
                                            "type=otro"))
                 )

# Se guardan las reglas obtenidas en un archivo '.csv'
write(reglas,
      file = "reglas_de_asociacion.csv",
      sep = ",",
      quote = TRUE,
      row.names = FALSE)

# Una vez obtenidas las reglas, se ordenan por "soporte", "confianza" y "lift". Esta ultima metrica permite
# obtener el grado de dependencia entre los distintos items que componen cada una de las reglas, mientras mayor
# sea el valor del parametro, mas dependientes seran los items.
reglas_por_soporte <- sort(reglas, by = "support")
reglas_por_confianza <- sort(reglas, by = "confidence")
reglas_por_lift <- sort(reglas, by = "lift")

# Se muestran por consola las 10 primeras reglas obtenidas previamente ordenadas segun su respectivo parametro.
cat("\n== REGLAS ORDENADAS POR SOPORTE (DESCENDENTE) ==\n")
inspect(head(reglas_por_soporte, 10))

cat("\n== REGLAS ORDENADAS POR CONFIANZA (DESCENDENTE) ==\n")
inspect(head(reglas_por_confianza, 10))

cat("\n== REGLAS ORDENADAS POR LIFT' (DESCENDENTE) ==\n")
inspect(head(reglas_por_lift, 10))

#
# ====== Visualizacion de reglas =========
#
# Para visualizar diversos datos interesantes de las reglas, se empleara la biblioteca 'arulesViz'.
# Primero se visualizaran las reglas obtenidas previamente a traves de una 'matriz agrupada', en donde
# sera posible identificar los antecedentes y consecuentes de algunas reglas. El tamano indicara el
# soporte de las reglas y el color la metrica 'lift' (dependencia entre los items).

matriz_agrupada_reglas <- plot(reglas, method = "grouped")

dev.copy(png, "plot_matriz_agrupada_reglas.png")
dev.off()

# Luego, es posible establecer una especie de "Clustering" a partir de las reglas obtenidas previamente.
# Sin embargo, el metodo empleado no soporta mas de 100 reglas, por lo que tampoco es categorico establecer
# conclusiones concretas al respecto.

agrupamiento_reglas <- plot(head(reglas, 100), method = "graph")

dev.copy(png, "plot_agrupamiento_reglas.png")
dev.off()