# ---------------------------------
# Laboratorio 1 - Analisis de Datos
# ---------------------------------
# Integrantes:  Pablo Caceres Luzanto
#               Benjamin Hernandez Cortes
#
# Fecha: 27 de Abril de 2018
# Profesor: Max Chacon
# Ayudante: Adolfo Guzman
# ---------------------------------

# El presente archivo utiliza la biblioteca "tidyverse" para realizar algunos estudios respecto
# a los datos que se emplean. Si no posee la biblioteca, descomente la siguiente linea para instalarlo.
#install.packages("tidyverse")

# Carga de la biblioteca "tidyverse".
library(tidyverse)

# Lectura del conjunto de datos a analizar.
datos <- read_delim(file = "/home/benjamin/Escritorio/zoo/zoo.data",
                    delim = ",",
                    col_names = FALSE)

numero_atributos <- ncol(datos)
numero_observaciones <- nrow(datos)

# Dado que el archivo con el conjunto de datos a analizar, no posee los nombres para cada uno de los
# atributos definidos, se asignaran los nombres correspondientes a los datos ya leidos, en base al
# archivo que contiene informacion resumida acerca del conjunto de datos.
nombres_atributos <- c("animal name",
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
colnames(datos) <- nombres_atributos

# ========================================
# ==== Analisis del conjunto de datos ====

cat("==== Analisis del conjunto de datos 'zoo' ==== \n")
cat("\n >>> Medias muestrales <<<")

# Se calculara la media muestral para cada uno de los atributos que se definen en los datos.
# Dado que muchos de los atributos son del tipo booleano (valor {0,1}), la media nos permitira
# identificar el porcentaje de observaciones que presentan alguno de los atributos definidos.
# Se excluyen de lo anterior, los atributos de tipo numerico "legs" y "type" y el atributo
# "animal name".

medias <- sapply( 2:numero_atributos,
                  function(i) round( mean(datos[[i]]), digits = 4 )
                  )

atributos_medias <- nombres_atributos[-c(1, 14, 18)]
medias <- medias[-c(13, 17)]

sapply( 1:length(atributos_medias),
        function(i) cat(sprintf("\n Atributo: %-8s | Media muestral: %0.4f",
                                atributos_medias[i],
                                medias[i]))
        )

# -----------------------------------------------------------------------------------------
# Visualizacion de las medias de cada atributos, considerando cada una de las observaciones
medias_tabuladas <- data.frame("Atributos" = atributos_medias,
                               "Porcentaje" = medias)

# Ploteo de los datos
p1 <-
  ggplot(data = medias_tabuladas) +
  geom_col(aes(Atributos, Porcentaje, fill = Porcentaje),
           color = "black") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Porcentaje del total de observaciones que\npresentan un determinado atributo",
       y = "")

plot(p1)

# Se guarda los 'plot' en un archivo.
dev.copy(png, "p1.png")
dev.off()

# -----------------------------------------------------------
# Visualizacion de las medias de cada atributo, segun el tipo
medias_por_tipo <-
  datos %>%
  group_by(type) %>%
  summarise_at(
    vars(-`animal name`, -legs),
    funs(mean))

long_medias_por_tipo <-
  medias_por_tipo %>%
  gather(Atributo, Media, 2:16)

for(var in medias_por_tipo$type) {
  p_aux <-
    ggplot(data = long_medias_por_tipo[long_medias_por_tipo$type == var,]) +
    geom_col(aes(Atributo, Media, fill = Media),
             color = "black") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = sprintf("Porcentaje de observaciones del tipo %d\nque presentan un determinado atributo", var),
         ylab = "")
  plot(p_aux)
  dev.copy(png, sprintf("p_type%d.png", var))
  dev.off()
}

# p2 <-
#   ggplot(data = long_medias_por_tipo) +
#   geom_col(aes(Atributo, Media, fill = Media),
#            color = "black") +
#   facet_wrap(~ type, nrow = 2) +
#   theme(axis.text.x = element_text(angle = 90),
#         plot.title = element_text(hjust = 0.5)) +
#   labs(title = "Porcentaje de observaciones que\npresentan un determinado atributo\nde acuerdo al tipo",
#        y = "Porcentaje")
# 
# 
# plot(p2)
# dev.copy(png, "p2.png")
# dev.off()

# -
# Visualizacion del atributo patas, para todas las observaciones
p3 <-
  ggplot(data = datos) +
  geom_histogram(aes(x = legs),
           color = "black",
           fill = "red",
           center = 0,
           binwidth = 0.5) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Frecuencia de las patas\nconsiderando todas las observaciones",
       x = "Cantidad de patas",
       y = "Frecuencia") +
  ylim(c(0,40))

plot(p3)
dev.copy(png, "p2.png")
dev.off()
