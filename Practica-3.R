library(readr)
library(stringr)


epa_http <- read_table("C:/Rita/DataScience/epa-http/epa-http.csv",col_names = FALSE)
View(epa_http)


#Nombres de columnas
names(epa_http) <- c("SourceName", "hora", "metodo", "recurso", "protocolo", "codigo_retorno", "tamano")

#Tipos de datos
epa_http$tamano <- as.numeric(epa_http$tamano)
epa_http$metodo <- str_sub(epa_http$metodo, 2)
epa_http$metodo <- as.factor(epa_http$metodo)


#Clustering de Datos
#Pregunta 5

library(mltools)
library(data.table)
epa_http_one_hot <- one_hot(as.data.table(epa_http), sparsifyNAs = TRUE)

#Reemplaza los NA de tamaño
epa_http_one_hot$tamano <-  ifelse(is.na(epa_http_one_hot$tamano), length(epa_http_one_hot$recurso), epa_http_one_hot$tamano)

#---- pruebas ---------------------------------
# K Means
 set.seed(1234)
 x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
 y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
 plot(x,y,col="blue", pch=19, cex=2, main = "Algoritmo K-Means")
 text(x+0.05, y+0.05, labels = as.character(1:12))
#----------------------------------------------

k <- 3
datos <- data.frame( metodo_GET=epa_http_one_hot$metodo_GET, metodo_HEAD=epa_http_one_hot$metodo_HEAD, metodo_POST=epa_http_one_hot$metodo_POST)
scale(datos)
resultado <- kmeans( datos, k)
print(resultado$centers)

# Graficar los puntos de datos coloreados según su cluster
plot(datos, col = resultado$cluster, pch = 19)
points(resultado$centers, col = 1:k, pch = 8, cex = 2)






