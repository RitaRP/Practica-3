library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(mltools)
# Cargando datos
epa_http <- read_table("C:/Users/Rodo/Documents/epa-http/epa-http.csv", col_names = FALSE)
names(epa_http) <- c("SourceName", "hora", "metodo", "recurso", "protocolo", "codigo_retorno", "tamano")

# Conversiones
epa_http$metodo <- as.factor(epa_http$metodo)
epa_http$protocolo <- as.factor(epa_http$protocolo)
epa_http$codigo_retorno <- as.factor(epa_http$codigo_retorno)
epa_http$tamano <- as.numeric(epa_http$tamano)
epa_http$tamano <- ifelse(is.na(epa_http$tamano), 0, epa_http$tamano)
nrow(epa_http)
View(epa_http)

#SourceName y Codigo_Retorno
# Creando nueva tabla segun las repeticiones de las direcciones, para obtener direcciones únicas
Tab_SourceName <- data.frame(SourceName = epa_http$SourceName, codigo_retorno =epa_http$codigo_retorno)
concurrencia <- as.data.frame(table(Tab_SourceName))

# Hallando la frecuencia de la columna http, filtrando previamente los recursos tipo imagen
#image_data <- epa_http %>% filter(!grepl("(?i)\\.(gif|jpg|jpeg|png|bmp)$", recurso))

Tab_Retorno <-table(epa_http$codigo_retorno)
codigo_retorno_table <- data.frame(codigo_retorno = names(Tab_Retorno),
                                  Frecuencia = as.vector(Tab_Retorno))

#Grafica 1
ggplot(codigo_retorno_table, aes(x = codigo_retorno, y = Frecuencia,  fill = codigo_retorno)) +
  geom_bar(stat = "identity") +
  labs(title = "Código de Retorno", x = "Código", y = "Cantidad")

#Grafica 2
ggplot(codigo_retorno_table, aes(x = "", y = Frecuencia, fill = codigo_retorno)) +
  geom_bar(stat = "identity") +
  labs(title = "Código de Retorno", fill = "Código") +  theme_void()


########Clustering


epa_http_2 <- epa_http[, c("metodo", "codigo_retorno", "protocolo")]
epa_http_one_hot <- one_hot(as.data.table(epa_http_2), sparsifyNAs = TRUE)
epa_http$recurso_size <- nchar(epa_http$recurso)
results1 <- kmeans(epa_http_one_hot, centers = 3)

# Gráfica
set.seed(123)

legend("topright", legend = levels(factor(results1$cluster)), col = colores2, pch = 16)
colores2 <- rainbow(n = length(unique(results1$cluster)))
grap1 <- plot(x = epa_http$tamano, y = epa_http$recurso_size, col = colores2[results1$cluster], main="Tamaño")
