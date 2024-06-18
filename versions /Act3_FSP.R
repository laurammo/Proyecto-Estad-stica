rm(list=ls())

library(dplyr)   # Para realizar operaciones con dataframes y trabajar con pipes
library(tidyr)
library(tidyverse)

# Prueba push

# Importación y procesado de datos
data_act3 <- read.csv("mubio02_act3_alimentos_nutrientes_4900.csv",
                      header = TRUE, stringsAsFactors = FALSE, sep = ",") %>%
  subset(select = -id)

# Convertir los valores se las columnas según el tipo de variable (numérica o categórica)
data_act3 <- data_act3 %>%
  mutate(ECV_prev = as.factor(sexo)) %>%
  mutate_at(vars(estado_civil:cancer_prev), as.factor)
data_act3 <- data_act3 %>%
  mutate_at(vars(altura:IMC), as.numeric) %>%
  mutate(edad = as.numeric(edad)) %>%
  mutate_at(vars(METs_h_semana:nutriente19), as.numeric)

alimentos_nutrientes <- data_act3[,27:ncol(data_act3)]

# Estandarización de datos antes de PCA
func_estandarizacion <- function(df) {
  # Aplicar la estandarización a cada columna
  df_estandarizado <- as.data.frame(lapply(df, function(col) {
    # Operación de normalización
    (col - mean(col, na.rm = TRUE)) / sd(col, na.rm = TRUE)
  }))
  return(df_estandarizado)
}

alimentos_nutrientes_st <- func_estandarizacion(alimentos_nutrientes)

# Extracción de los componentes principales
### OPCIÓN 1 ###
library(FactoMineR)
library(factoextra)
pca_result <- PCA(alimentos_nutrientes)
head(pca_result$eig) # comprobar los eigenvalores de cada componente principal

### OPCIÓN 2 ### ES LA QUE YO PONDRÍA
library(stats) # para realizar PCA
data_pca <- as.matrix(alimentos_nutrientes_st)
pca_result <- prcomp(t(data_pca), scale. = TRUE)
print(summary(pca_result)) # Muestra los valores de las componentes principales

# Evaluamos la adecuación de los datos para PCA según el criterio de KMO
### NO CONSIGO QUE FUNCIONE ESTO. LO PODEMOS QUITAR SI NO FUNCIONA
# library(psych)
# KMO(r = t(data_pca))
# print(kmo_result)  # Aceptable según el criterio de KMO


# Representación gráfica de los dos componentes principales de PCA
### OPCIÓN 1 ### ESTA ES LA ÚNICA QUE CONSIGO QUE ME QUEDE BIEN
plot(pca_result$x[,1], pca_result$x[,2]) # Representación de los dos componentes principales

### OPCIÓN 2 ###
plot <- fviz_pca_biplot(pca_result,
                        geom.ind = "point",
                        col.ind = "black",
                        pointsize = 2,
                        addEllipses = TRUE,
                        alpha.var = "contrib",
                        col.var = "contrib",
                        repel = TRUE,
)
plot

# Porcentaje de varianza para cada componente principal
pca.var <- pca_result$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per

# Obtenemos un "Scree plot" con la contribuición de los componentes principales
barplot(
  pca.var.per,main = "% varianza por componente principal",
  xlab = "Componente principal",
  ylab = "Porcentaje de varianza total")

# Rotación de cargas
cargas_pc1 <- pca_result$rotation[,1]
food_scores_pc1 <- abs(cargas_pc1)
food_scores_pc1_sorted <- sort(food_scores_pc1, decreasing = TRUE)
food_scores_pc1_top <- food_scores_pc1_sorted[1:10]
print(food_scores_pc1_top)

cargas_pc2 <- pca_result$rotation[,2]
food_scores_pc2 <- abs(cargas_pc2)
food_scores_pc2_sorted <- sort(food_scores_pc2, decreasing = TRUE)
food_scores_pc2_top <- food_scores_pc2_sorted[1:10]
print(food_scores_pc2_top)

print(pca_result$rotation[,1]) # Cargas de cada variable para el componente 1
print(pca_result$rotation[,2]) # Cargas de cada variable para el componente 2


### La salida de la línea 47 sería para la tabla 2. podemos poner los 10 primeros componentes
### La rotación de cargas de las líneas 87 y 93 serían para la tabla 3

