rm(list=ls())

library(dplyr)   
library(tidyr)
library(tidyverse)

# Importación y procesado de datos
data_act3 <- read.csv("mubio02_act3_alimentos_nutrientes_4900.csv",
                      header = TRUE, stringsAsFactors = TRUE, sep = ",") %>%
  subset(select = -id)

# Seleccionar las columnas relevantes (alimento y nutriente)
alimentos_nutrientes <- data_act3 %>% 
  select(starts_with("alimento"), starts_with("nutriente"))

# Asegurarse de que no hay valores nulos
any(is.na(alimentos_nutrientes))

# Realizo análisis PCA: center y scale hacen la normalización o estandarización, no hacía falta la función.
library(stats)
pca_result <- prcomp (alimentos_nutrientes, center=TRUE, scale = TRUE)

# Muestra los valores de las componentes principales
summary(pca_result)

# Evaluamos la adecuación de los datos para PCA según el criterio de KMO
install.packages("psych")
library(psych)

#Correlacion spearman para poder meter la matriz en el KMO. 
#Usamos Spearman porque sale mejor coeficiente de KMO (sale 0.8)
#Si se usa pearson sale 0.5 que no es apto para PCA.
corr_an <-cor(alimentos_nutrientes, method="spearman")
resultado_KMO <- KMO(r=corr_an)

#Grafico scree plot para ver qué factores aportan más.
install.packages("factoextra")
library(factoextra)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 15))

#Obtención resultados (factores PCA)
var <- get_pca_var(pca_result)
fviz_pca_var(pca_result, axes = c(1, 2), 
             col.var = "cos2", 
             gradient.cols=c("blue", "yellow", "red"))


# Gráfica de los dos componentes principales de PCA.
# Representación de los dos componentes principales.

plot(pca_result$x[,1], pca_result$x[,2]) 
plot <- fviz_pca_biplot(pca_result, axes = c(1,3),
                        geom.ind = "point",
                        col.ind = "black",
                        pointsize = 0.5,
                        addEllipses = TRUE,
                        alpha.var = "cos2",
                        col.var = "cos2"
)
plot

# Vamos a conocer la % de la varianza explicada por los componentes del pca:

pca.var <- pca_result$sdev^2
pca.var.porcentaje <- round(pca.var/sum(pca.var)*100, 1)
pca.var.porcentaje

#Consultamos las cargas de cada componente (%) para los 4 más importantes:
head((pca_result$rotation[,1])*100)

#Podemos hacer biplot (ya hecho arriba) de pca_result con los componentes principales.
# Vemos representadas las cargas de cada componente y por tanto su relación o influencia.

biplot (pca_result)

# Rotación de cargas

# Para pc1 qué alimentos/nutrientes contribuyen más a la loc de los datos en PCA.
# Es decir, extraigo las cargas del PC1.
cargas_pc1 <- pca_result$rotation[,1]
# Con abs le digo que me de el valor absoluto de las cargas.
cargaPC1_scores <- abs(cargas_pc1)
# Ordeno los alimentos/nutrientes (cargas) de mayor a menor 
cargaPC1_scores_ordenado <- sort(cargaPC1_scores, decreasing = TRUE)
# Saco los 10 alimentos/nutrientes que más aportan (top 10) para PC1:
top_10_PC1 <- names(cargaPC1_scores_ordenado [1:10])
# Visualización del top 10 alimentos/nutrientes para PC1:
top_10_PC1

# Repito para PC2
cargas_pc2 <- pca_result$rotation[,2]
# Con abs le digo que me de el valor absoluto de las cargas.
cargaPC2_scores <- abs(cargas_pc2)
# Ordeno los alimentos/nutrientes (cargas) de mayor a menor 
cargaPC2_scores_ordenado <- sort(cargaPC2_scores, decreasing = TRUE)
# Saco los 10 alimentos/nutrientes que más aportan (top 10) para PC1:
top_10_PC2 <- names(cargaPC2_scores_ordenado [1:10])
# Visualización del top 10 alimentos/nutrientes para PC1:
top_10_PC2

# Repito para PC3
cargas_PC3 <- pca_result$rotation[,3]
cargaPC3_scores <- abs(cargas_PC3)
cargaPC3_scores_ordenado <- sort(cargaPC3_scores, decreasing = TRUE)
top_10_PC3 <- names(cargaPC3_scores_ordenado [1:10])
top_10_PC3

# Repito para PC4
cargas_PC4 <- pca_result$rotation[,4]
cargaPC4_scores <- abs(cargas_PC4)
cargaPC4_scores_ordenado <- sort(cargaPC4_scores, decreasing = TRUE)
top_10_PC4 <- names(cargaPC4_scores_ordenado [1:10])
top_10_PC4

# Visualizamos los valores de las cargas del top 10 A/N que más aportan pero sin valor absoluto:
pca_result$rotation[top_10_PC1,1]
pca_result$rotation[top_10_PC2,2]
pca_result$rotation[top_10_PC3,3]
pca_result$rotation[top_10_PC4,4]

# AQUÍ YA TENEMOS LOS PC MÁS IMPORTANTES Y LOS ALIMENTOS Y NUTRIENTES MÁS INFLUYENTES. 
# YO CREO QUE NO HAY QUE HACER MÁS.

################################################################################
######################## ANALISIS DESCRIPTIVO ##################################
################################################################################

# Extraer los scores del PC1 y PC2 y añadirlo a nuestra base de datos 

alimentos_nutrientes$PC1 <- pca_result$x[,1]
alimentos_nutrientes$PC2 <- pca_result$x[,2]

# Calcular los puntos de corte (cuantiles) para PC1 y PC2

cut_points_PC1 <- quantile(alimentos_nutrientes$PC1, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
cut_points_PC2 <- quantile(alimentos_nutrientes$PC2, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

# Crear la variable de terciles para PC1 y para PC2

alimentos_nutrientes$Terciles_PC1 <- cut(alimentos_nutrientes$PC1, 
                                         breaks = cut_points_PC1, 
                                         labels = c("Tercil 1", "Tercil 2", "Tercil 3"), 
                                         include.lowest = TRUE)


alimentos_nutrientes$Terciles_PC2 <- cut(alimentos_nutrientes$PC2, 
                                         breaks = cut_points_PC2, 
                                         labels = c("Tercil 1", "Tercil 2", "Tercil 3"), 
                                         include.lowest = TRUE)

