rm(list=ls())


# Librerías necesarias

library(nortest)
library(dplyr)   
library(tidyr)
library(tidyverse)
library(stats)
library(psych)
library(factoextra)
library(gtsummary)
library(devtools)
library(gt)

rm(list=ls())

# Librerías necesarias

library(nortest)
library(dplyr)   
library(tidyr)
library(tidyverse)
library(stats)
library(psych)
library(factoextra)
library(gtsummary)
library(devtools)
library(gt)

#############################################################################
###################### PRUEBA DE NORMALIDAD #################################
#############################################################################

# Importar los datos 

data_act3 <- read.csv("mubio02_act3_alimentos_nutrientes_4900.csv",
                      header = TRUE, stringsAsFactors = TRUE, sep = ",") %>%
  subset(select = -id)

# Comprobamos la normalidad de los datos usando el test de Anderson-Darling realizando un bucle.

pvalor_nutrientes <- matrix(NA, nrow = 19, ncol = 1) # Matriz vacía con 19 filas porque tenemos 19 variables y una columna
columnas_nutrientes <- c("nutriente1", "nutriente2", "nutriente3", "nutriente4", "nutriente5",
                       "nutriente6", "nutriente7", "nutriente8", "nutriente9", "nutriente10", 
                       "nutriente11", "nutriente12", "nutriente13", "nutriente14", "nutriente15", 
                       "nutriente16", "nutriente17", "nutriente18", "nutriente19")

for (i in 1:length(columnas_nutrientes)) {
  ad_test_result_nutrientes <- ad.test(data_act3[[columnas_nutrientes[i]]])
  pvalor_nutrientes[i, ] <- ad_test_result_nutrientes$p.value
}

pvalor_nutrientes #p-valor <0.001 = no cumple la normalidad, ni de manera conjunta ni separada


pvalor_alimentos <- matrix(NA, nrow = 131, ncol = 1) # Matriz vacía con 20 filas porque tenemos 20 variables y una columna
columnas_alimentos <- paste0("alimento", 1:131)

for (i in 1:length(columnas_alimentos)) {
  ad_test_result_alimentos <- ad.test(data_act3[[columnas_alimentos[i]]])
  pvalor_alimentos[i, ] <- ad_test_result_alimentos$p.value
}

pvalor_alimentos #p-valor <0.001 = no cumple la normalidad, ni de manera conjunta ni separada


#############################################################################
###################### ANALISIS PCA #########################################
#############################################################################

# Seleccionar las columnas relevantes (alimento y nutriente)

alimentos_nutrientes <- data_act3 %>% 
  select(starts_with("alimento"), starts_with("nutriente"))

# Comprobar si existen valores nulos

any(is.na(alimentos_nutrientes)) 

# Análisis de PCA + estandarización de los datos mediante la función scale

pca_result <- prcomp (alimentos_nutrientes, center=TRUE, scale = TRUE)

# Visualizar los resultados de los componentes

summary(pca_result)

# Evaluamos la adecuación de los datos para PCA según el criterio de KMO
# Correlacion spearman para poder meter la matriz en el KMO. 
# Usamos Spearman porque sale mejor coeficiente de KMO (sale 0.86)

corr_an <-cor(alimentos_nutrientes, method="spearman")
resultado_KMO <- KMO(r=corr_an)
resultado_KMO

#Grafico scree plot para ver qué factores aportan más.
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 15))

# Visualizar la importancia de cada variable por dimensiones

fviz_contrib(pca_result, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_result, choice = "var", axes = 2, top = 10)

# Visualizar el gráfico de correlación variable (muestra las relaciones entre todas las variables)

var <- get_pca_var(pca_result) # varianza

fviz_pca_var(pca_result, axes = c(1, 2), 
             col.var = "cos2", 
             gradient.cols=c("blue", "yellow", "red"))


# Gráfica de los dos componentes principales de PCA.
# Representación de los dos componentes principales.

plot_1 <- plot(pca_result$x[,1], pca_result$x[,2])

PCA_Biplot <- fviz_pca_biplot(pca_result, axes = c(1,2),
                        geom.ind = "point",
                        col.ind = "black",
                        pointsize = 0.5,
                        addEllipses = TRUE,
                        alpha.var = "cos2",
                        col.var = "cos2"
)

PCA_Biplot

# % de la varianza explicada por los componentes del pca:

pca.var <- pca_result$sdev^2
pca.var.porcentaje <- round(pca.var/sum(pca.var)*100, 1)
pca.var.porcentaje

#Consultamos las cargas de cada componente (%) para los 4 más importantes:

head((pca_result$rotation[,1]))

# Podemos hacer biplot (ya hecho arriba) de pca_result con los componentes principales.
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


# Visualizamos los valores de las cargas del top 10 A/N que más aportan pero sin valor absoluto:

pca_result$rotation[top_10_PC1,1]
pca_result$rotation[top_10_PC2,2]

# Seleccionar las columnas relevantes (alimento y nutriente)

alimentos_nutrientes <- data_act3 %>% 
  select(starts_with("alimento"), starts_with("nutriente"))

# Comprobar si existen valores nulos

any(is.na(alimentos_nutrientes)) 

# Análisis de PCA + estandarización de los datos mediante la función scale

pca_result <- prcomp (alimentos_nutrientes, center=TRUE, scale = TRUE)

################################################################################
####################---- REGRESIÓN LOGÍSTICA ---- ##############################
################################################################################

# Extraer los scores del PC1 y PC2 y añadirlo a nuestra base de datos 

data_act3$PC1 <- pca_result$x[,1]
data_act3$PC2 <- pca_result$x[,2]

# Calcular los puntos de corte (cuantiles) para PC1 y PC2

cut_points_PC1 <- quantile(data_act3$PC1, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
cut_points_PC2 <- quantile(data_act3$PC2, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

# Crear la variable de terciles para PC1 y para PC2

data_act3$Terciles_PC1 <- cut(data_act3$PC1, 
                              breaks = cut_points_PC1, 
                              labels = c("Tercil 1", "Tercil 2", "Tercil 3"), 
                              include.lowest = TRUE)


data_act3$Terciles_PC2 <- cut(data_act3$PC2, 
                              breaks = cut_points_PC2, 
                              labels = c("Tercil 1", "Tercil 2", "Tercil 3"), 
                              include.lowest = TRUE)
#### ---- Ver los NA ---- ####

colSums(is.na(data_act3)) # sumatorio de NA es mi data frame
data <- data_act3[complete.cases(data_act3),] #creo un data frame eliminando los NA con complete.cases
any(is.na(data)) # Me aseguro de que se han borrado los NA

#### ---- Convertir a factor las variables categóricas ---- ####

data$diab_prev <- as.factor(data$diab_prev) 
data$Terciles_PC1 <- as.factor(data$Terciles_PC1)
data$Terciles_PC2 <- as.factor(data$Terciles_PC2)
data$hipercolesterolemia <- as.factor(data$hipercolesterolemia)
data$hipertrigliceridemia <- as.factor(data$hipertrigliceridemia)
data$tabaco <- as.factor(data$tabaco)

####Modelo de regresión logística####

modelo_logistica <- glm(diab_prev ~ Terciles_PC1 + Terciles_PC2 + hipertrigliceridemia + hipercolesterolemia + tabaco, data= data, family = "binomial")
summary(modelo_logistica) # tenemos las betas o coeficientes, necesitamos las odds ratios y el intervalo de confianza. # el valor de p no cambia si lo exponenciamos.
or_confint <- confint(modelo_logistica) # intervalo de confianza

or_model <- exp(coef(modelo_logistica)) # las OR 
or_confident <- exp(confint(modelo_logistica)) # intervalo de confianza de las OR añadiendo coeficientes

# OR = 1 (EFECTO NULO)
# OR < 1 (PROTECCIÓN/DISMINUCIÓN DEL RIESGO)
# OR > 1 (INCREMENTO DEL RIESGO O MAYOR PROBABILIDAD)

# Calidad del modelo: r2

pseudo_R2_McFadden <- 1 - (modelo_logistica$deviance / modelo_logistica$null.deviance)
cat("Pseudo R-cuadrado de McFadden:", pseudo_R2_McFadden, "\n")
