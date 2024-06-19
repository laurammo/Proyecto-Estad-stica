library(dplyr)   
library(tidyr)
library(tidyverse)
library(stats)
library(psych)
library(factoextra)
library(gtsummary)
library(devtools)
library(gt)


data_act3 <- read.csv("mubio02_act3_alimentos_nutrientes_4900.csv",
                      header = TRUE, stringsAsFactors = TRUE, sep = ",") %>%
  subset(select = -id)

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
