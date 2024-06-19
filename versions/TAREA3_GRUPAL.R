
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

# varianza por dimensión + eigenvalues
head(get_eigenvalue(pca_result))

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

head(pca_result$rotation)[,1:2]
pca_result$rotation[,1:2]

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


################################################################################
######################## ANALISIS DESCRIPTIVO ##################################
################################################################################

# Extraer los scores del PC1 y PC2 y añadirlo a nuestra base de datos 

alimentos_nutrientes$PC1 <- pca_result$x[,1]
alimentos_nutrientes$PC2 <- pca_result$x[,2]

# Calcular los puntos de corte (terciles) para PC1 y PC2

cut_points_PC1 <- quantile(alimentos_nutrientes$PC1, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
cut_points_PC2 <- quantile(alimentos_nutrientes$PC2, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

# Crear la variable de terciles para PC1 y para PC2

alimentos_nutrientes$PC1 <- cut(alimentos_nutrientes$PC1, 
                                breaks = cut_points_PC1, 
                                labels = c("Tercil 1", "Tercil 2", "Tercil 3"), 
                                include.lowest = TRUE)


alimentos_nutrientes$PC2 <- cut(alimentos_nutrientes$PC2, 
                                breaks = cut_points_PC2, 
                                labels = c("Tercil 1", "Tercil 2", "Tercil 3"), 
                                include.lowest = TRUE)

# Del df alimentos_nutrientes nos quedamos las columnas de interés (variables sociodemograficas).
# También cogemos las columnas que contienen las etiquetas de los terciles.

columnas <- data_act3 %>% select(c(1:26))
tercilescolumnas <- alimentos_nutrientes %>% select(c("PC1", "PC2"))

# Juntar en un df nuevo todas las columnas

df_tabla <- cbind(columnas, tercilescolumnas)

# Juntar en un nuevo df (df_tablaPC) las columnas con los terciles.

df_tablaPC <- df_tabla %>%
  pivot_longer(cols = c(PC1, PC2), values_to = "PC")


# Hacemos la tabla.

select_gtsummary <- df_tablaPC %>% select(c(1:12, 19:28))

tabla_descriptiva <- select_gtsummary %>%
  tbl_strata(strata = name,
             .tbl_fun = ~.x %>%
               tbl_summary(by=PC,
                           label = list(
                             sexo ~ "sexo",
                             edad ~ "Edad (años)"),
                           percent = "row",
                           digits = list(edad ~ 1),
                           statistic = list (sexo ~ "{n}/{N} ({p}%)", 
                                             edad ~ "{median} ({p25}-{p75})"),
                           type=list(sexo ~ "categorical")) %>%
               add_p()) %>%
  as_gt()

tabla_descriptiva

gt::gtsave(tabla_descriptiva, "/Users/Usuario/Desktop/BIOINFORMATICA/PRIMER CUATRI/ESTADISTICA EN R/EJERCICIOS/EJERICICIO 3/tabla.png")


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
data$sexo <- as.factor(data$sexo)
data$edad <- as.factor(data$edad)
data$verdura <- as.factor(data$verdura)
data$carnes <- as.factor(data$carnes)

#### ---- Modelo de regresión logística 1 ---- ####


modelo_logistica <- glm(diab_prev ~ Terciles_PC1 + Terciles_PC2 + hipertrigliceridemia + hipercolesterolemia + tabaco, data= data, family = "binomial")
summary(modelo_logistica) # tenemos las betas o coeficientes, necesitamos las odds ratios y el intervalo de confianza. # el valor de p no cambia si lo exponenciamos.
confint(modelo_logistica) # intervalo de confianza

exp(coef(modelo_logistica)) # las OR 
exp(confint(modelo_logistica)) # intervalo de confianza de las OR añadiendo coeficientes

# Los coeficientes exponenciales son las OR. 

# OR = 1 (EFECTO NULO)
# OR < 1 (PROTECCIÓN/DISMINUCIÓN DEL RIESGO)
# OR > 1 (INCREMENTO DEL RIESGO O MAYOR PROBABILIDAD)

#### ---- Evaluar la calidad del modelo ---- ####

pseudo_R2_McFadden <- 1 - (modelo_logistica$deviance / modelo_logistica$null.deviance)
cat("Pseudo R-cuadrado de McFadden:", pseudo_R2_McFadden, "\n")

#### ---- Modelo de regresión logística 2 ---- ####

modelo_logistica_2 <- glm(diab_prev ~ Terciles_PC1 + Terciles_PC2 + edad + sexo + hipertrigliceridemia + hipercolesterolemia + tabaco, data= data, family = "binomial")
summary(modelo_logistica_2) 
confint(modelo_logistica_2) 

exp(coef(modelo_logistica_2)) 
exp(confint(modelo_logistica_2)) 

pseudo_R2_McFadden_2 <- 1 - (modelo_logistica_2$deviance / modelo_logistica_2$null.deviance)
cat("Pseudo R-cuadrado de McFadden:", pseudo_R2_McFadden_2, "\n")

#### ---- Modelo de regresión logística 3 ---- ####


modelo_logistica_3 <- glm(diab_prev ~ Terciles_PC1 + Terciles_PC2 + edad + sexo + hipertrigliceridemia + hipercolesterolemia + tabaco + verdura + carnes, data= data, family = "binomial")
summary(modelo_logistica_3) 
confint(modelo_logistica_3) 

exp(coef(modelo_logistica_3))
exp(confint(modelo_logistica_3)) 


pseudo_R2_McFadden_3 <- 1 - (modelo_logistica_3$deviance / modelo_logistica_3$null.deviance)
cat("Pseudo R-cuadrado de McFadden:", pseudo_R2_McFadden_3, "\n")

# Tabla con los resultados

modelo1 <- tbl_regression(modelo_logistica,
                          exponentiate = TRUE,
                          add_estimate_to_reference_rows = TRUE) %>%
  modify_header(label ~ "Variable") %>%
  modify_caption("**OR and 95% CI**") 

modelo1

# Imprimir la tabla
print(modelo1)


modelo2 <- tbl_regression(modelo_logistica_2,
                          exponentiate = TRUE,
                          add_estimate_to_reference_rows = TRUE) %>%
  modify_header(label ~ "Variable") %>%
  modify_caption("**OR and 95% CI**") 

print(modelo2)


modelo3 <- tbl_regression(modelo_logistica_3,
                          exponentiate = TRUE,
                          add_estimate_to_reference_rows = TRUE) %>%
  modify_header(label ~ "Variable") %>%
  modify_caption("**OR and 95% CI**") 

print(modelo3)


tbl_merge <- tbl_merge(list(modelo1, modelo2, modelo3),
                       tab_spanner = c("**Multivariable model 1**", "**Multivariable model 2**", "**Multivariable model 3**")) %>%
  as_gt() 

gt::gtsave(tbl_merge, "/Users/Usuario/Desktop/BIOINFORMATICA/PRIMER CUATRI/ESTADISTICA EN R/EJERCICIOS/EJERICICIO 3/modeloRegresion.png")

