# Proyecto-Estadistica
## Análisis de una base de datos de alimentos y nutrientes con respecto al consumo y prevalencia de enfermedades 
Exploramis una base de datos que contiene tanto información sobre datos descriptivos, consumo de alimentos y nutrientes como la prevalencia de alguna enfermedad (por ejemplo, diabetes, obesidad o enfermedad cardiovascular).
### PCA de los datos de alimentos y nutrientes
Utilizamos la librería correspondiente para realizar el PCA de los datos y los estandarizarmos antes de aplicar el PCA para que todas las variables tengan la misma escala.
### Gráficos descriptivos de los componentes principales
Representamos visualmente los resultados del PCA, incluyendo gráficos descriptivos o heatmaps de las cargas por componentes que aporten información relevante a los resultados. 
### Tabla descriptiva con las variables más importantes
Incluímos las variables sociodemográficas y descriptivas originales por componentes del PCA.
### Modelo de regresión logística
Construimos el modelo de regresión logística, donde la variable objetivo es la prevalencia de la enfermedad de diabetes y las variables predictoras son los terciles o cuartiles de los componentes principales obtenidos del PCA y otras variables de ajuste relevante. 
### Análisis de los resultados 
Analizamos el modelo de regresión logística obtenido, evaluando la calidad, además de las funciones específicas para sacar los parámetros (coeficientes exponenciados, IC 95 %, valores p) de cada variable.

