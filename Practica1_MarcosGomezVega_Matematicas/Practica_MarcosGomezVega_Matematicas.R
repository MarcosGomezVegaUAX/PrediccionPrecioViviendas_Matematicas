# Cargar las librerías necesarias
library(tidyverse)
library(readr)
# Cargar el conjunto de datos   
df <- read_csv("PrediccionPrecioViviendas_Matematicas/train.csv")

# Inspeccionar las primeras filas del conjunto de datos
head(df)
# Resumen estadístico del conjunto de datos
str(df)
# Visualizar un resumen estadístico detallado
summary(df)

# Visualizar la distribución de la variable objetivo 'SalePrice'
hist(df$SalePrice)