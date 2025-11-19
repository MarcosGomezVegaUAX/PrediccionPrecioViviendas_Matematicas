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

# Eliminar la columna 'Id' que no aporta información relevante para el análisis
df <- df %>% select(-Id)

# Visualizar la distribución de la variable objetivo 'SalePrice'
hist(df$SalePrice)

# Calcular estadísticas básicas de 'SalePrice'
mean(df$SalePrice)
median(df$SalePrice)
sd(df$SalePrice)

# +----------------------------------------+
# |                                        |
# | Analisis Exploratorio de datos (EDA)   |
# |                                        |
# +----------------------------------------+

# Dado que el histograma de 'SalePrice' muestra una distribución sesgada a la derecha,
# aplicamos una transformación logarítmica para normalizar la distribución
df <- df %>% mutate(LogSalePrice = log(SalePrice))
hist(df$LogSalePrice)

# Limpiamos el conjunto de datos 
# 1. Identificar y contar valores faltantes (NA) por columna
# Se usa summarise(across(...)) para aplicar la función sum(is.na(.)) a todas las columnas
na_counts <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%  
  filter(NA_Count > 0) %>% 
  arrange(desc(NA_Count)) 

# Mostrar las variables con valores faltantes
print(na_counts)