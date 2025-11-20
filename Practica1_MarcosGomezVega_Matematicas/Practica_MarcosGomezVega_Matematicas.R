# +----------------------+
# | Carga de librerías   |
# +----------------------+
#install.packages(c("tidyverse",
#                  "skimr",
#                  "DataExplorer",
#                  "corrplot",
#                  "GGally",
#                  "readr",
#                  "skimr",
#                  "dplyr",
#                  "tidyr"))
library(tidyverse)
library(readr)
library(skimr)
library(DataExplorer)
library(corrplot)
library(GGally)
library(skimr)
library(dplyr)
library(tidyr)

# +----------------------------------+
# | Carga de datos y su inspección   |
# +----------------------------------+

df <- read_csv("PrediccionPrecioViviendas_Matematicas/train.csv")
View(df)
str(df)
summary(df)


# Eliminar la columna 'Id' que no aporta información relevante para el análisis
df <- df %>% select(-Id)
View(df)

# +--------------------------------------+
# | Limpieza y preparacion de los datos  |
# +--------------------------------------+

### 1. Identificación de Tipos de Variables

num_vars <- df %>% select(where(is.numeric)) %>% names()
cual_vars <- df %>% select(where(~!is.numeric(.))) %>% names()

print("Variables Cuantitativas (num_vars):")
print(num_vars)
print("Variables Cualitativas (cual_vars):")
print(cual_vars)

# ----------------------------------------------------------------------
### 2. Conteo y Preparación de Valores Faltantes (NA)

na_counts <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  filter(NA_Count > 0)


print("Variables con valores faltantes (NA_Count > 0):")
print(na_counts)

# ----------------------------------------------------------------------
### 3. Imputación Estratégica de Ausencia (NA -> "No" o 0)

# Definir la lista de variables cualitativas donde NA significa "No Aplica".
variables_ausencia <- c(
  "Alley",        # Acceso por callejón
  "BsmtQual",     # Calidad del sótano
  "BsmtCond",     # Condición del sótano
  "BsmtExposure", # Exposición del sótano
  "BsmtFinType1", # Tipo de acabado 1 del sótano
  "BsmtFinType2", # Tipo de acabado 2 del sótano
  "GarageType",   # Tipo de garaje
  "GarageFinish", # Acabado interior del garaje
  "GarageQual",   # Calidad del garaje
  "GarageCond",   # Condición del garaje
  "PoolQC",       # Calidad de la piscina
  "Fence",        # Calidad de la cerca
  "MiscFeature"   # Característica miscelánea
)

variables_a_imputar <- intersect(variables_ausencia, cual_vars)

df[variables_a_imputar] <- lapply(df[variables_a_imputar], function(columna) {
  ifelse(is.na(columna), "No", columna)
})

# Imputación para 'GarageYrBlt' con la media de la columna.
media_entera_garaje <- round(mean(df$GarageYrBlt, na.rm = TRUE))

df$GarageYrBlt <- ifelse(is.na(df$GarageYrBlt),
                         media_entera_garaje,
                         df$GarageYrBlt)

df$GarageYrBlt <- as.integer(df$GarageYrBlt)
View(df)

# ----------------------------------------------------------------------
### 4. Recalculo de Valores na

na_counts_restantes <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  filter(NA_Count > 0)

print("Variables restantes que aún contienen NA (necesitan otro manejo):")
print(na_counts_restantes)

# ----------------------------------------------------------------------
### 5. Eliminacción de las varibles restantes con Na ya que son fallos
df <- df %>%
  filter(if_all(all_of(na_counts_restantes$Variable), ~!is.na(.)))

View(df)

# Verificación que no quedan NA
na_counts_finales <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  filter(NA_Count > 0)

print("Verificación final: Variables con NA después de la limpieza:")
print(na_counts_finales)

# ----------------------------------------------------------------------
### 6. Detección de Variables Duplicadas

num_duplicados <- sum(duplicated(df))

if (num_duplicados > 0) {
  print(paste("Hay", num_duplicados, "filas duplicadas en el dataset."))
} else {
  print("No se encontraron filas duplicadas.")
}

# +--------------------------------------+
# | Análisis Exploratorio de Datos (EDA) |
# +--------------------------------------+

# 1. Analisis variable Objetivo 'SalePrice'
summary(df$SalePrice)

# Histograma de 'SalePrice'
hist(df$SalePrice, main = "Histograma de SalePrice",
     xlab = "SalePrice",
     col = "lightblue",
     border = "black")

# Boxplot para detectar outliers en 'SalePrice'
boxplot(df$SalePrice, main = "Boxplot de SalePrice",
        ylab = "SalePrice",
        col = "green")

# Densidad de 'SalePrice'
plot(density(df$SalePrice), main = "Densidad de SalePrice",
     xlab = "SalePrice",
     col = "blue",
     lwd = 2)

# ------------------------------------------------------
### 2. Análisis de Correlación entre Variables Numéricas


# Verificamos las variables que no son numeros


# HAcemos sustitituicion cualitativas


# Hacemos PCA para variables numéricas
# Para reduccion de dimesionlidad de muchas varibles