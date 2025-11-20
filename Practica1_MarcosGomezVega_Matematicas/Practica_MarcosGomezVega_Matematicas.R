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

contar_nas <- function(data) {
  na_counts <- data %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Variable",
                 values_to = "NA_Count") %>%
  filter(NA_Count > 0)

  na_counts
}

na_counts_iniciales <- contar_nas(df)

print("Variables con valores faltantes (NA_Count > 0):")
print(na_counts_iniciales)

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
  "FireplaceQu",  # Calidad de la chimenea
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

na_counts_iniciales <- contar_nas(df)

print("Variables con valores faltantes (NA_Count > 0):")
print(na_counts_iniciales)

# ------------------------------------------------------
### 5. Sustituimos variables con muchas lineas

# Sustituimos las lineas de LotFrontage que sean NA por la media
media_lotfrontage <- round(mean(df$LotFrontage, na.rm = TRUE))
df$LotFrontage <- ifelse(is.na(df$LotFrontage),
                         media_lotfrontage,
                         df$LotFrontage)

na_counts_iniciales <- contar_nas(df)

print("Variables con valores faltantes (NA_Count > 0):")
print(na_counts_iniciales)

# ----------------------------------------------------------------------
### 6. Eliminacción de las varibles restantes con Na ya que son fallos
df <- df %>%
  filter(if_all(all_of(na_counts_restantes$Variable), ~!is.na(.)))

View(df)

# Verificamos que ya no hay NA
na_counts_iniciales <- contar_nas(df)

print("Variables con valores faltantes (NA_Count > 0):")
print(na_counts_iniciales)

# ----------------------------------------------------------------------
### 7. Detección de Variables Duplicadas

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
### 2. Como SalePrice esta sesgado a la derecha, aplicamos logaritmo
df$Log_SalePrice <- log(df$SalePrice)
summary(df$Log_SalePrice)
# Histograma de 'Log_SalePrice'
hist(df$Log_SalePrice, main = "Histograma de Log_SalePrice",
     xlab = "Log_SalePrice",
     col = "lightcoral",
     border = "black")
# Boxplot para detectar outliers en 'Log_SalePrice'
boxplot(df$Log_SalePrice, main = "Boxplot de Log_SalePrice",
        ylab = "Log_SalePrice",
        col = "orange")
# Densidad de 'Log_SalePrice'
plot(density(df$Log_SalePrice), main = "Densidad de Log_SalePrice",
     xlab = "Log_SalePrice",
     col = "red",
     lwd = 2)

# ------------------------------------------------------
### 3. Análisis de Correlación entre Variables Numéricas

print(num_vars)

cor_matrix <- cor(df[num_vars], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 90,
         title = "Matriz de Correlación entre Variables Numéricas",
         mar = c(0, 0, 1, 0))

# Los 15 valores con mayor correlacion con SalePrice execptuando SalePrice
cor_saleprice <- cor_matrix[, "SalePrice"]
cor_saleprice_sorted <- sort(cor_saleprice, decreasing = TRUE)
print("Top 15 variables con mayor correlación con SalePrice:")
print(head(cor_saleprice_sorted[-1], 10))

best_10_cor_vars <- names(head(cor_saleprice_sorted[-1], 10))
print(best_10_cor_vars)
# ------------------------------------------------------
### 4.Verificar si la relacion entre variables numericas y SalePrice es lineal
# Graficos de dispersion de las 5 variables con mayor correlacion con SalePrice

# Diagrama de dispersion de OverallQual vs SalePrice
plot(df[[best_10_cor_vars[1]]], df$SalePrice,
     main = paste("Scatterplot de", best_10_cor_vars[1], "vs SalePrice"),
     xlab = best_10_cor_vars[1],
     ylab = "SalePrice",
     col = "blue",
     pch = 19)

# Diagrama de dispersion de GrLiveArea vs SalePrice
plot(df[[best_10_cor_vars[2]]], df$SalePrice,
     main = paste("Scatterplot de", best_10_cor_vars[2], "vs SalePrice"),
     xlab = best_10_cor_vars[2],
     ylab = "SalePrice",
     col = "blue",
     pch = 19)

# Diagrama de dispersion de GarageCars vs SalePrice
plot(df[[best_10_cor_vars[3]]], df$SalePrice,
     main = paste("Scatterplot de", best_10_cor_vars[3], "vs  SalePrice"),
     xlab = best_10_cor_vars[3],
     ylab = "SalePrice",
     col = "blue",
     pch = 19)

# Diagrama de dispersion de GarageArea vs SalePrice
plot(df[[best_10_cor_vars[4]]], df$SalePrice,
     main = paste("Scatterplot de", best_10_cor_vars[4], "vs SalePrice"),
     xlab = best_10_cor_vars[4],
     ylab = "SalePrice",
     col = "blue",
     pch = 19)

# Diagrama de dispersion de TotalBsmtSF vs SalePrice
plot(df[[best_10_cor_vars[5]]], df$SalePrice,
     main = paste("Scatterplot de", best_10_cor_vars[5], "vs SalePrice"),
     xlab = best_10_cor_vars[5],
     ylab = "SalePrice",
     col = "blue",
     pch = 19)

# En los graficos de GrLivArea, GarageArea y TotalBsmtSF
# se observan algunos puntos atipicos que pueden afectar la linealidad

#------------------------------------------------------
### 5. Tratamiento de Outliers en Variables Numéricas

# Eliminamos los puntos atipicos de las variables con mayor correlacion
# Definimos una función para eliminar outliers el rango intercuartílico (IQR)
eliminar_outliers_iqr <- function(data, variable) {

  n_initial <- nrow(data)
  q1 <- quantile(data[[variable]], 0.25)
  q3 <- quantile(data[[variable]], 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  data_filtered <- data %>%
    filter(data[[variable]] >= lower_bound & data[[variable]] <= upper_bound)

  n_final <- nrow(data_filtered)
  n_removed <- n_initial - n_final

  cat("Limpieza en:", variable, "\n")
  cat("  -> Filas eliminadas:", n_removed, "\n")
  cat("  -> Filas restantes:", n_final, "\n\n")

  data_filtered
}

variables_a_tratar <- c("OverallQual",
                        "GarageCars",
                        "GrLivArea",
                        "GarageArea",
                        "TotalBsmtSF")

for (var in variables_a_tratar) {
  df <- eliminar_outliers_iqr(df, var)
}
View(df)

# ------------------------------------------------------
### 6 Codficacion one-hot para variable cualitativas categoricas
print(cual_vars)

list_one_hot_vars <- c("MSZoning",
                       "Street",
                       "Alley",
                       "LotConfig",
                       "Neighborhood",
                       "Condition1",
                       "Condition2",
                       "BldgType",
                       "HouseStyle",
                       "RoofStyle",
                       "RoofMatl",
                       "Exterior1st",
                       "Exterior2nd",
                       "MasVnrType",
                       "Foundation",
                       "Heating",
                       "Electrical",
                       "Functional",
                       "GarageType",
                       "PavedDrive",
                       "MiscFeature",
                       "SaleType",
                       "SaleCondition")

list_one_hot_validated <- intersect(list_one_hot_vars, cual_vars)

df[list_one_hot_validated] <- lapply(df[list_one_hot_validated], factor)

View(df)

# Excluir variables con menos de 2 niveles después de la limpieza
variables_a_excluir <- c()

for (var in list_one_hot_validated) {
  num_levels <- length(levels(df[[var]]))
  if (num_levels < 2) {
    cat("EXCLUIDA: La variable '", var,
        "' tiene solo ", num_levels, " nivel(es) restante(s).\n")
    variables_a_excluir <- c(variables_a_excluir, var)
  }
}

if (length(variables_a_excluir) > 0) {
  list_one_hot_final <- setdiff(list_one_hot_validated, variables_a_excluir)
} else {
  list_one_hot_final <- list_one_hot_validated
}
print("\nLista final para One-Hot Encoding:")
print(list_one_hot_final)

# Generar la fórmula para model.matrix
formula_one_hot_final <- as.formula(paste("~",
                                          paste(list_one_hot_final,
                                                collapse = " + ")))
# Crear la matriz dummy sin la columna intercepto
dummy_matrix <- model.matrix(formula_one_hot_final, data = df)[, -1]
# Eliminar las variables cualitativas originales del DataFrame
df_processed <- df %>% select(-all_of(list_one_hot_final))
# Combinar el DataFrame procesado con la matriz dummy
df_encoded <- cbind(df_processed, as.data.frame(dummy_matrix))

print("Dimensiones del DataFrame original:")
print(dim(df))
print("Dimensiones del DataFrame procesado (sin variables cualitativas):")
print(dim(df_processed))
print("Dimensiones de la matriz dummy generada:")
print(dim(dummy_matrix))
View(df_encoded)

#------------------------------------------------------
### 7. Codificacion ordinal para variables cualitativas ordinales

list_ordinal_codification <- setdiff(cual_vars, list_one_hot_final)
print("Variables Categóricas Ordinales (Requieren Mapeo Numérico Manual):\n")
print(list_ordinal_codification)

# Mapeos manuales para variables ordinales
mapeo_ordinal <- list(
  LotShape = c("Reg" = 4, "IR1" = 3, "IR2" = 2, "IR3" = 1, "No" = 0),
  LandContour = c("Lvl" = 4, "Bnk" = 3, "HLS" = 2, "Low" = 1, "No" = 0),
  Utilities = c("AllPub" = 4, "NoSewr" = 3, "NoSeWa" = 2, "ELO" = 1, "No" = 0),
  LandSlope = c("Gtl" = 3, "Mod" = 2, "Sev" = 1, "No" = 0),
  ExterQual = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0),
  ExterCond = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0),
  BsmtQual = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0),
  BsmtCond = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0),
  BsmtExposure = c("Gd" = 4, "Av" = 3, "Mn" = 2, "No" = 1, "No" = 0),
  BsmtFinType1 = c("GLQ" = 6, "ALQ" = 5, "BLQ" = 4, "Rec" = 3,
                   "LwQ" = 2, "Unf" = 1, "No" = 0),
  BsmtFinType2 = c("GLQ" = 6, "ALQ" = 5, "BLQ" = 4, "Rec" = 3,
                   "LwQ" = 2, "Unf" = 1, "No" = 0),
  HeatingQC = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0),
  CentralAir = c("Y" = 1, "N" = 0, "No" = 0),
  KitchenQual = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0),
  FireplaceQu = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0),
  GarageFinish = c("Fin" = 3, "RFn" = 2, "Unf" = 1, "No" = 0),
  GarageQual = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0),
  GarageCond = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0),
  PoolQC = c("Ex" = 4, "Gd" = 3, "TA" = 2, "Fa" = 1, "No" = 0),
  Fence = c("GdPrv" = 4, "MnPrv" = 3, "GdWo" = 2, "MnWw" = 1, "No" = 0)
)

variables_con_mapeo <- names(mapeo_ordinal)
list_ordinal_final <- intersect(list_ordinal_codification, variables_con_mapeo)

for (var in list_ordinal_final) {
  mapa_actual <- mapeo_ordinal[[var]]
  etiquetas <- names(mapa_actual)
  valores <- unname(mapa_actual)

  df_encoded[[var]] <- valores[match(df_encoded[[var]], etiquetas)]
  df_encoded[[var]] <- as.numeric(df_encoded[[var]])

}
View(df_encoded)

#------------------------------------------------------
### 8. Estandarizamos toas las variables numericas

# Comprobar que todo el dataframe es numerico
str(df_encoded)

# Estandarizamos con Z-score todas las variables
vars_a_estandarizar <- setdiff(names(df_encoded),
                               c("SalePrice", "Log_SalePrice"))
df_encoded[vars_a_estandarizar] <- scale(df_encoded[vars_a_estandarizar])
View(df_encoded)
summary(df_encoded)

# +---------------------------------------------+
# | Análisis de Componentes Principales (PCA)   |
# +---------------------------------------------+
