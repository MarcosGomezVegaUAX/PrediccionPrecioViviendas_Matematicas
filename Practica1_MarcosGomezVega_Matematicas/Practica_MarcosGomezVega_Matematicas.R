# +----------------------+
# | Carga de librerías   |
# +----------------------+

# install.packages(c("tidyverse",
#                    "skimr",
#                    "DataExplorer",
#                    "corrplot",
#                    "GGally",
#                    "readr",
#                    "skimr",
#                    "dplyr",
#                    "tidyr",
#                    "stats",
#                    "factoextra",
#                    "glmnet"))
library(tidyverse)
library(readr)
library(skimr)
library(DataExplorer)
library(corrplot)
library(GGally)
library(skimr)
library(dplyr)
library(tidyr)
library(stats)
library(factoextra)
library(glmnet)

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

# Analisis variable Objetivo 'SalePrice'
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


# Como SalePrice esta sesgado a la derecha, aplicamos logaritmo
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

na_counts_restantes <- contar_nas(df)

print("Variables con valores faltantes (NA_Count > 0):")
print(na_counts_restantes)

# ------------------------------------------------------
### 5. Sustituimos variables con muchas lineas

# Sustituimos las lineas de LotFrontage que sean NA por la media
media_lotfrontage <- round(mean(df$LotFrontage, na.rm = TRUE))
df$LotFrontage <- ifelse(is.na(df$LotFrontage),
                         media_lotfrontage,
                         df$LotFrontage)

na_counts_restantes <- contar_nas(df)

print("Variables con valores faltantes (NA_Count > 0):")
print(na_counts_restantes)

# ----------------------------------------------------------------------
### 6. Eliminacción de las varibles restantes con Na ya que son fallos
df <- df %>%
  filter(if_all(all_of(na_counts_restantes$Variable), ~!is.na(.)))

View(df)

# Verificamos que ya no hay NA
na_counts_restantes <- contar_nas(df)

print("Variables con valores faltantes (NA_Count > 0):")
print(na_counts_restantes)

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

### 1. Análisis de Correlación entre Variables Numéricas

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

### 2.Verificar si la relacion entre variables numericas y SalePrice es lineal
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
### 3. Tratamiento de Outliers en Variables Numéricas

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
### 4 Codficacion one-hot para variable cualitativas categoricas
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
### 5. Codificacion ordinal para variables cualitativas ordinales

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
### 6. Estandarizamos toas las variables numericas

# Comprobar que todo el dataframe es numerico
str(df_encoded)

# Estandarizamos con Z-score todas las variables
vars_a_estandarizar <- setdiff(names(df_encoded),
                               c("SalePrice", "Log_SalePrice"))
df_encoded[vars_a_estandarizar] <- scale(df_encoded[vars_a_estandarizar])
View(df_encoded)
summary(df_encoded)
dim(df_encoded)

# +-------------------------------------------------------------+
# | División de los datos en conjunto de entrenamiento y prueba |
# +-------------------------------------------------------------+

# Dividmoes el dataset en conjunto de entrenamiento 60%
# prueba 20% y validacion 20%
set.seed(123)
n <- nrow(df_encoded)
train_indices <- sample(1:n, size = 0.6 * n)
remaining_indices <- setdiff(1:n, train_indices)
test_indices <- sample(remaining_indices,
                       size = 0.5 * length(remaining_indices))
val_indices <- setdiff(remaining_indices, test_indices)


print(paste("Tamaño del conjunto de entrenamiento:",
            length(train_indices)))
print(paste("Tamaño del conjunto de prueba:",
            length(test_indices)))
print(paste("Tamaño del conjunto de validación:",
            length(val_indices)))

# Creamos los conjuntos de datos
train_data <- df_encoded[train_indices, ]
test_data <- df_encoded[test_indices, ]
val_data <- df_encoded[val_indices, ]

print("Dimensiones del conjunto de entrenamiento:")
print(dim(train_data))
print("Dimensiones del conjunto de prueba:")
print(dim(test_data))
print("Dimensiones del conjunto de validación:")
print(dim(val_data))

# +-----------------------------------------------------------------+
# | Filtro de Variables con Varianza Cero (Constantes) antes de PCA |
# +-----------------------------------------------------------------+

# Inicializar los conjuntos finales
df_train_final <- train_data
df_test_final <- test_data
df_val_final <- val_data

predict_vars <- setdiff(names(df_train_final), c("SalePrice", "Log_SalePrice"))

unique_counts <- sapply(df_train_final[, predict_vars],
                        function(x) length(unique(x)))

zero_variance_vars <- names(unique_counts[unique_counts <= 1])

print("Variables con varianza cero en el conjunto de entrenamiento:")
print(zero_variance_vars)

if (length(zero_variance_vars) > 0) {
  vars_to_keep <- setdiff(names(df_train_final), zero_variance_vars)
  df_train_final <- df_train_final %>% select(all_of(vars_to_keep))
  df_test_final <- df_test_final %>% select(all_of(vars_to_keep))
  df_val_final <- df_val_final %>% select(all_of(vars_to_keep))

  print("Dimensiones después de eliminar variables con varianza cero:")
  print(length(zero_variance_vars))
} else {
  print("No se encontraron variables con varianza cero.")
}
print("Dimensiones de entrenamiento final:")
print(dim(df_train_final))
print("Dimensiones de prueba final:")
print(dim(df_test_final))
print("Dimensiones de validación final:")
print(dim(df_val_final))

# +---------------------------------------------+
# | Análisis de Componentes Principales (PCA)   |
# +---------------------------------------------+

# 1. Usamos la función prcomp para realizar PCA

respca <- prcomp(df_train_final %>% select(-SalePrice, -Log_SalePrice),
                 center = TRUE,
                 scale. = TRUE)
names(respca)
head(respca$rotation)[, 1:5]
dim(respca$rotation)
head(respca$x)[, 1:5]
respca$sdev
respca ~ sdev^2
summary(respca)


# Nos quedamos con los 90 primeros componentes principales
# ya que estan muy dispersosn y la proporcion acumuluda de varianza
# es de un 86.329%

# ----------------------------------------------------------------------

# 2. Visualización
fviz_eig(respca)

# Contribución de las variables a los componentes principales
fviz_contrib(respca, choice = "var") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
fviz_contrib(respca, choice = "ind") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# ----------------------------------------------------------------------

# 3. Selección de Componentes Principales para el entrenamiento
var_explained <- summary(respca)$importance[3, ]
num_componentes <- min(which(var_explained >= 0.85))
print(paste("Número de componentes principales seleccionados:",
            num_componentes))

df_pca_train <- as.data.frame(respca$x[, 1:num_componentes])
df_pca_train$SalePrice <- df_train_final$SalePrice
df_pca_train$Log_SalePrice <- df_train_final$Log_SalePrice

View(df_pca_train)
dim(df_pca_train)

# ----------------------------------------------------------------------

# 4. Transformación de los conjuntos de prueba y validación
# respca contiene las "formulas" para crear los PC
# a partir de esta "formulas" y usando predict y
# los datos de prueba y validacion se crean los nuevos dataframes
# y de esos dataframes se seleccionan los primeros num_componentes
test_pca_transformed <- predict(respca,
                                newdata = df_test_final %>%
                                  select(-SalePrice, -Log_SalePrice))

df_pca_test <- as.data.frame(test_pca_transformed[, 1:num_componentes])
df_pca_test$SalePrice <- df_test_final$SalePrice
df_pca_test$Log_SalePrice <- df_test_final$Log_SalePrice

val_pca_transformed <- predict(respca,
                               newdata = df_val_final %>%
                                 select(-SalePrice, -Log_SalePrice))
df_pca_val <- as.data.frame(val_pca_transformed[, 1:num_componentes])
df_pca_val$SalePrice <- df_val_final$SalePrice
df_pca_val$Log_SalePrice <- df_val_final$Log_SalePrice

print("--- Dimensiones de los DataFrames PCA ---")
print("Dimensiones de PCA (Entrenamiento):")
print(dim(df_pca_train))
print("Dimensiones de PCA (Prueba):")
print(dim(df_pca_test))
print("Dimensiones de PCA (Validación):")
print(dim(df_pca_val))

View(df_pca_test)
View(df_pca_val)
View(df_pca_train)


# +-----------------------------------+
# | Regresión Lineal Múltiple con PCA |
# +-----------------------------------+

# 1. Eentrenamiento del Modelo de Regresión Lineal Múltiple con PCA
modelo_lm_pca <- lm(Log_SalePrice ~ ., data = df_pca_train)

print("Modelo Ajustado con PCA:")
print(summary(modelo_lm_pca))

#----------------------------------------------
# 2. Perdicción y Evaluación

pred_train_pca <- predict(modelo_lm_pca, newdata = df_pca_train)
pred_val_pca <- predict(modelo_lm_pca, newdata = df_pca_val)
pred_test_pca <- predict(modelo_lm_pca, newdata = df_pca_test)

# Función para calcular métricas de error
rmse <- function(y_true, y_pred) sqrt(mean((y_true - y_pred)^2))
mae <- function(y_true, y_pred) mean(abs(y_true - y_pred))
r2 <- function(y_true, y_pred) cor(y_true, y_pred)^2


# +-------------------------------------------+
# | Funcion para Regresión con regularización |
# +-------------------------------------------+

regresion_con_regularizacion <- function(x_train, y_train,
                                         alpha) {
  set.seed(123)
  cv <- cv.glmnet(x_train, y_train,
                  alpha = alpha,
                  nfolds = 15)
  best_lambda <- cv$lambda.min

  print(paste("Mejor lambda:", best_lambda))
  modelo <- glmnet(x_train, y_train,
                   alpha = alpha,
                   lambda = best_lambda)
  print("Modelo Ajustado:")
  print(modelo)
  list(modelo = modelo, best_lambda = best_lambda)
}

# + -----------------------------------+
# | Regresión con regularización Lasso |
# + -----------------------------------+

# 1. Preparación de los datos para glmnet quitando las varibles objetivo
x_train <- as.matrix(df_train_final %>% select(-SalePrice, -Log_SalePrice))
y_train <- df_train_final$Log_SalePrice
x_val <- as.matrix(df_val_final %>% select(-SalePrice, -Log_SalePrice))
y_val <- df_val_final$Log_SalePrice
x_test <- as.matrix(df_test_final %>% select(-SalePrice, -Log_SalePrice))
y_test <- df_test_final$Log_SalePrice


#-----------------------------------------------
# 2. Creamos el modelo Lasso (alpha = 1)
resultados_lasso <- regresion_con_regularizacion(x_train,
                                                 y_train,
                                                 alpha = 1)


modelo_lasso <- resultados_lasso$modelo
best_lambda_lasso <- resultados_lasso$best_lambda

print("Modelo Lasso Ajustado:")
print(modelo_lasso)

#-----------------------------------------------
# 3. Predicción y Evaluación del modelo Lasso
pred_train_lasso <- predict(modelo_lasso, s = best_lambda_lasso,
                            newx = x_train)

pred_val_lasso <- predict(modelo_lasso, s = best_lambda_lasso,
                          newx = x_val)

pred_test_lasso <- predict(modelo_lasso, s = best_lambda_lasso,
                           newx = x_test)


# +--------------------------------+
# | Regrsión Lineal Múltiple Ridge |
# +--------------------------------+

#1 . Creamos el modelo Ridge (alpha = 0)
resultados_ridge <- regresion_con_regularizacion(x_train,
                                                 y_train,
                                                 alpha = 0)
modelo_ridge <- resultados_ridge$modelo
best_lambda_ridge <- resultados_ridge$best_lambda
print("Modelo Ridge Ajustado:")
print(modelo_ridge)

#-----------------------------------------------
# 2. Predicción y Evaluación del modelo Ridge
pred_train_ridge <- predict(modelo_ridge, s = best_lambda_ridge,
                            newx = x_train)

pred_val_ridge <- predict(modelo_ridge, s = best_lambda_ridge,
                          newx = x_val)

pred_test_ridge <- predict(modelo_ridge, s = best_lambda_ridge,
                           newx = x_test)


# +---------------------------------+
# | Regresión Lasso y Ridge con PCA |
# +---------------------------------+

# 1. Preparación de los datos para glmnet con PCA
x_train_pca <- as.matrix(df_pca_train %>% select(-SalePrice, -Log_SalePrice))
y_train_pca <- df_pca_train$Log_SalePrice
x_val_pca <- as.matrix(df_pca_val %>% select(-SalePrice, -Log_SalePrice))
y_val_pca <- df_pca_val$Log_SalePrice
x_test_pca <- as.matrix(df_pca_test %>% select(-SalePrice, -Log_SalePrice))
y_test_pca <- df_pca_test$Log_SalePrice

#----------------------------------------------
# 2. Creamos el modelo Lasso con PCA (alpha = 1)
resultados_lasso_pca <- regresion_con_regularizacion(x_train_pca,
                                                     y_train_pca,
                                                     alpha = 1)
modelo_lasso_pca <- resultados_lasso_pca$modelo
best_lambda_lasso_pca <- resultados_lasso_pca$best_lambda
print("Modelo Lasso con PCA Ajustado:")
print(modelo_lasso_pca)

pred_train_lasso_pca <- predict(modelo_lasso_pca,
                                s = best_lambda_lasso_pca,
                                newx = x_train_pca)
pred_val_lasso_pca <- predict(modelo_lasso_pca,
                              s = best_lambda_lasso_pca,
                              newx = x_val_pca)
pred_test_lasso_pca <- predict(modelo_lasso_pca,
                               s = best_lambda_lasso_pca,
                               newx = x_test_pca)


#----------------------------------------------
# 3. Creamos el modelo Ridge con PCA (alpha = 0)
resultados_ridge_pca <- regresion_con_regularizacion(x_train_pca,
                                                     y_train_pca,
                                                     alpha = 0)
modelo_ridge_pca <- resultados_ridge_pca$modelo
best_lambda_ridge_pca <- resultados_ridge_pca$best_lambda
print("Modelo Ridge con PCA Ajustado:")
print(modelo_ridge_pca)

pred_train_ridge_pca <- predict(modelo_ridge_pca,
                                s = best_lambda_ridge_pca,
                                newx = x_train_pca)
pred_val_ridge_pca <- predict(modelo_ridge_pca,
                              s = best_lambda_ridge_pca,
                              newx = x_val_pca)
pred_test_ridge_pca <- predict(modelo_ridge_pca,
                               s = best_lambda_ridge_pca,
                               newx = x_test_pca)


# +---------------------------------------------------------------+
# | Evaluar la precisión, robustez y capacidad de generalización  |
# +---------------------------------------------------------------+

# Creo una tabla comparativa de resultados de los diferentes modelos
# con la varible objetivo Log_SalePrice
resultados_modelos <- data.frame(
  Modelo = c("Lineal con PCA",
             "Lasso (L1)",
             "Ridge (L2)",
             "Lasso con PCA",
             "Ridge con PCA"),
  MAE_E = c(
    round(mae(df_pca_train$Log_SalePrice, pred_train_pca), 2),
    round(mae(y_train, pred_train_lasso), 2),
    round(mae(y_train, pred_train_ridge), 2),
    round(mae(y_train_pca, pred_train_lasso_pca), 2),
    round(mae(y_train_pca, pred_train_ridge_pca), 2)
  ),
  MAE_V = c(
    round(mae(df_pca_val$Log_SalePrice, pred_val_pca), 2),
    round(mae(y_val, pred_val_lasso), 2),
    round(mae(y_val, pred_val_ridge), 2),
    round(mae(y_val_pca, pred_val_lasso_pca), 2),
    round(mae(y_val_pca, pred_val_ridge_pca), 2)
  ),
  MAE_T = c(
    round(mae(df_pca_test$Log_SalePrice, pred_test_pca), 2),
    round(mae(y_test, pred_test_lasso), 2),
    round(mae(y_test, pred_test_ridge), 2),
    round(mae(y_test_pca, pred_test_lasso_pca), 2),
    round(mae(y_test_pca, pred_test_ridge_pca), 2)
  ),
  RMSE_E = c(
    round(rmse(df_pca_train$Log_SalePrice, pred_train_pca), 2),
    round(rmse(y_train, pred_train_lasso), 2),
    round(rmse(y_train, pred_train_ridge), 2),
    round(rmse(y_train_pca, pred_train_lasso_pca), 2),
    round(rmse(y_train_pca, pred_train_ridge_pca), 2)
  ),
  RMSE_V = c(
    round(rmse(df_pca_val$Log_SalePrice, pred_val_pca), 2),
    round(rmse(y_val, pred_val_lasso), 2),
    round(rmse(y_val, pred_val_ridge), 2),
    round(rmse(y_val_pca, pred_val_lasso_pca), 2),
    round(rmse(y_val_pca, pred_val_ridge_pca), 2)
  ),
  RMSE_T = c(
    round(rmse(df_pca_test$Log_SalePrice, pred_test_pca), 2),
    round(rmse(y_test, pred_test_lasso), 2),
    round(rmse(y_test, pred_test_ridge), 2),
    round(rmse(y_test_pca, pred_test_lasso_pca), 2),
    round(rmse(y_test_pca, pred_test_ridge_pca), 2)
  ),
  R2_E = c(
    round(r2(df_pca_train$Log_SalePrice, pred_train_pca), 4),
    round(r2(y_train, pred_train_lasso), 4),
    round(r2(y_train, pred_train_ridge), 4),
    round(r2(y_train_pca, pred_train_lasso_pca), 4),
    round(r2(y_train_pca, pred_train_ridge_pca), 4)
  ),
  R2_V = c(
    round(r2(df_pca_val$Log_SalePrice, pred_val_pca), 4),
    round(r2(y_val, pred_val_lasso), 4),
    round(r2(y_val, pred_val_ridge), 4),
    round(r2(y_val_pca, pred_val_lasso_pca), 4),
    round(r2(y_val_pca, pred_val_ridge_pca), 4)
  ),
  R2_T = c(
    round(r2(df_pca_test$Log_SalePrice, pred_test_pca), 4),
    round(r2(y_test, pred_test_lasso), 4),
    round(r2(y_test, pred_test_ridge), 4),
    round(r2(y_test_pca, pred_test_lasso_pca), 4),
    round(r2(y_test_pca, pred_test_ridge_pca), 4)
  )
)

print("========================================")
print("      COMPARATIVA DE MODELOS")
print("========================================")
print(resultados_modelos)

# ---------------------------------------------------------------
# 2. Transfomrar a la escala originial de SalePrice
pred_train_lm_orig <- exp(pred_train_pca)
pred_val_lm_orig <- exp(pred_val_pca)
pred_test_lm_orig <- exp(pred_test_pca)

pred_train_lasso_orig <- exp(pred_train_lasso)
pred_val_lasso_orig <- exp(pred_val_lasso)
pred_test_lasso_orig <- exp(pred_test_lasso)

pred_train_ridge_orig <- exp(pred_train_ridge)
pred_val_ridge_orig <- exp(pred_val_ridge)
pred_test_ridge_orig <- exp(pred_test_ridge)

pred_train_lasso_pca_orig <- exp(pred_train_lasso_pca)
pred_val_lasso_pca_orig <- exp(pred_val_lasso_pca)
pred_test_lasso_pca_orig <- exp(pred_test_lasso_pca)

pred_train_ridge_pca_orig <- exp(pred_train_ridge_pca)
pred_val_ridge_pca_orig <- exp(pred_val_ridge_pca)
pred_test_ridge_pca_orig <- exp(pred_test_ridge_pca)


y_train_orig <- df_train_final$SalePrice
y_val_orig <- df_val_final$SalePrice
y_test_orig <- df_test_final$SalePrice



resultados_modelos_orig <- data.frame(
  Modelo = c("Lineal con PCA",
             "Lasso (L1)",
             "Ridge (L2)",
             "Lasso con PCA",
             "Ridge con PCA"),
  MAE_E = c(round(mae(y_train_orig, pred_train_lm_orig), 0),
            round(mae(y_train_orig, pred_train_lasso_orig), 0),
            round(mae(y_train_orig, pred_train_ridge_orig), 0),
            round(mae(y_train_orig, pred_train_lasso_pca_orig), 0),
            round(mae(y_train_orig, pred_train_ridge_pca_orig), 0)),

  MAE_V = c(round(mae(y_val_orig, pred_val_lm_orig), 0),
            round(mae(y_val_orig, pred_val_lasso_orig), 0),
            round(mae(y_val_orig, pred_val_ridge_orig), 0),
            round(mae(y_val_orig, pred_val_lasso_pca_orig), 0),
            round(mae(y_val_orig, pred_val_ridge_pca_orig), 0)),
  
  MAE_T = c(round(mae(y_test_orig, pred_test_lm_orig), 0),
            round(mae(y_test_orig, pred_test_lasso_orig), 0),
            round(mae(y_test_orig, pred_test_ridge_orig), 0),
            round(mae(y_test_orig, pred_test_lasso_pca_orig), 0),
            round(mae(y_test_orig, pred_test_ridge_pca_orig), 0)),

  RMSE_E = c(round(rmse(y_train_orig, pred_train_lm_orig), 0),
             round(rmse(y_train_orig, pred_train_lasso_orig), 0),
             round(rmse(y_train_orig, pred_train_ridge_orig), 0),
             round(rmse(y_train_orig, pred_train_lasso_pca_orig), 0),
             round(rmse(y_train_orig, pred_train_ridge_pca_orig), 0)),

  RMSE_V = c(round(rmse(y_val_orig, pred_val_lm_orig), 0),
             round(rmse(y_val_orig, pred_val_lasso_orig), 0),
             round(rmse(y_val_orig, pred_val_ridge_orig), 0),
             round(rmse(y_val_orig, pred_val_lasso_pca_orig), 0),
             round(rmse(y_val_orig, pred_val_ridge_pca_orig), 0)),

  RMSE_T = c(round(rmse(y_test_orig, pred_test_lm_orig), 0),
             round(rmse(y_test_orig, pred_test_lasso_orig), 0),
             round(rmse(y_test_orig, pred_test_ridge_orig), 0),
             round(rmse(y_test_orig, pred_test_lasso_pca_orig), 0),
             round(rmse(y_test_orig, pred_test_ridge_pca_orig), 0)),

  R2_E = c(round(r2(y_train_orig, pred_train_lm_orig), 4),
           round(r2(y_train_orig, pred_train_lasso_orig), 4),
           round(r2(y_train_orig, pred_train_ridge_orig), 4),
           round(r2(y_train_orig, pred_train_lasso_pca_orig), 4),
           round(r2(y_train_orig, pred_train_ridge_pca_orig), 4)),

  R2_V = c(round(r2(y_val_orig, pred_val_lm_orig), 4),
           round(r2(y_val_orig, pred_val_lasso_orig), 4),
           round(r2(y_val_orig, pred_val_ridge_orig), 4),
           round(r2(y_val_orig, pred_val_lasso_pca_orig), 4),
           round(r2(y_val_orig, pred_val_ridge_pca_orig), 4)),

  R2_T = c(round(r2(y_test_orig, pred_test_lm_orig), 4),
           round(r2(y_test_orig, pred_test_lasso_orig), 4),
           round(r2(y_test_orig, pred_test_ridge_orig), 4),
           round(r2(y_test_orig, pred_test_lasso_pca_orig), 4),
           round(r2(y_test_orig, pred_test_ridge_pca_orig), 4))
)

print("========================================")
print(" COMPARATIVA DE MODELOS (Escala Original)")
print("========================================")
print(resultados_modelos_orig)