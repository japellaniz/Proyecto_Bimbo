#
# File descriptions
# 
# train.csv — the training set
# test.csv — the test set
# sample_submission.csv — a sample submission file in the correct format
# cliente_tabla.csv — client names (can be joined with train/test on Cliente_ID)
# producto_tabla.csv — product names (can be joined with train/test on Producto_ID)
# town_state.csv — town and state (can be joined with train/test on Agencia_ID)
# 
# Data fields
# 
# Semana — Week number (From Thursday to Wednesday)
# Agencia_ID — Sales Depot ID
# Canal_ID — Sales Channel ID
# Ruta_SAK — Route ID (Several routes = Sales Depot)
# Cliente_ID — Client ID
# NombreCliente — Client name
# Producto_ID — Product ID
# NombreProducto — Product Name
# Venta_uni_hoy — Sales unit this week (integer)
# Venta_hoy — Sales this week (unit: pesos)
# Dev_uni_proxima — Returns unit next week (integer)
# Dev_proxima — Returns next week (unit: pesos)
# Demanda_uni_equil — Adjusted Demand (integer) (This is the target you will predict)


{
library(tidyverse)
library(data.table)
library(funModeling)
library(caret)
library(magrittr)
library(xgboost)
}
rm(list = ls())

# ##################################################################################
# Lectura de ficheros .csv con readr::read_csv y con data.table::fread
# Para los ficheros más grandes usamos fread
# Adaptamos las columnas al tipo adecuado tras reducir el tamaño de los df
# ##################################################################################
#
# Orden de mayor a menor tamaño: train, test, (sample,) clientes, producto, town_state

#df_train <- fread("Data/train.csv")

# Tarda en leer el train.csv así que lo guardamos
#saveRDS(df_train,"Data/df_train")
df_train<-readRDS("Data/df_train")

df_test <- fread("Data/test.csv")

#df_sample <- read_csv("Data/sample_submission.csv")

df_clientes <- fread("Data/cliente_tabla.csv", colClasses = c("factor","character"))
df_producto <- read_csv("Data/producto_tabla.csv", col_types = "fc")
df_town_state <- read_csv("Data/town_state.csv",col_types = "fcc")

# Por si queremos cambiar los nombres de las columnas
#names(df_train) <- c()
##################################################################################

####################################################################################
# Filtrado de datos de train con 7 ó más semanas.
# Sobre el df original, le quitamos las variables numéricas de ventas y devoluciones
# que no son la Demanda (ya que esta última es el objetivo de la predicción y además es 
# linealmente dependiente de las otras), agrupamos por parejas Cliente-Producto y nos 
# quedamos con los registros con más de 6 entradas para asegurarnos de que hay al menos 
# un valor por semana.
####################################################################################
df_train_7semanas <- df_train %>%
  select(-Venta_hoy, -Dev_proxima, -Venta_uni_hoy, -Dev_uni_proxima) %>% 
  group_by(Cliente_ID,Producto_ID) %>% 
  mutate(n=n()) %>% 
  filter(n>6) %>% 
  ungroup()


# Veamos la distribución de n (combinaciones Cliente-Producto) sin tener en cuenta el valor 
# dominante "7".
par(mfrow=c(1,1))
df_train_7semanas %>% select(n) %>% filter(n>7) %>% 
  hist(.,breaks = 1000, xlim= range(0,3000))

# # En caso de querer agrupar las cantidades por cliente y producto
# df_train_7s <- df_train_7semanas %>% select(everything()) %>% 
#   group_by(Semana, Cliente_ID, Producto_ID, n) %>% 
#   summarise(sum(Demanda_uni_equil), .groups = "keep")
# # Pero se pierde la dependencia con las otras variables

# Eliminamos la columna "n". Ya no nos sirve.
df_train_7semanas$n <- NULL

str(df_train_7semanas)
summary(df_train_7semanas)
train_status <- df_status(df_train_7semanas, print_results = F)
train_status



##################################################################################
# Adaptación de las dimensiones de los df de train y test a tamaños manejables
# ################################################################################

set.seed(43)

df_train_backup <- df_train
df_train <- df_train_7semanas[sample(nrow(df_train_7semanas), nrow(df_train_7semanas)*0.01),]


df_test_backup <- df_test
df_test <- df_test[sample(nrow(df_test), nrow(df_test)*0.01),]


# ###############################################################################
# Calidad del dato de df_train y df_test
# ###############################################################################
str(df_train)
summary(df_train)
train_status <- df_status(df_train, print_results = F)
train_status

str(df_test)
summary(df_test)
test_status <- df_status(df_test, print_results = F)
test_status

###############################################################################
# Preparación para el modelo XGboost
##############################################################################

# Hacemos dos sets, uno para entrenamiento y otro para validación
set.seed(43)
train_df <- sample_frac(df_train, size = 0.7)
valid_df <- setdiff(df_train, train_df)

# Conversión a DMatrix de los dataset de train y valid

df_train_mat <- train_df %>% 
  select(-Demanda_uni_equil) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = train_df$Demanda_uni_equil)

df_valid_mat <- valid_df %>% 
  select(-Demanda_uni_equil) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = valid_df$Demanda_uni_equil)

# Entrenamiento del modelo
modelo_01 <- xgboost(data = df_train_mat, objective = "reg:linear",
                     nrounds = 10000, max.depth = 12, eta = 0.1, nthread = 6, 
                     min_child_weight = 4, subsample = 0.85,
                     early_stopping_rounds = 10,
                     xgb_model = "xgboost.model",
                     save_period = 100,
                     print_every_n = 5)
                     
# Predicciones
predict_01 <- predict(modelo_01, df_valid_mat)


cbind(valid_df$Demanda_uni_equil,predict_01)

RMSE_valid <- sqrt(mean((predict_01-valid_df$Demanda_uni_equil)^2))
#####################################################################################
# Repetimos el entrenamiento pero con los datos de 7 semanas completos
#####################################################################################

# Hacemos dos sets, uno para entrenamiento y otro para validación
set.seed(43)
train_df <- sample_frac(df_train_7semanas, size = 0.7)
valid_df <- setdiff(df_train_7semanas, train_df)

# Conversión a DMatrix de los dataset de train y valid

df_train_mat <- train_df %>% 
  select(-Demanda_uni_equil) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = train_df$Demanda_uni_equil)

df_valid_mat <- valid_df %>% 
  select(-Demanda_uni_equil) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = valid_df$Demanda_uni_equil)

# Entrenamiento del modelo
modelo_01 <- xgboost(data = df_train_mat, objective = "reg:linear",
                     nrounds = 100000, max.depth = 2, eta = 0.05, nthread = 6, 
                     min_child_weight = 4,
                     early_stopping_rounds = 10,
                     #xgb_model = "xgboost.model",
                     save_period = 100)

# Predicciones
predict_01 <- predict(modelo_01, df_valid_mat)


cbind(valid_df$Demanda_uni_equil,predict_01)

