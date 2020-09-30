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



library(tidyverse)
library(data.table)
library(funModeling)

rm(list = ls())

df_clientes <- read_csv("Data/cliente_tabla.csv")
df_producto <- read_csv("Data/producto_tabla.csv")
df_town_state <- read_csv("Data/town_state.csv")
df_test <- read_csv("Data/test.csv")
df_sample <- read_csv("Data/sample_submission.csv")
#df_train <- read_csv("Data/train.csv")
df_train<-readRDS("Data/df_train")

# names(df_train) <- c()

df_train$Semana <- as.factor(df_train$Semana)
df_train$Agencia_ID <- as.factor(df_train$Agencia_ID)
df_train$Canal_ID <- as.factor(df_train$Canal_ID)
df_train$Ruta_SAK <- as.factor(df_train$Ruta_SAK)
df_train$Cliente_ID <- as.factor(df_train$Cliente_ID)
df_train$Producto_ID <- as.factor(df_train$Producto_ID)

#saveRDS(df_train,"Data/df_train")

str(df_train)
# summary(df_train)
train_status <- df_status(df_train, print_results = F)
train_status
