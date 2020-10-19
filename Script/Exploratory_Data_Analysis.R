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
library(caret)
library(magrittr)

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

##################################################################################
# Adaptación de los tamaños de los df de train, test y sample a tamaños manejables
# ################################################################################

set.seed(43)

df_train_backup <- df_train
df_train <- df_train[sample(nrow(df_train), nrow(df_train)*0.001),]

df_test_backup <- df_test
df_test <- df_test[sample(nrow(df_test), nrow(df_test)*0.001),]


# ###############################################################################
# Calidad del dato de df_train y df_test
# ###############################################################################
str(df_train)
df_train$Semana <- as.factor(df_train$Semana)
df_train$Agencia_ID <- as.factor(df_train$Agencia_ID)
df_train$Canal_ID <- as.factor(df_train$Canal_ID)
df_train$Ruta_SAK <- as.factor(df_train$Ruta_SAK)
df_train$Cliente_ID <- as.factor(df_train$Cliente_ID)
df_train$Producto_ID <- as.factor(df_train$Producto_ID)

str(df_train)
summary(df_train)
train_status <- df_status(df_train, print_results = F)

train_status

str(df_test)
df_test$id <- as.factor(df_test$id)
df_test$Semana <- as.factor(df_test$Semana)
df_test$Agencia_ID <- as.factor(df_test$Agencia_ID)
df_test$Canal_ID <- as.factor(df_test$Canal_ID)
df_test$Ruta_SAK <- as.factor(df_test$Ruta_SAK)
df_test$Cliente_ID <- as.factor(df_test$Cliente_ID)
df_test$Producto_ID <- as.factor(df_test$Producto_ID)


str(df_test)
summary(df_test)
test_status <- df_status(df_test, print_results = F)
test_status


df_clientes$Cliente_ID <- as.factor(df_clientes$Cliente_ID)
df_producto$Producto_ID <- as.factor(df_producto$Producto_ID)
df_town_state$Agencia_ID <- as.factor(df_town_state$Agencia_ID)

#########################################################################
# Análisis de outliers
#########################################################################

# Miramos los valores numéricos de train
par(mfrow=c(1,5))
plot(df_train$Venta_uni_hoy, main = "Venta_uni_hoy", 
     xlab= "Observación", ylab = "Unidades vendidas", col = "blue")
plot(df_train$Venta_hoy, main = "Venta_hoy", 
     xlab= "Observación", ylab = "Venta en pesos", col = "blue")
plot(df_train$Dev_uni_proxima, main = "Dev_uni_proxima", 
     xlab= "Observación", ylab = "Unidades devueltas", col = "blue")
plot(df_train$Dev_proxima, main = "Dev_proxima", 
     xlab= "Observación", ylab = "Devolucion en pesos", col = "blue")
plot(df_train$Demanda_uni_equil, main = "Demanda_uni_equil", 
     xlab= "Observación", ylab = "Unidades demandadas", col = "blue")


# Comprobamos con boxplot y recogemos los valores tipificados como 
# outliers por boxplot
par(mfrow=c(1,1))
venta.uni <- boxplot(df_train$Venta_uni_hoy, main = "Venta_uni_hoy", 
     ylab = "Unidades vendidas")
venta.uni$out
venta.pesos <- boxplot(df_train$Venta_hoy, main = "Venta_hoy", 
     ylab = "Venta en pesos")
venta.pesos$out
dev.uni <- boxplot(df_train$Dev_uni_proxima, main = "Dev_uni_proxima", 
     ylab = "Unidades devueltas")
dev.uni$out
dev.pesos <- boxplot(df_train$Dev_proxima, main = "Dev_proxima", 
     ylab = "Devolucion en pesos")
dev.pesos$out
demanda <- boxplot(df_train$Demanda_uni_equil, main = "Demanda_uni_equil", 
     ylab = "Unidades demandadas")
demanda$out

summary(df_train)

# Del análisis comparado de outliers decidimos no quitar ni imputar valores
# ya que cuando se dan valores extremos de ventas unitarias también se producen
# valores extremos de venta en pesos, descartando de esa forma los posibles errores
# en la captación de datos.
# Lo mismo se puede decir de los regitros de devolución.
# Los valores de demanda son dependientes de los anteriores por lo que tampoco
# hay casos claros de outliers

##################################################################################
# Análisis de distribución de los datos
##################################################################################

str(df_train)

# Distribución de las variables categóricas
par(mfrow=c(2,3))
plot(df_train$Semana, main="Semanas")
plot(df_train$Agencia_ID,main="Agencias")
plot(df_train$Canal_ID,main="Canales")
plot(df_train$Ruta_SAK,main="Rutas")
plot(df_train$Cliente_ID,main="Clientes")
plot(df_train$Producto_ID,main="Productos")

# Vemos que:
# - Las semanas están equilibradas en cuanto a nº de registros
# - En las Agencias hay mucha variabilidad de unas a otras pero ninguna destaca
# de forma excesiva respecto a las demás.
# - En los canales domina claramente el "1".
# - En las Rutas hay cierta variabilidad entre ellas pero tampoco hay una
# claramente superior al resto.
# - En los Clientes hay uno que domina sobre los demás.
# - En los productos tambien hay variabilidad con un grupo mayoritario.

# Distribución de las variables numéricas
par(mfrow=c(1,1))
hist(df_train$Venta_uni_hoy, main = "Ventas Unidades", breaks = 1000, xlim= range(0,100))
hist(df_train$Venta_hoy, main = "Ventas en pesos", breaks = 1000, xlim= range(0,1000))
hist(df_train$Dev_uni_proxima, main = "Devoluciones Unidades", breaks = 10000, xlim= range(0,10))
hist(df_train$Dev_proxima, main = "Devoluciones en pesos",  breaks = 10000, xlim= range(0,20))
hist(df_train$Demanda_uni_equil, main = "Demanda",  breaks = 1000, xlim= range(0,100))

# Las distribuciones de las variables numéricas presentan un patrón común en todas 
# ellas hacia los valores bajos para cada una de las combinaciones de 
# semana-agencia-canal-ruta-cliente-producto: se venden mayoritariamente valores pequeños
# se producen muy pocas devoluciones (mayoritariamente "0"),y la demanda ajustada 
# (diferencia entre ventas de una semana y devoluciones de la siguiente) también está
# dominada por valores bajos (0-5).

########################################################################################
# Exploratory Data Analysis
########################################################################################

# Buscamos: semana-cliente-producto-->demanda
# El 99,98% de las transacciones se producto-cliente realizan con la misma combinación
# de agencia, canal y ruta.

df_train %>% select(Semana,Cliente_ID,Producto_ID) %>% 
  n_distinct()
# 74169/74180 = 99,98%

df_temp <- df_train %>% select(Semana,Cliente_ID,Producto_ID,Demanda_uni_equil) %>% 
  unite(Cliente_ID,Producto_ID, col = Cliente_Producto_ID, sep = "-") %>% 
  group_by(Semana, Cliente_Producto_ID) %>% 
  summarise(sum(Demanda_uni_equil), .groups = "keep")
# Salen muy pocas observaciones con más de una semana de registros por lo que no nos sirve 
# para hacer predicciones

# Vamos a volver sobre el df original, le quitamos las variables numéricas que no son
# la Demanda (ya que esta última es el objetivo de la predicción y además es linealmente
# dependiente de las otras), agrupamos por parejas Cliente-Producto y nos quedamos
# con los registros con más de 6 entradas para asegurarnos de que hay al menos un valor 
# por semana.
df_train_7semanas <- df_train %>%
  select(-Venta_hoy, -Dev_proxima, -Venta_uni_hoy, -Dev_uni_proxima) %>% 
  group_by(Cliente_ID,Producto_ID) %>% 
  mutate(n=n()) %>% 
  filter(n>6) 


# Veamos la distribución de n (combinaciones Cliente-Producto) sin tener en cuenta el valor 
# dominante "7".
par(mfrow=c(1,1))
df_train_7semanas %>% ungroup() %>% select(n) %>% filter(n>7) %>% 
  hist(.,breaks = 1000, xlim= range(0,3000))

