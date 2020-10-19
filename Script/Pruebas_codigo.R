###############################################################################
# Esto son pruebas de agrupamiento con dplyr
df_train_mini_7semanas <- df_train %>%
  select(-Venta_hoy, -Dev_proxima, -Canal_ID, -Ruta_SAK, -Agencia_ID) %>% 
  group_by(Cliente_ID,Producto_ID) %>% 
  mutate(n=n()) %>% 
  filter(n>1)

df_train_mini_7semanas <- df_train %>%
  select(-Venta_hoy, -Dev_proxima, -Canal_ID, -Ruta_SAK, -Agencia_ID) %>% 
  group_by(Cliente_ID,Producto_ID) %>% 
  summarise(n=n()) %>% 
  filter(n>1)

################################################################################

df_train <- data.frame(df_train[Semana>7,])

df_train <- df_train %>% 
  select(-Venta_uni_hoy,-Venta_hoy,-Dev_uni_proxima,-Dev_proxima)

df_train <- df_train %>% 
  select(-Agencia_ID,-Canal_ID, -Ruta_SAK)

df_train_7semanas <- df_train %>%
  group_by(Cliente_ID,Producto_ID) %>% 
  mutate(n=n()) %>% 
  filter(n>6) %>% 
  ungroup()

df_train_7semanas_sum <- df_train_7semanas %>% 
  select(-n) %>% 
  group_by(Semana, Cliente_ID, Producto_ID) %>% 
  summarise(Demanda_uni_equil=sum(Demanda_uni_equil)) %>% 
  ungroup()



df_train <- df_train_7semanas_sum
rm(df_train_7semanas, df_train_7semanas_sum)


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
modelo_01 <- xgboost(data = df_train_mat, #objective = "reg:linear",
                     nrounds = 1000000, max.depth = 10, eta = 0.01, nthread = 6, 
                     min_child_weight = 4, #subsample = 0.85,
                     early_stopping_rounds = 50,
                     #xgb_model = "xgboost.model",
                     save_period = 100,
                     print_every_n = 5)

# Predicciones
predict_01 <- predict(modelo_01, df_valid_mat)


cbind(valid_df$Demanda_uni_equil,predict_01)

RMSE_valid <- sqrt(mean((predict_01-valid_df$Demanda_uni_equil)^2))
