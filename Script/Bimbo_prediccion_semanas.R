

# Carga de las librerías necesarias ################################
{
  library(tidyverse)
  library(data.table)
  library(caret)
  library(magrittr)
  library(Metrics)
  library(ggpubr)
}

set.seed(43)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Carga del archivo de datos con fread #############################
#dt_train <- fread("Data/train.csv")
# Tarda en leer el train.csv así que lo guardamos
#saveRDS(dt_train,"Data/dt_train")
dt_train<-readRDS("Data/dt_train")
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Obtención de un tablón ############################################
# Utilizamos data.table para manipular la tabla y obtener un tablón
# con las semanas en columnas
train<-dt_train[, .(Semana,Cliente_ID,Producto_ID, Demanda_uni_equil)]
rm(dt_train)
train<-train[, .(Demanda_agg=sum(Demanda_uni_equil)), .(Semana, Cliente_ID,Producto_ID)]
train<-train[, .(Semana,Demanda_agg,num_sem=.N),.(Cliente_ID,Producto_ID)]
train<-train[num_sem==7]
train<-train[,num_sem:=NULL]
df_train<-as_tibble(train)
df_sample <- df_train %>% select(Cliente_ID,Producto_ID) %>% 
  distinct() %>%
  sample_frac(0.01)  
rm(train)
df_sample <- df_train %>% inner_join(df_sample, by = c("Cliente_ID", "Producto_ID"))
rm(df_train)
tablon <- df_sample %>% 
  spread(Semana,Demanda_agg) %>% 
  rowid_to_column(var = "id")

colnames(tablon) <- c("id","Cliente_ID", "Producto_ID", "S3", "S4", "S5", "S6", "S7", "S8", "S9")
saveRDS(tablon, "Data/tablon")
write_csv(tablon,"Data/tablon.csv")
write_csv(df_sample,"Data/df_sample.csv")
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Punto de recuperacion del WS #####
#save.image(file = "Data/Bimbo_WS.RData")
load("Data/Bimbo_WS.RData")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MODELOS DE PREDICCIÓN ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Siguiendo el principio de la navaja de Occam optamos por la adopción
# de los modelos más sencillos posibles que den buenos resultados.
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modelo: Datos sin normalizar ####
# Obtenemos la media para cada Cliente-Producto y calculamos la diferencia con la semana 9.
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
train_normal <- tablon %>% 
  rowwise() %>% 
  mutate(media = floor(mean(c_across(S3:S8)))) %>% 
  mutate(diff = S9-media)

p1 <-train_normal %>% ggplot() + 
  geom_density(aes(x=media,fill = "Prediccion"),alpha = 0.5) +
  geom_density(aes(x=S9,fill = "Semana 9"),alpha = 0.5) +
  scale_fill_manual("", guide = "legend",
                    values = c("Prediccion" = "orangered2",
                               "Semana 9" = "gray50")) +
  theme_minimal()+
  theme(legend.position = c(0.8,0.9))+
  labs(x = "Demanda unitaria",
       y = "Densidad",
       title = "Comparativa Semana 9 vs media",
       subtitle = "Para todo el rango de valores de demanda")

p2 <- train_normal %>% ggplot() + 
  geom_density(aes(x=media,fill = "Prediccion"),alpha = 0.5) +
  geom_density(aes(x=S9,fill = "Semana 9"),alpha = 0.5) +
  scale_fill_manual("", guide = "legend",
                    values = c("Prediccion" = "orangered2",
                               "Semana 9" = "gray50")) +
  scale_x_continuous(limits=c(0,100))+
  theme_minimal()+
  theme(legend.position = c(0.8,0.9))+
  labs(x = "Demanda unitaria",
       y = "Densidad",
       title = "Comparativa Semana 9 vs media",
       subtitle = "Rango de demanda 0-100") 
  
p3 <- train_normal %>% ggplot() + 
  geom_density(aes(x=media,fill = "Prediccion"),alpha = 0.5) +
  geom_density(aes(x=S9,fill = "Semana 9"),alpha = 0.5) +
  scale_fill_manual("", guide = "legend",
                     values = c("Prediccion" = "orangered2",
                                "Semana 9" = "gray50")) +
  scale_x_continuous(limits=c(0,50))+
  theme_minimal()+
  theme(legend.position = c(0.8,0.9))+
  labs(x = "Demanda unitaria",
       y = "Densidad",
       title = "Comparativa Semana 9 vs media",
       subtitle = "Rango de demanda 0-50") 
  
p4 <- train_normal %>% ggplot() + 
  geom_density(aes(x=media,fill = "Prediccion"),alpha = 0.5) +
  geom_density(aes(x=S9,fill = "Semana 9"),alpha = 0.5) +
  scale_fill_manual("", guide = "legend",
                    values = c("Prediccion" = "orangered2",
                               "Semana 9" = "gray50")) +
  scale_x_continuous(limits=c(0,10))+
  theme_minimal()+
  theme(legend.position = c(0.8,0.9))+
  labs(x = "Demanda unitaria",
       y = "Densidad",
       title = "Comparativa Semana 9 vs media",
       subtitle = "Rango de demanda 0-10") 

ggarrange(p1, p2, p3, p4, common.legend = TRUE, legend = "bottom")

df_resumen <- tibble() 
smr <- summary(train_normal$diff) 
sd <- sd(train_normal$diff)
var <- var(train_normal$diff)
rmsle <- rmsle(train_normal$S9, train_normal$media)

df_resumen <- as_tibble(t(c("Media", smr[1], smr[2], smr[3], smr[4], smr[5], smr[6], sd, var, rmsle)))
colnames(df_resumen) <- c("Model","Min", "1st Q", "Median", "Mean", "3rd Q", "Max", "SD", "Var", "RMSLE")
df_resumen$Min <- format(round(as.numeric(df_resumen$Min),4))
df_resumen$`1st Q` <- format(round(as.numeric(df_resumen$`1st Q`),4))
df_resumen$Median <- format(round(as.numeric(df_resumen$Median),4))
df_resumen$Mean <- format(round(as.numeric(df_resumen$Mean),4))
df_resumen$`3rd Q` <- format(round(as.numeric(df_resumen$`3rd Q`),4))
df_resumen$Max <- format(round(as.numeric(df_resumen$Max),4))
df_resumen$SD <- format(round(as.numeric(df_resumen$SD),4))
df_resumen$Var <- format(round(as.numeric(df_resumen$Var),4))
df_resumen$RMSLE <- format(round(as.numeric(df_resumen$RMSLE),4))


train_normal %>% ggplot(aes(x=id, y=diff))+
  geom_point(alpha = 1/3) +
  geom_line(alpha = 1/6)+
  scale_y_continuous(limits=c(-800,800))+
  theme_minimal()+
  labs(x = "id Producto-Cliente",
       y = " % Desviación de la predicción",
       title = "Diferencia S9-Predicción")

train_normal %>% ggplot(aes(x=diff))+
  geom_density(color = "gray50") +
  scale_x_continuous(limits=c(-50,50))+
  theme_minimal()+
  labs(x = "Desviación de la predicción",
       y = "Densidad",
       title = "Densidad de distribución de la diferencia S9-Predicción")

train_normal %>% ggplot(aes(y=diff))+
  geom_boxplot(varwidth = TRUE, color = "orangered") +
  scale_y_continuous(limits=c(-10,10))+
  theme_minimal()+
  labs(y = "Desviación de la predicción",
       title = "Boxplot para la diferencia S9-Predicción")

RMSLE <- rmsle(train_normal$S9, train_normal$media)
# 0,464071

train_normal <- train_normal %>% 
  mutate(Prediccion = round(media + mean(train_normal$diff))) %>% 
  mutate(diff_2 = S9 - Prediccion)

train_normal %>% ggplot(aes(fill=c(S9,Prediccion))) + 
  geom_density(aes(x=Prediccion),alpha = 0.5,fill = "orangered2") +
  geom_density(aes(x=S9),alpha = 0.5,fill = "gray50") +
  scale_x_continuous(limits=c(-5,10))+
  theme_minimal()+
  labs(x = "Demanda normalizada",
       y = "Densidad",
       title = "Modelo media-normalizado-corregido",
       subtitle = "Comparativa Semana 9 vs Prediccion") 
train_normal %>% select(S9,Prediccion) %>% 
  filter(Prediccion <= S9+2 & Prediccion >= S9-2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modelo media: Calculo del error ####
# Cálculo de la predicción como redondeo de la media hacia abajo 
# para evitar devoluciones.

# Calculo de la prediccion (media redondeada hacia abajo), nueva diferencia entre S9 y predicción
# y la diferencia en valor absoluto.
train_normal <- train_normal %>% 
  mutate(prediccion = floor(media)) %>% 
  mutate(diff_2 = S9-prediccion) %>% 
  mutate(diff_2_abs = abs(diff_2))

train_normal %>% ggplot()+
  geom_density(aes(x=S9),color="green")+
  geom_density(aes(x=prediccion),color="red")+
  scale_x_continuous(limits=c(0,50))


# Error en el número de productos en valor absoluto: productos por exceso + productos por defecto
train_error <- sum(train_normal$diff_2_abs)

# Contamos los productos por exceso y por defecto
train_error_positivos <- 0
train_error_negativos <- 0

for (i in 1:nrow(train_normal)){
    if (train_normal$diff_2[i]>0) 
      train_error_positivos <- train_error_positivos + train_normal$diff_2[i]
    else 
      train_error_negativos <- train_error_positivos + train_normal$diff_2[i]
}

# Diferencia entre productos por exceso y productos por defecto
train_error_positivos - train_error_negativos
# Error en porcentaje
error_producto = (train_error/sum(train_normal$S9))*100

RMSLE_mod3.2 <- rmsle(train_normal$S9, train_normal$prediccion)
# 0,466419

# Comprobamos con boxplot y recogemos los valores tipificados como 
# outliers por boxplot
par(mfrow=c(1,1))

bp <- train_normal %>% ggplot(aes(x=(S3+S4+S5+S6+S7+S8+S9)))+
  geom_boxplot(outlier.colour="red",outlier.shape=1,outlier.size=2)+
  coord_flip()
bp

lista_minimos <- c()

x <- train_normal$S3
out <- train_normal$S3[x > quantile(x, 0.75) + 1.5 * IQR(x)]
lista_minimos[1] <- min(out)

x <- train_normal$S4
out <- train_normal$S4[x > quantile(x, 0.75) + 1.5 * IQR(x)]
lista_minimos[2] <- min(out)

x <- train_normal$S5
out <- train_normal$S5[x > quantile(x, 0.75) + 1.5 * IQR(x)]
lista_minimos[3] <- min(out)

x <- train_normal$S6
out <- train_normal$S6[x > quantile(x, 0.75) + 1.5 * IQR(x)]
lista_minimos[4] <- min(out)

x <- train_normal$S7
out <- train_normal$S7[x > quantile(x, 0.75) + 1.5 * IQR(x)]
lista_minimos[5] <- min(out)

x <- train_normal$S8
out <- train_normal$S8[x > quantile(x, 0.75) + 1.5 * IQR(x)]
lista_minimos[6] <- min(out)

x <- train_normal$S9
out <- train_normal$S9[x > quantile(x, 0.75) + 1.5 * IQR(x)]
lista_minimos[7] <- min(out)

min(lista_minimos)

summary(train_normal)
# Confirma que el minimo de los outliers es 26

train_sin_out <- train_normal %>% 
  select(everything()) %>% 
  filter(S3<26 & S4<26 & S5<26 & S6<26 & S7<26 & S8<26 & S9<26)

train_con_out <- train_normal %>% 
  select(everything()) %>% 
  filter(S3>25 | S4>25 | S5>25 | S6>25 | S7>25 | S8>25 | S9>25)


# Sin outliers
train_sin_out %>% ggplot(aes(x=diff))+
  geom_density()
  #scale_x_continuous(limits=c(-20,20))

# Error en el número de productos en valor absoluto: productos por exceso + productos por defecto
train_error_sin_out <- sum(train_sin_out$diff_2_abs)

# Contamos los productos por exceso y por defecto
train_error_sin_out_positivos <- 0
train_error_sin_out_negativos <- 0

for (i in 1:nrow(train_sin_out)){
  if (train_sin_out$diff_2[i]>0) 
    train_error_sin_out_positivos <- train_error_sin_out_positivos + train_sin_out$diff_2[i]
  else 
    train_error_sin_out_negativos <- train_error_sin_out_positivos + train_sin_out$diff_2[i]
}

# Diferencia entre productos por exceso y productos por defecto
train_error_sin_out_positivos - train_error_sin_out_negativos
# Error en porcentaje
error_producto_sin_out = (train_error_sin_out/sum(train_sin_out$S9))*100

RMSLE_mod3.3 <- rmsle(train_sin_out$S9, train_sin_out$prediccion)

# Con outliers
train_con_out %>% ggplot(aes(x=diff))+
  geom_density()+
  scale_x_continuous(limits=c(-100,100))

# Error en el número de productos en valor absoluto: productos por exceso + productos por defecto
train_error_con_out <- sum(train_con_out$diff_2_abs)

# Contamos los productos por exceso y por defecto
train_error_con_out_positivos <- 0
train_error_con_out_negativos <- 0

for (i in 1:nrow(train_con_out)){
  if (train_con_out$diff_2[i]>0) 
    train_error_con_out_positivos <- train_error_con_out_positivos + train_con_out$diff_2[i]
  else 
    train_error_con_out_negativos <- train_error_con_out_positivos + train_con_out$diff_2[i]
}

# Diferencia entre productos por exceso y productos por defecto
train_error_con_out_positivos - train_error_con_out_negativos
# Error en porcentaje
error_producto_con_out = (train_error_con_out/sum(train_con_out$S9))*100

RMSLE_mod3.4 <- rmsle(train_con_out$S9, train_con_out$prediccion)



# Trabajo con el archivo de test

dt_test <- fread("Data/test.csv")
dt_test <- dt_test[, .(Semana,Cliente_ID,Producto_ID)]
dt_test <- dt_test[Semana==10]
df_test <- as_tibble(dt_test)
rm(dt_test)
df_test <- distinct(df_test)
df_pred <- tablon %>%  
  rowwise() %>% 
  mutate(pred = floor(mean(c_across(S3:S9)))) %>% 
  select(Cliente_ID, Producto_ID, pred)

df_result <- df_test %>% 
  inner_join(df_pred, by = c("Cliente_ID", "Producto_ID"))
  
  

