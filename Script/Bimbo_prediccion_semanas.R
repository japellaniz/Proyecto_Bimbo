

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
dt_train <- fread("Data/train.csv")
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

# Creación de partición train-test ####
indice <- createDataPartition(tablon$S9, p = 0.8, times = 1, list=FALSE)
datostra = tablon[ indice,]
datostst = tablon[-indice,]

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MODELOS DE PREDICCIÓN ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Siguiendo el principio de la navaja de Occam optamos por la adopción
# de los modelos más sencillos posibles que den buenos resultados.
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modelo 1. Normalizando entre 0 y 1 ####
# Normalizamos entre 0 y 1 los valores de todas las semana.
# Obtenemos la media para cada Cliente-Producto y calculamos la diferencia con la semana 9.
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
train_mean <- tablon

maximo <- max(train_mean[,4:10])

# Normalización
for (i in 4:10){
  train_mean[i]<-train_mean[i]/maximo
}

train_mean <- train_mean %>% 
  rowwise() %>% 
  mutate(media = mean(c_across(S3:S8))) %>% 
  mutate(diff = S9-media)

train_mean %>% ggplot(aes(fill=c(S9,media))) + 
  geom_density(aes(x=media),alpha = 0.5,fill = "orangered2") +
  geom_density(aes(x=S9),alpha = 0.5,fill = "gray50") +
  scale_x_continuous(limits=c(0,0.02))+
  theme_minimal()+
  labs(x = "Demanda normalizada",
       y = "Densidad",
       title = "Modelo media-normalizado",
       subtitle = "Comparativa Semana 9 vs Media") 

# Las dos curvas siguen el mismo patrón pero la predicción va adelantada por lo que 
# para valores bajos de demanda se predecirá de menos y para valores altos
# de más. Aplicamos una corrección equivalente a la media de la diferencia 
# entre las dos curvas.

train_mean <- train_mean %>% 
  mutate(Prediccion = media + mean(train_mean$diff)) %>% 
  mutate(diff_2 = S9 - Prediccion)

train_mean %>% ggplot(aes(fill=c(S9,Prediccion))) + 
  geom_density(aes(x=Prediccion),alpha = 0.5,fill = "orangered2") +
  geom_density(aes(x=S9),alpha = 0.5,fill = "gray50") +
  scale_x_continuous(limits=c(0,0.02))+
  theme_minimal()+
  labs(x = "Demanda normalizada",
       y = "Densidad",
       title = "Modelo media-normalizado-corregido",
       subtitle = "Comparativa Semana 9 vs Prediccion") 

smr <- summary(train_mean$diff_2) 
sd_mean <- sd(train_mean$diff_2)
var_mean <- var(train_mean$diff_2)


p1<-train_mean %>% ggplot(aes(x=id, y=diff_2*100))+
  geom_point(alpha = 1/3) +
  geom_line(alpha = 1/6)+
  scale_y_continuous(limits=c(-50,50))+
  theme_minimal()+
  labs(x = "id Producto-Cliente",
       y = " % Desviación de la predicción",
       title = "Modelo media-normalizado corregido")

p2<-train_mean %>% ggplot(aes(x=diff_2))+
  geom_density(color = "gray50") +
  scale_x_continuous(limits=c(-0.005,0.005))+
  theme_minimal()+
  labs(x = "Desviación de la predicción",
       y = "Densidad",
       title = "Modelo media-normalizado corregido")

p3<-train_mean %>% ggplot(aes(fill=c(S9,Prediccion)))+
  geom_density(aes(x=Prediccion),alpha = 0.5,fill = "orangered2") +
  geom_density(aes(x=S9),alpha = 0.5,fill = "gray50") +
  scale_x_continuous(limits=c(0,0.02))+
  theme_minimal()+
  labs(x = "Demanda normalizada",
       y = "Densidad",
       title = "Modelo media-normalizado corregido",
       subtitle = "Comparativa Semana 9 vs Predicción")

p4<-train_mean %>% ggplot(aes(y=diff_2))+
  geom_boxplot(color = "orangered") +
  scale_y_continuous(limits=c(-0.005,0.005))+
  theme_minimal()+
  labs(y = "Desviación de la predicción",
       title = "Modelo media-normalizado corregido")

final_plot <- ggarrange(p1, p2, p3, p4, common.legend = TRUE)
final_plot

RMSLE_mod1 <- rmsle(train_mean$S9, train_mean$Prediccion)
# 0,00449

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modelo 2: Normalizando con logaritmos neperianos. ####
# Obtenemos la media para cada Cliente-Producto y calculamos la diferencia con la semana 9.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
train_log <- tablon

for (i in 4:10){
  train_log[i] <- log(train_log[i])
}

train_log$S3[train_log$S3==-Inf]<-0
train_log$S4[train_log$S4==-Inf]<-0
train_log$S5[train_log$S5==-Inf]<-0
train_log$S6[train_log$S6==-Inf]<-0
train_log$S7[train_log$S7==-Inf]<-0
train_log$S8[train_log$S8==-Inf]<-0
train_log$S9[train_log$S9==-Inf]<-0


train_log <- train_log %>% 
  rowwise() %>% 
  mutate(log_media = mean(c_across(S3:S8)))

#train_log$log_media[train_log$log_media==-Inf]<-0

train_log <- train_log %>% 
  mutate(diff = S9-log_media)

#train_log$diff[train_log$diff==-Inf]<-0

summary(train_log$diff) 
sd_log <- sd(train_log$diff)

train_log %>% ggplot(aes(x=id, y=diff))+
  geom_point(color = "red") +
  geom_line(color = "red")+
  scale_y_continuous(limits=c(-10,10))+
  theme_minimal()+
  labs(x = "id Producto-Cliente",
       y = " % Desviación de la predicción",
       title = "Modelo log-normalizado")


train_log %>% ggplot(aes(y=diff))+
  geom_boxplot(color = "red") +
  scale_y_continuous(limits=c(-0.00005,0.00005))+
  theme_minimal()+
  labs(y = "Desviación de la predicción",
       title = "Modelo log-normalizado")


train_log %>% ggplot()+
  geom_density(aes(x=S9),color= "green")+
  geom_density(aes(x=log_media),color="red")+
  scale_x_continuous(limits=c(0,10))

RMSLE_mod2 <- rmsle(train_log$S9, train_log$log_media)
# 0,257018

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modelo 3: Datos sin normalizar ####
# Obtenemos la media para cada Cliente-Producto y calculamos la diferencia con la semana 9.
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
train_normal <- tablon %>% 
  rowwise() %>% 
  mutate(media = mean(c_across(S3:S8))) %>% 
  mutate(diff = S9-media)

summary(train_normal$diff) 
sd(train_normal$diff)

train_normal %>% ggplot(aes(x=diff))+
  geom_density()+
  scale_x_continuous(limits=c(-12,12))

train_normal %>% ggplot()+
  geom_density(aes(x=S9),color= "green")+
  geom_density(aes(x=media),color="red")+
  scale_x_continuous(limits=c(0,10))

RMSLE_mod3 <- rmsle(train_normal$S9, train_normal$media)
# 0,464071

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modelo 3: Calculo del error ####
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
  geom_density()+
  scale_x_continuous(limits=c(-20,20))

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
  scale_x_continuous(limits=c(-50,50))

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

