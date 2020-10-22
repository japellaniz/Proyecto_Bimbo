

{
  library(tidyverse)
  library(data.table)
  library(funModeling)
  library(caret)
  library(magrittr)
  library(xgboost)
}

dt_train <- fread("Data/train.csv")
# Tarda en leer el train.csv así que lo guardamos
#saveRDS(dt_train,"Data/dt_train")
dt_train<-readRDS("Data/dt_train")

train<-dt_train[, .(Semana,Cliente_ID,Producto_ID, Demanda_uni_equil)]
rm(dt_train)
train<-train[, .(Demanda_agg=sum(Demanda_uni_equil)), .(Semana, Cliente_ID,Producto_ID)]
train<-train[, .(Semana,Demanda_agg,num_sem=.N),.(Cliente_ID,Producto_ID)]
train<-train[num_sem==7]
train<-train[,num_sem:=NULL]
df_train<-as.tibble(train)
df_sample <- df_train %>% select(Cliente_ID,Producto_ID) %>% 
  distinct() %>%
  sample_frac(0.01)  
df_sample <- df_train %>% inner_join(df_sample, by = c("Cliente_ID", "Producto_ID"))

tablon <- df_sample %>% spread(Semana,Demanda_agg)

colnames(tablon) <- c("Cliente_ID", "Producto_ID", "S3", "S4", "S5", "S6", "S7", "S8", "S9")

train <- tablon %>% select(1:8)
test <- tablon %>% select(c(1,2,9))


Bimbo_rf <- randomForest(S8 ~ ., data = train, 
                         do.trace = TRUE, ntree = 200, mtry = 5, replace = TRUE)
Bimbo_rf
plot(Bimbo_rf)



