

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



test<-train[Semana==9]
train<-train[Semana<9]

