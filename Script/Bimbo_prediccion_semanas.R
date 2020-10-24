


{
  library(tidyverse)
  library(data.table)
  library(funModeling)
  library(caret)
  library(magrittr)
  library(xgboost)
  library(randomForest)
}
set.seed(43)


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
df_train<-as_tibble(train)
df_sample <- df_train %>% select(Cliente_ID,Producto_ID) %>% 
  distinct() %>%
  sample_frac(0.01)  
rm(train)
df_sample <- df_train %>% inner_join(df_sample, by = c("Cliente_ID", "Producto_ID"))
rm(df_train)
tablon <- df_sample %>% spread(Semana,Demanda_agg)

colnames(tablon) <- c("Cliente_ID", "Producto_ID", "S3", "S4", "S5", "S6", "S7", "S8", "S9")
saveRDS(tablon, "Data/tablon")
write_csv(tablon,"Data/tablon.csv")
write_csv(df_sample,"Data/df_sample.csv")



train_mean <- tablon

maximo <- max(train_mean[,3:8])
# Normalización
train_mean[3]<-train_mean[3]/maximo
train_mean[4]<-train_mean[4]/maximo
train_mean[5]<-train_mean[5]/maximo
train_mean[6]<-train_mean[6]/maximo
train_mean[7]<-train_mean[7]/maximo
train_mean[8]<-train_mean[8]/maximo
train_mean[9]<-train_mean[9]/maximo




# library(caret)
# 
# preproc1 <- preProcess(tablon[,c(3:9)], method=c("range"))
# norm1 <- predict(preproc1, tablon[,c(3:9)])
# summary(norm1)
# tablon[3]<-norm1[1]
# tablon[4]<-norm1[2]
# tablon[5]<-norm1[3]
# tablon[6]<-norm1[4]
# tablon[7]<-norm1[5]
# tablon[8]<-norm1[6]
# tablon[9]<-norm1[7]


train_mean <- train_mean %>% 
  rowwise() %>% 
  mutate(media = mean(c_across(S3:S8))) %>% 
  mutate(diff = S9-media)
 
summary(train_mean$diff) 

train_mean %>% ggplot(aes(x=diff))+
  geom_density()



train_log <- train
train_log[3] <- log(train_log[3])
train_log[4] <- log(train_log[4])
train_log[5] <- log(train_log[5])
train_log[6] <- log(train_log[6])
train_log[7] <- log(train_log[7])
train_log[8] <- log(train_log[8])
train_log[9] <- log(train_log[9])

train_log$S3[train_log$S3==-Inf]<-0
train_log$S4[train_log$S4==-Inf]<-0
train_log$S5[train_log$S5==-Inf]<-0
train_log$S6[train_log$S6==-Inf]<-0
train_log$S7[train_log$S7==-Inf]<-0
train_log$S8[train_log$S8==-Inf]<-0
train_log$S9[train_log$S9==-Inf]<-0




train_log <- train_log %>% 
  rowwise() %>% 
  mutate(log_media = log(mean(c_across(S3:S8))))

train_log$log_media[train_log$log_media==-Inf]<-0

train_log <- train_log %>% 
  mutate(diff = S8-log_media)
train_log$diff[train_log$diff==-Inf]<-0

summary(train_log$diff) 

train_log %>% ggplot(aes(x=diff))+
  geom_density()


train_normal <- train %>% 
  rowwise() %>% 
  mutate(media = mean(c_across(S3:S8))) %>% 
  mutate(diff = S9-media)

summary(train_normal$diff) 

train_normal %>% ggplot(aes(x=diff))+
  geom_density()
