# Tratamiento como data.table y modelado con random forest

library(data.table)
library(randomForest)

#train<-fread("Data/train.csv"
#             ,select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Demanda_uni_equil"))
#saveRDS(train,"Data/train")
train<-readRDS("Data/train")

# Evaluación con RMSLE: usamos función exp(mean(log(X)+1))-1 que minimiza la métrica
# Tomamos las filas de las semanas 3-7, agrupando por Cliente_ID+Producto_ID y obtenemos 
# una nueva variable target transformada.
#train_transform <- train[Semana<8, .(target=expm1(mean(log1p(Demanda_uni_equil)))), .(Cliente_ID,Producto_ID)]
#saveRDS(train_transform,"Data/train_transform")
train_transform<-readRDS("Data/train_transform")


# Nos quedamos con la parte de train correspondiente a las semanas 8 y 9
train <- train[Semana>=8,]

# Juntamos las dos tablas
train <- merge(train, train_transform, by = c("Cliente_ID","Producto_ID"), all.x =TRUE) 

# Quitamos los NAs
train[is.na(target),target:=0]

######################################################################
# Radom Forest
######################################################################
# Separamos el dataset en train y test para el random forest
train_rf <- train[Semana==8,]
test_rf <- train[Semana ==9,]

# Lo convertimos a data frame y muestreamos
train_rf_df <- as.data.frame(train_rf)
train_rf_df <- train_rf_df[sample(nrow(train_rf_df), nrow(train_rf_df)*0.02),]
                           
                           
Bimbo.rf <- randomForest(target ~ ., data = train_rf_df, 
                         do.trace = TRUE, ntree = 200, mtry = 5, replace = TRUE)
Bimbo.rf

Bimbo.rf.0.02<-Bimbo.rf
plot(Bimbo.rf)
saveRDS(Bimbo.rf.0.02,"Data/Bimbo_rf_0_02")

test_rf_df <- as.data.frame(test_rf)
test_rf_df <- test_rf_df[sample(nrow(test_rf_df), nrow(test_rf_df)*0.001),]
pred <- predict(Bimbo.rf.0.02, )
###############################################################################

################################################################################
# H2O
################################################################################
library(h2o)
h2o.init(nthreads=-1,max_mem_size = '5G')
dev<-as.h2o(train[Semana==8,],destination_frame = "dev.hex")
val<-as.h2o(train[Semana==9,],destination_frame = "val.hex")

dev[1:10,]

predictors<-colnames(val)[!colnames(val) %in% c("target","Demanda_uni_equil","Semana")]
g<-h2o.gbm(
  training_frame = dev,      ## H2O frame holding the training data
  validation_frame = val,  ## extra holdout piece for three layer modeling
  x=predictors,                 ## this can be names or column numbers
  y="target",                   ## target: using the logged variable created earlier
  model_id="gbm1",              ## internal H2O name for model
  ntrees = 50,                  ## use fewer trees than default (50) to speed up training
  learn_rate = 0.1,             ## lower learn_rate is better, but use high rate to offset few trees
  score_tree_interval = 3,      ## score every 3 trees
  sample_rate = 0.5,            ## use half the rows each scoring round
  col_sample_rate = 0.8)        ## use 4/5 the columns to decide each split decision

## look at model diagnostics
summary(g)
## specifically look at validation RMSE (sqrt of MSE)
(h2o.mse(g,valid=T))^0.5  




