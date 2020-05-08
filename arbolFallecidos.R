library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n�mero de clusters �ptimo
library(factoextra) #Para hacer gr�ficos bonitos de clustering
library(tidyr)
library(splitstackshape)
library(plyr)

datos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)

datosImportantes <- datos[c('d�a_ocu', 'g_hora_5', 'sexo_per', 'edad_quinquenales', 'marca_veh', 'g_modelo_veh', 'tipo_eve', 'fall_les', 'int_o_noint')]
set.seed(123)

datos <- na.omit(datos)
datos <- filter(datos, datos$edad_quinquenales !=18, datos$marca_veh != 999, datos$marca_veh != 99, datos$sexo_per != 9, 
                datos$g_modelo_veh != 99, datos$g_modelo_veh != 999, datos$tipo_eve != 99, datos$int_o_noint != 9)


datosImportantes <- na.omit(datosImportantes)
datosImportantes <- filter(datosImportantes, datosImportantes$edad_quinquenales !=18, datosImportantes$marca_veh != 999, datosImportantes$marca_veh != 99, datosImportantes$sexo_per != 9, datosImportantes$g_modelo_veh != 99, datosImportantes$g_modelo_veh != 999, datosImportantes$tipo_eve != 99, datosImportantes$int_o_noint != 9)
porcentaje <- 0.7

km<-kmeans(datosImportantes, 3)
datos$grupo <- km$cluster

#g1 <- datos[datos$grupo==1, ]
#g2 <- datos[datos$grupo==2, ]
#g3 <- datos[datos$grupo==3, ]

trainTree <- datos[c('g_hora_5','d�a_sem_ocu','sexo_per','mayor_menor','g_modelo_veh','grupo')]
trainRowsNumber<-sample(1:nrow(trainTree),porcentaje*nrow(trainTree))

train <- trainTree[trainRowsNumber, ]
test <- trainTree[-trainRowsNumber, ]

dt_model<-rpart(as.factor(train$fall_les)~.,train,method = "class")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)
prediccion <- predict(dt_model, newdata = test1[,1:7])

#set.seed(2)
#trainRowsNumber<-sample(1:nrow(datos),porciento*nrow(datos))
#train1<-datos[trainRowsNumber,]
#test1<-datos[-trainRowsNumber,]
#dt_model<-rpart(grupo~.,train1,method = "class")
#plot(dt_model);text(dt_model)
#prp(dt_model)
#rpart.plot(dt_model)
#prediccion <- predict(dt_model, newdata = test1[,1:7])

#columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
#test1$prediccion<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción
#View(test1)
#cfm<-confusionMatrix(table(test1$prediccion, test1$grupo))
#cfm

