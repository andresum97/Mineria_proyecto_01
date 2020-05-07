library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians

library(fpc)
library(plyr)
library(dplyr)
library(factoextra)
library(cluster)
library(openxlsx)

#set.seed(123)

#fallecidos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)

#importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
#importaciones <- na.omit(importaciones)

#datos<-fallecidos
#datosImportantes <- datos[c('año_ocu','día_ocu','hora_ocu','g_hora','g_hora_5','día_sem_ocu','sexo_per','edad_quinquenales','mayor_menor','marca_veh','g_modelo_veh','tipo_eve','fall_les','int_o_noint')]
#km<-kmeans(datosImportantes,3)
#datos$grupo<-km$cluster

#g1<- datos[datos$grupo==1,]
#g2<- datos[datos$grupo==2,]
#g3<- datos[datos$grupo==3,]


#trainTree <- datos[c('día_ocu','hora_ocu','g_hora','g_hora_5','día_sem_ocu','sexo_per','edad_quinquenales','mayor_menor','marca_veh','g_modelo_veh','grupo')]
#porciento <- 70/100
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
#test1$prediccion<-columnaMasAlta #Se le aÃ±ade al grupo de prueba el valor de la predicciÃ³n
#View(test1)
#cfm<-confusionMatrix(table(test1$prediccion, test1$grupo))
#cfm




fallecidos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)
datos <- fallecidos
datos <- datos[c('día_ocu', 'g_hora_5', 'sexo_per', 'edad_quinquenales', 'marca_veh', 'g_modelo_veh', 'tipo_eve', 'fall_les', 'int_o_noint')]
set.seed(123)

datos <- na.omit(datos)
datos <- filter(datos, datos$marca_veh != 999, datos$sexo_per != 9, datos$g_modelo_veh != 99, datos$tipo_eve != 99, datos$int_o_noint != 9)
porcentaje <- 0.7

km<-kmeans(datos, 3)
datos$grupo <- km$cluster

corte <- sample(nrow(datos), nrow(datos) * porcentaje)
train <- datos[corte, ]
test <- datos[-corte, ]

g1 <- datos[datos$grupo==1, ]
g2 <- datos[datos$grupo==2, ]
g3 <- datos[datos$grupo==3, ]


