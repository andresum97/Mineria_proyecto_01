setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos/Proyecto-01/Mineria_proyecto_01")
library(e1071)
library(caret)
library(plyr)
library(dplyr)
library(lattice)
library(rpart)


fallecidos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)
datos<-fallecidos
datos <- datos[c('día_ocu', 'g_hora_5', 'sexo_per', 'edad_quinquenales', 'tipo_veh', 'color_veh','g_modelo_veh', 'tipo_eve', 'fall_les', 'int_o_noint')]
set.seed(123)
#head(datos)
datos <- na.omit(datos)
datos <- filter(datos, datos$sexo_per != 9, datos$g_modelo_veh != 99, datos$tipo_veh != 99, datos$color_veh != 99, datos$tipo_eve != 99, datos$int_o_noint != 9)

datos$fall_les <- mapvalues(datos$fall_les, c(1, 2), c('Fallecido', 'Lesionado'))

porcentaje<-0.7
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

modelo<-naiveBayes(as.factor(fall_les)~.,data=datos)
predBayes<-predict(modelo, newdata = test[,1:9])
confusionMatrix(table(predBayes,test$fall_les))



#km<-kmeans(datos,3)
#trainImportantes$grupo<-km$cluster
#trainImportantes$grupo <- mapvalues(trainImportantes$grupo, c(1,2,3), c("Intermedio","Barato","caro"))