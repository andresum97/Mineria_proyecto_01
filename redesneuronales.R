library(caret)
library(caret)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)
library(dplyr)
library(corrplot)
library(dplyr)
library(plyr)
library(e1071)
library(rpart)
library("rpart.plot")
library(knitr)



# ------------------------------------------------------------------------------------------

fallecidos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)
datos <- fallecidos
datos <- datos[c('día_ocu', 'g_hora_5', 'sexo_per', 'edad_quinquenales', 'marca_veh', 'g_modelo_veh', 'tipo_eve', 'fall_les', 'int_o_noint')]
set.seed(123)

datos <- na.omit(datos)
datos <- filter(datos, datos$marca_veh != 999, datos$sexo_per != 9, datos$g_modelo_veh != 99, datos$tipo_eve != 99, datos$int_o_noint != 9)

porcentaje <- 0.7

corte <- sample(nrow(datos), nrow(datos) * porcentaje)
train <- datos[corte, ]
test <- datos[-corte, ]

NB <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
nnodos='10'

modeloWeka <- NB(fall_les~., data=train, control=Weka_control(H=nnodos, N=7000, G=TRUE), options=NULL)
test$prediccionWeka <- predict(modeloWeka, newdata = test[, c(1:7, 9)])
prediccion <- ifelse(test$prediccionWeka > 1.5, 2, 1)

confusionMatrix(as.factor(prediccion), as.factor(test$fall_les))

# ------------------------------------------------------------------------------------------

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

count(g1$fall_les)
count(g2$fall_les)
count(g3$fall_les)

head(datos)
modeloWeka <- NB(grupo~., data=train[, c(1:7, 9, 10)], control=Weka_control(H=nnodos, N=5000, G=TRUE), options=NULL)
test$prediccionWeka <- predict(modeloWeka, newdata = test[, c(1:7, 9)])
prediccion <- round(test$prediccionWeka)

confusionMatrix(as.factor(prediccion), as.factor(test$grupo))




