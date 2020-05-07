library(dplyr)
library(plyr)
library(caret)
library(corrplot)
library(ggplot2)
library(e1071)
library(rpart)
library(rpart.plot)
library(knitr)


fallecidos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)
datos <- fallecidos
datos <- datos[c('día_ocu', 'g_hora_5', 'sexo_per', 'edad_quinquenales', 'marca_veh', 'g_modelo_veh', 'tipo_eve', 'fall_les', 'int_o_noint')]
set.seed(123)

datos <- na.omit(datos)
datos <- filter(datos, datos$marca_veh != 999, datos$sexo_per != 9, datos$g_modelo_veh != 99, datos$tipo_eve != 99, datos$int_o_noint != 9)

datos$fall_les <- mapvalues(datos$fall_les, c(1, 2), c('Fallecido', 'Lesionado'))
datos <- cbind(datos, dummy(datos$fall_les))

porcentaje <- 0.7

corte <- sample(nrow(datos), nrow(datos) * porcentaje)
train <- datos[corte, ]
test <- datos[-corte, ]

head(datos)

modelo <- glm(datosFallecido~., data = train[, c(1:7, 9, 10)], family = binomial(), maxit = 100)
summary(modelo)

pred <- predict(modelo, newdata = test[, c(1:7, 9)], type = "response")
prediccion <- ifelse(pred >= 0.5, 1, 0)
prediccion <- mapvalues(prediccion, c(0, 1), c('Lesionado', 'Fallecido'))

confusionMatrix(as.factor(test$fall_les), as.factor(prediccion))



