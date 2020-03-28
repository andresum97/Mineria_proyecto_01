
library(fpc)
library(dplyr)
library(factoextra)
library(cluster)

importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
importaciones <- na.omit(importaciones)

numImportaciones <- select(importaciones, Modelo.del.Vehiculo, Centimetros.Cubicos, Asientos, Puertas, Tonelaje, Valor.CIF, Impuesto, Anio, Mes, Dia, DiaSem)
# Converti a numero de ser posible
numImportaciones$Modelo.del.Vehiculo <- as.numeric(numImportaciones$Modelo.del.Vehiculo)
numImportaciones$Centimetros.Cubicos <- as.numeric(numImportaciones$Centimetros.Cubicos)
numImportaciones$Asientos <- as.numeric(numImportaciones$Asientos)
numImportaciones$Puertas <- as.numeric(numImportaciones$Puertas)
numImportaciones$Tonelaje <- as.numeric(numImportaciones$Tonelaje)
numImportaciones$Valor.CIF <- as.numeric(numImportaciones$Valor.CIF)
numImportaciones$Impuesto <- as.numeric(numImportaciones$Impuesto)
numImportaciones$Anio <- as.numeric(numImportaciones$Anio)
numImportaciones$Mes <- as.numeric(numImportaciones$Mes)
numImportaciones$Dia <- as.numeric(numImportaciones$Dia)
numImportaciones$DiaSem <- as.numeric(numImportaciones$DiaSem)

#---------------------- Grafica de codo para determinar el numero de clusters

wss <- (nrow(numImportaciones) - 1) * sum(apply(numImportaciones, 2, var))
wss
for (i in 2:10) 
  wss[i] <- sum(kmeans(numImportaciones, centers=i)$withinss)

plot(1:10, wss, type="b", main = "Gráfica de codo para cantidad de clústers", xlab="Número de clusters",  ylab="Costo de fusión de clústers")


mCompleto<-numImportaciones[complete.cases(numImportaciones), ]
km<-kmeans(numImportaciones, 4)
importaciones$KGroup<-km$cluster

##  Gráficas
plotcluster(numImportaciones, km$cluster) #grafica la ubicación de los clusters
fviz_cluster(km, data = numImportaciones, geom = "point", ellipse.type = "norm") #gráfica con mejor visualización

##  Método de la silueta

silkm <- silhouette(km$cluster, dist(numImportaciones))
mean(silkm[, 4]) # Valor obtenido: 0.8283339

head(importaciones$KGroup)
## tabla de frecuencia proporcional 
g1 <- importaciones[importaciones$KGroup==1, ]
g1 <- data.frame(g1$Marca)
g1Freq <- as.data.frame(prop.table(table(g1))*100)
# View(g1Freq)

g2 <- importaciones[importaciones$KGroup==2, ]
g2 <- data.frame(g2$Marca)
g2Freq <- as.data.frame(prop.table(table(g2)) * 100)
# View(g2Freq)

g3 <- importaciones[importaciones$KGroup==3, ]
g3 <- data.frame(g3$Marca)
g3Freq <- as.data.frame(prop.table(table(g3)) * 100)
# View(g3Freq)

g4 <- importaciones[importaciones$KGroup==4, ]
g4 <- data.frame(g4$Marca)
g4Freq <- as.data.frame(prop.table(table(g4)) * 100)
# View(g4Freq)

colnames(g1Freq)[colnames(g1Freq) == "g1"] <- "Marca"
colnames(g2Freq)[colnames(g2Freq) == "g2"] <- "Marca"
colnames(g3Freq)[colnames(g3Freq) == "g3"] <- "Marca"
colnames(g4Freq)[colnames(g4Freq) == "g4"] <- "Marca"

colnames(g4Freq)
merge <- merge(g1Freq, g2Freq, by = "Marca", all.x = T, all.y = F)
View(merge)
groupList <- list(g1, g2, g3, g4)
Reduce(function(x, y) merge(x, y, by = "Var1", all.x = TRUE, all.y = FALSE), groupList)

