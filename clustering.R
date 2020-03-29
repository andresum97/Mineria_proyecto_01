
library(fpc)
library(dplyr)
library(factoextra)
library(cluster)
library(openxlsx)

importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
importaciones <- na.omit(importaciones)

numImportaciones <- select(importaciones, Modelo.del.Vehiculo, Centimetros.Cubicos, Asientos, Puertas, Tonelaje, Valor.CIF, Impuesto)
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


importaciones$Modelo.del.Vehiculo <- as.numeric(importaciones$Modelo.del.Vehiculo)
importaciones$Centimetros.Cubicos <- as.numeric(importaciones$Centimetros.Cubicos)
importaciones$Asientos <- as.numeric(importaciones$Asientos)
importaciones$Puertas <- as.numeric(importaciones$Puertas)
importaciones$Tonelaje <- as.numeric(importaciones$Tonelaje)
importaciones$Valor.CIF <- as.numeric(importaciones$Valor.CIF)
importaciones$Impuesto <- as.numeric(importaciones$Impuesto)


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

merge <- merge(g1Freq, g2Freq, by = "Marca", all.x = T, all.y = F)
merge <- merge(merge, g3Freq, by = "Marca", all.x = T, all.y = F)
merge <- merge(merge, g4Freq, by = "Marca", all.x = T, all.y = F)
colnames(merge) <- c("Marca", "Freq1", "Freq2", "Freq3", "Freq4")

View(merge)
dfMerge <- as.data.frame(merge)
# write.xlsx(dfMerge, "Grupos.xlsx")

colnames(importaciones)
agg <- aggregate(importaciones[,c("Modelo.del.Vehiculo", "Centimetros.Cubicos", "Asientos", "Puertas", "Tonelaje", "Valor.CIF", "Impuesto")], by = list(importaciones$Marca), FUN = mean)
agg$Modelo.del.Vehiculo <- round(agg$Modelo.del.Vehiculo)
agg$Asientos <- round(agg$Asientos)
agg$Puertas <- round(agg$Puertas)

View(agg)

df <- as.data.frame(agg)
write.xlsx(df, "Cluster.xlsx")
