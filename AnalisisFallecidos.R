library(corrplot)
library(dplyr)
library(randomcoloR)
library("ggplot2")
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n�mero de clusters �ptimo
library(factoextra) #Para hacer gr�ficos bonitos de clustering


#=================================================================================
fallecidos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)
# View(fallecidos)
# summary(fallecidos)
# fallecidos$uno<-1
# yearplot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$a�o_ocu), FUN=sum)
# View(yearplot)
# 
# barplot(yearplot$x,names.arg=yearplot$Category,xlab="a�o",ylab="Cantidad", main = "Grafico de a�o de accidentes",col=c("red","yellow","blue","orange"))
# 
# 
# diaocu_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$d�a_ocu), FUN=sum)
# View(diaocu_plot)
# diaocucol <- distinctColorPalette(length(diaocu_plot$Category))
# barplot(diaocu_plot$x,names.arg=diaocu_plot$Category,xlab="dia del mes",ylab="Cantidad", main = "Grafico de dia de accidentes",col=diaocucol)
# 
# 
# horaocu_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$hora_ocu), FUN=sum)
# col_hora <- distinctColorPalette(length(horaocu_plot$Category))
# barplot(horaocu_plot$x,names.arg=horaocu_plot$Category,xlab="hora", main = "Grafico de hora de accidentes",col=col_hora)
# 
# fallecidos$g_hora <- mapvalues(fallecidos$g_hora, c(1,2,3,4), c("00:00 a 05:59","06:00 a 11:59","12:00 a 17:59","18:00 a 23:59"))
# g_hora_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$g_hora), FUN=sum)
# col_g_hora <- distinctColorPalette(length(g_hora_plot$Category))
# barplot(g_hora_plot$x,names.arg=g_hora_plot$Category, main = "Grafico de hora de accidentes",col=col_g_hora)
# 
# fallecidos$g_hora_5 <- mapvalues(fallecidos$g_hora_5, c(1,2,3), c("Ma�ana","Tarde","Noche"))
# g_hora5_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$g_hora_5), FUN=sum)
# col_g_hora5 <- distinctColorPalette(length(g_hora5_plot$Category))
# barplot(g_hora5_plot$x,names.arg=g_hora5_plot$Category, main = "Grafico de horario de accidentes",col=col_g_hora5)
# 
# fallecidos$mes_ocu <- mapvalues(fallecidos$mes_ocu, c(1,2,3,4,5,6,7,8,9,10,11,12), c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"))
# mes_ocu_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$mes_ocu), FUN=sum)
# col_mes_ocu <- distinctColorPalette(length(mes_ocu_plot$Category))
# barplot(mes_ocu_plot$x,names.arg=mes_ocu_plot$Category, main = "Grafico de accidentes por mes",col=col_mes_ocu)
# 
# fallecidos$d�a_sem_ocu <- mapvalues(fallecidos$d�a_sem_ocu, c(1,2,3,4,5,6,7), c("lunes","martes","miercoles","jueves","viernes","sabado","domingo"))
# dia_sem_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$d�a_sem_ocu), FUN=sum)
# col_dia_sem_ocu <- distinctColorPalette(length(dia_sem_plot$Category))
# barplot(dia_sem_plot$x,names.arg=dia_sem_plot$Category, main = "Grafico de accidentes por dia de la semana",col=col_dia_sem_ocu)
# 
# fallecidos$depto_ocu <- mapvalues(fallecidos$depto_ocu, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22), c("Guatemala","El Progreso","Sacatep�quez","Chimaltenango","Escuintla","Santa Rosa","Solol�","Totonicap�n","Quetzaltenango","Suchitep�quez","Retalhuleu","San Marcos","Huehuetenango","Quich�","Baja Verapaz","Alta Verapaz","Pet�n","Izabal","Zacapa","Chiquimula","Jalapa","Jutiapa"))
# dept_ocu_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$depto_ocu), FUN=sum)
# col_dept_ocu <- distinctColorPalette(length(dept_ocu_plot$Category))
# barplot(dept_ocu_plot$x,names.arg=dept_ocu_plot$Category,las=2,main = "Grafico de accidentes por departamento",col= col_dept_ocu)
# 
# fallecidos$sexo_per <- mapvalues(fallecidos$sexo_per, c(1,2,9), c("Hombre","Mujer","Ignorado"))
# sexo_per_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$sexo_per), FUN=sum)
# col_sexo_per <- distinctColorPalette(length(sexo_per_plot$Category))
# barplot(sexo_per_plot$x,names.arg=sexo_per_plot$Category, main = "Grafico de victimas por sexo",col=col_sexo_per)
# 
# 
# edad_per_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$edad_per), FUN=sum)
# col_edad_per <- distinctColorPalette(length(edad_per_plot$Category))
# barplot(edad_per_plot$x,names.arg=edad_per_plot$Category, main = "Grafico de victimas por edad",col=col_edad_per)
# 
# fallecidos$g_edad_80ym�s <- mapvalues(fallecidos$g_edad_80ym�s, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), c("Menor de 15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-80","80 y m�s","Ignorado"))
# g_edad80_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$g_edad_80ym�s), FUN=sum)
# col_g_edad80 <- distinctColorPalette(length(g_edad80_plot$Category))
# barplot(g_edad80_plot$x,names.arg=g_edad80_plot$Category, las=2,main = "Grafico de victimas por grupo de edad hasta 80 a�os",col=col_g_edad80)
# 
# fallecidos$g_edad_60ym�s <- mapvalues(fallecidos$g_edad_60ym�s, c(1,2,3,4,5,6,7,8,9,10,11,12), c("Menor de 15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60 y m�s","Ignorado"))
# g_edad60_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$g_edad_60ym�s), FUN=sum)
# col_g_edad60 <- distinctColorPalette(length(g_edad60_plot$Category))
# barplot(g_edad60_plot$x,names.arg=g_edad60_plot$Category, las=2,main = "Grafico de victimas por grupo de edad hasta 60 a�os",col=col_g_edad60)
# 
# fallecidos$edad_quinquenales <- mapvalues(fallecidos$edad_quinquenales, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-80","80 y m�s","Ignorado"))
# edad_quincenales_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$edad_quinquenales), FUN=sum)
# col_edad_quincenales <- distinctColorPalette(length(edad_quincenales_plot$Category))
# barplot(edad_quincenales_plot$x,names.arg=edad_quincenales_plot$Category, las=2,main = "Grafico de victimas por grupo de edad incluidos menores",col=col_edad_quincenales)
# 
# fallecidos$mayor_menor <- mapvalues(fallecidos$mayor_menor, c(1,2,9), c("Mayor","Menor","Ignorado"))
# mayor_menor_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$mayor_menor), FUN=sum)
# col_mayor_menor <- distinctColorPalette(length(mayor_menor_plot$Category))
# barplot(mayor_menor_plot$x,names.arg=mayor_menor_plot$Category, main = "Grafico de grupo de edad",col=col_mayor_menor)
# 
# fallecidos$tipo_veh <- mapvalues(fallecidos$tipo_veh, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,99), c("Autom�vil","Camioneta","Pick Up","Moto","Cami�n","Cabezal","Bus extraurbano","Jeep","Microb�s","Taxi","Panel","Bus Urbano","Tractor","Moto Taxi","Furg�n","Gr�a","Bus escolar","Bicicleta","Avioneta","Montacargas","Bus militar","Cuatrimoto","Furgoneta","Ignorado"))
# tipo_veh_plot<-aggregate(fallecidos$uno,by=list(Category=fallecidos$tipo_veh), FUN=sum)
# col_tipo_veh <- distinctColorPalette(length(tipo_veh_plot$Category))
# barplot(tipo_veh_plot$x,names.arg=tipo_veh_plot$Category, las=2,main = "Grafico de tipo de vehiculo involucrado",col=col_tipo_veh)
# 
# #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(plyr)
# library(dplyr)
# fallecidos$fall_les     <-mapvalues(fallecidos$fall_les, c(1,2), c("Fallecido","Lesionado"))
# fallecidos$int_o_noint  <-mapvalues(fallecidos$int_o_noint, c(1,2,9), c("Internado","No Internado","Ignorado"))
# fallecidos$tipo_eve     <-mapvalues(fallecidos$tipo_eve, c(1,2,3,4,5,6,7,8,99), c("Colision","Choque","Vuelco","Caida","Atropello","Derrape","Embarranco","Encuneto","Ignorado"))
# fallecidos$g_modelo_veh <-mapvalues(fallecidos$g_modelo_veh, c(1,2,3,4,5,6,99), c("1970-1979","1980-1989","1990-1999","2000-2009","2010-2019","Ignorado","Ignorado"))
# 
# fallecidos$color_veh    <-mapvalues(fallecidos$color_veh,c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,99),c('Rojo','Blanco','Azul','Gris','Negro','Verde','Amarillo','Celeste','Corinto','Caf�','Beige','Turquesa','Anaranjado','Morado','Rosado','Varios colores','Ignorado'
# ))
# #fallecidos$marca_veh    <-mapvalues(fallecidos$marca_veh,)
# #-------------------------------------------------------------------------------------------------------------------------------------------------
# variable <- table(fallecidos$int_o_noint)
# barplot(variable, main="Grafico de internados", col=c("red", "yellow", "green") )
# 
# variable1 <- table(fallecidos$fall_les)
# barplot(variable1, main="Grafico de Fallecidos", col=c("red", "yellow", "green") )
# 
# variable1 <- table(fallecidos$tipo_eve)
# barplot(variable1, main="Grafico tipo de evento", col=c("red", "yellow", "green","blue", "skyblue", "purple","orange", "beige", "gray") )
# 
# variable1 <- table(fallecidos$g_modelo_veh)
# barplot(variable1, main="Grafico modelo de vehiculo", col=c("red", "yellow", "green","blue", "skyblue", "purple","orange", "beige", "gray") )
# 
# variable1 <- table(fallecidos$color_veh)
# barplot(variable1, main="Grafico color de vehiculo", col=c("yellow", "orange", "blue","beige", "white", "brown","skyblue","maroon", "gray","goldenrod3","purple","black","red","pink","turquoise","aliceblue","green") )
# #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# setwd("C:/Users/alber/Documents/UVG/Septimo semestre/Mineria de Datos")
# fallecidos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)
#Clustering

datos<-fallecidos
datosImportantes <- datos[c('n�m_corre','a�o_ocu','d�a_ocu','hora_ocu','g_hora','g_hora_5','d�a_sem_ocu','sexo_per','edad_quinquenales','mayor_menor','marca_veh','g_modelo_veh','tipo_eve','fall_les','int_o_noint')]
#Para saber cual es el mejor numero de clusters
wss <- (nrow(datosImportantes)-1)*sum(apply(datosImportantes,2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosImportantes, centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

km<-kmeans(datosImportantes,3)
datos$grupo<-km$cluster

plotcluster(datosImportantes,km$cluster)#Visualizaci�n de las k-medias
#fviz_cluster(km, data = datosImportantes,geom = "point", ellipse.type = "norm")
#-----------------------------------------------------------------------------------------------
#Silueta de que tan bien hizo el cluster
#silkm<-silhouette(km$cluster,dist(datosImportantes))
#mean(silkm[,3])

#-------------------------------------------------------------------------------------------
library(openxlsx)
head(datos$grupo)
## tabla de frecuencia proporcional 
g1 <- datos[datos$grupo==1, ]
g1 <- data.frame(g1$tipo_veh)
g1Freq <- as.data.frame(prop.table(table(g1))*100)
# View(g1Freq)

g2 <- datos[datos$grupo==2, ]
g2 <- data.frame(g2$tipo_veh)
g2Freq <- as.data.frame(prop.table(table(g2))*100)
# View(g2Freq)

g3 <- datos[datos$grupo==3, ]
g3 <- data.frame(g3$tipo_veh)
g3Freq <- as.data.frame(prop.table(table(g3))*100)
# View(g3Freq)

colnames(g1Freq)[colnames(g1Freq) == "g1"] <- "tipo_veh"
colnames(g2Freq)[colnames(g2Freq) == "g2"] <- "tipo_veh"
colnames(g3Freq)[colnames(g3Freq) == "g3"] <- "tipo_veh"

merge1 <- merge(g1Freq, g2Freq, by = "tipo_veh", all.x = T, all.y = F)
merge <- merge(merge1, g3Freq, by = "tipo_veh", all.x = T, all.y = F)

colnames(merge) <- c("tipo_veh", "Freq1", "Freq2", "Freq3")

View(merge)
dfMerge <- as.data.frame(merge)
# write.xlsx(dfMerge, "Grupos.xlsx")

colnames(datos)
agg <- aggregate(datos[,c('n�m_corre','a�o_ocu','d�a_ocu','hora_ocu','g_hora','g_hora_5','d�a_sem_ocu','sexo_per','edad_quinquenales','mayor_menor','tipo_veh','marca_veh','color_veh','g_modelo_veh','tipo_eve','fall_les','int_o_noint')], by = list(datos$mes_ocu), FUN = mean)

View(agg)

df <- as.data.frame(agg)
write.xlsx(df, "Cluster_fallecidos.xlsx")











