library(corrplot)
library(dplyr)
library(randomcoloR)
library("ggplot2")

importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"), sep = ',')
importaciones <- na.omit(importaciones)

View(importaciones)

fallecidos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)



# Pais de proveniencia
View(table(importaciones$Pais.de.Proveniencia))

# Aduana de ingreso
View(table(importaciones$Aduana.de.Ingreso))

# Fecha de la poliza
View(table(importaciones$Fecha.de.la.Poliza))


# Partida arancelaria
View(table(importaciones$Partida.Arancelaria))

# Modelo del vehiculo
View(table(importaciones$Modelo.del.Vehiculo))

# Marca
View(table(importaciones$Marca))

# Linea
View(table(importaciones$Linea))

# cm^3
View(table(as.numeric(importaciones$Centimetros.Cubicos[importaciones$Centimetros.Cubicos!=0])))
hist(as.numeric(importaciones$Centimetros.Cubicos[importaciones$Centimetros.Cubicos!=0]))
summary(as.numeric(importaciones$Centimetros.Cubicos[importaciones$Centimetros.Cubicos!=0]))
qqnorm(as.numeric(importaciones$Centimetros.Cubicos[importaciones$Centimetros.Cubicos!=0]))
qqline(as.numeric(importaciones$Centimetros.Cubicos[importaciones$Centimetros.Cubicos!=0]))

# Distintivo
View(table(importaciones$Distintivo))

# Tipo de vehiculo
View(table(importaciones$Tipo.de.Vehiculo))

# Tipo de importador
View(table(importaciones$Tipo.de.Importador))

# Tipo de combustible
View(table(importaciones$Tipo.Combustible))

# Asientos
hist(as.numeric(importaciones$Asientos))
qqnorm(as.numeric(importaciones$Asientos))
qqline(as.numeric(importaciones$Asientos))

# Puertas
hist(as.numeric(importaciones$Puertas))
qqnorm(as.numeric(importaciones$Puertas))
qqline(as.numeric(importaciones$Puertas))

# Tonelaje
hist(as.numeric(importaciones$Tonelaje[importaciones$Tonelaje!=0]))
summary(as.numeric(importaciones$Tonelaje[importaciones$Tonelaje!=0]))
qqnorm(as.numeric(importaciones$Tonelaje))
qqline(as.numeric(importaciones$Tonelaje))

# Valor CIF
hist(as.numeric(importaciones$Valor.CIF[importaciones$Valor.CIF!=0]))
qqnorm(as.numeric(importaciones$Valor.CIF))
qqline(as.numeric(importaciones$Valor.CIF))

# Impuesto
hist(as.numeric(importaciones$Impuesto))
qqnorm(as.numeric(importaciones$Impuesto))
qqline(as.numeric(importaciones$Impuesto))

# Anio
View(table(importaciones$Anio))

# Mes
View(table(importaciones$Mes))

# Dia
View(table(importaciones$Dia))

# DiaSem
View(table(importaciones$DiaSem))



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

matriz_cor <- cor(numImportaciones)
corrplot(matriz_cor)


# Barplot de paises de proveniencia
paises <- data.frame(table(importaciones$Pais.de.Proveniencia))
paises2 <- paises[order(paises[, 2], decreasing=TRUE), ]
paises2 <- filter(paises2, paises2$Freq > 10000)
colPaises <- distinctColorPalette(length(table(importaciones$Pais.de.Proveniencia)))
barplot(as.vector(paises2[, 2]), 
        names = as.vector(paises2[, 1]), 
        col = colPaises,
        las = 2,
        main = "Pais de proveniencia"
)

# Barplot de aduana de ingreso
aduana <- data.frame(table(importaciones$Aduana.de.Ingreso))
aduana2 <- aduana[order(aduana[, 2], decreasing=TRUE), ]
aduana2 <- filter(aduana2, aduana2$Freq > 2500)
colAduana <- distinctColorPalette(length(table(importaciones$Aduana.de.Ingreso)))
barplot(as.vector(aduana2[, 2]), 
        names = as.vector(aduana2[, 1]), 
        col = colPaises,
        las = 2,
        main = "Aduana de Ingreso"
)

# Barplot de marca
marca <- data.frame(table(importaciones$Marca))
marca2 <- marca[order(marca[, 2], decreasing=TRUE), ]
marca2 <- filter(marca2, marca2$Freq > 10000)
colMarca <- distinctColorPalette(length(table(importaciones$Marca)))
barplot(as.vector(marca2[, 2]), 
        names = as.vector(marca2[, 1]), 
        col = colMarca,
        las = 2,
        main = "Marca"
)

# Barplot de linea
linea <- data.frame(table(importaciones$Linea))
linea2 <- linea[order(linea[, 2], decreasing=TRUE), ]
linea2 <- filter(linea2, linea2$Freq > 7500)
colLinea <- distinctColorPalette(length(linea2$Var1))
barplot(as.vector(linea2[, 2]), 
        names = as.vector(linea2[, 1]), 
        col = colLinea,        
        las = 2,
        main = "Linea"
)

# Barplot de tipo de vehiculo
vehiculo <- data.frame(table(importaciones$Tipo.de.Vehiculo))
vehiculo2 <- vehiculo[order(vehiculo[, 2], decreasing=TRUE), ]
vehiculo2 <- filter(vehiculo2, vehiculo2$Freq > 10000)
colVehiculo <- distinctColorPalette(length(vehiculo2$Var1))
barplot(as.vector(vehiculo2[, 2]), 
        names = as.vector(vehiculo2[, 1]), 
        col = colVehiculo,
        las = 2,
        main = "Tipo de vehiculo"
)

# Barplot de importador
importador <- data.frame(table(importaciones$Tipo.de.Importador))
importador2 <- importador[order(importador[, 2], decreasing=TRUE), ]
colimportador <- distinctColorPalette(length(importador2$Var1))
barplot(as.vector(importador2[, 2]), 
        names = as.vector(importador2[, 1]), 
        col = colimportador,
        las = 2,
        main = "Tipo de importador"
)

# Barplot de combustible
combustible <- data.frame(table(importaciones$Tipo.Combustible))
combustible2 <- combustible[order(combustible[, 2], decreasing=TRUE), ]
colcombustible <- distinctColorPalette(length(combustible2$Var1))
barplot(as.vector(combustible2[, 2]), 
        names = as.vector(combustible2[, 1]), 
        col = colcombustible,
        las = 2,
        main = "Tipo de combustible"
)

# Barplot de asientos
asientos <- data.frame(table(importaciones$Asientos))
asientos2 <- asientos[order(asientos[, 2], decreasing=TRUE), ]
asientos2 <- filter(asientos2, asientos2$Freq > 1000)
colasientos <- distinctColorPalette(length(asientos2$Var1))
barplot(as.vector(asientos2[, 2]), 
        names = as.vector(asientos2[, 1]), 
        col = colasientos,
        las = 2,
        main = "Asientos"
)

# Barplot de puertas
puertas <- data.frame(table(importaciones$Puertas))
puertas2 <- puertas[order(puertas[, 2], decreasing=TRUE), ]
colpuertas <- distinctColorPalette(length(puertas2$Var1))
barplot(as.vector(puertas2[, 2]), 
        names = as.vector(puertas2[, 1]), 
        col = colpuertas,
        las = 2,
        main = "Puertas"
)
