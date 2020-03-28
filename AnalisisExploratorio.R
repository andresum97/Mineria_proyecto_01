
importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE)
fallecidos <- read.csv("fallecidos.csv", stringsAsFactors = FALSE)

importaciones <- na.omit(importaciones)
summary(importaciones)
length(importaciones)


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

