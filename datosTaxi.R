## 1. Obtención de los datos

# Importación de la base de datos *Taxi Flota* de la base de datos abiertos de Madrid, en formato .csv
datos <- read.table("TaxiFlota.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE )

dim(datos) # Vemos la dimension
head(datos,5) # Mostramos toda la info de los 5 primeros
tail(datos) # Consultamos los últimos datos

## 2. Tratamiento y limpieza

### 1. Renombrar los nombres de las columnas

#### 1.1 Cambiar los títulos a minúsculas

colnames(datos) <- sapply(colnames(datos), tolower) 
colnames(datos)

#### 1.2 Sustituir los espacios por "_"

colnames(datos) <- gsub("\\.", "_", colnames(datos)) 
colnames(datos)

#### 1.3 Quitar tildes
colnames(datos) <- iconv(colnames(datos), to='ASCII//TRANSLIT')
colnames(datos)

#### 1.4. Renombrar manualmente las columnas con nombres más largos
colnames(datos)[12] <- "numero_plazas"
colnames(datos)[13] <- "fecha_inicio_prestacion"
colnames(datos)[15] <- "regimen_eurotaxi"
colnames(datos)[16] <- "fecha_inicio_eurotaxi"
colnames(datos)[17] <- "fecha_fin_eurotaxi"

colnames(datos) 

### 2. Convertir las variables a los tipos correspondientes

#### 2.1 Inspeccionar el tipo de variables

sapply(datos, class)

#### 2.2 Identificar las variables a cambiar:

# codigo: int -> character
datos$codigo <- as.character(datos$codigo)
sapply(datos$codigo, class)

# fecha_matriculacion: character -> date
datos$fecha_matriculacion <- strptime(datos$fecha_matriculacion, "%d/%m/%Y") 
datos$fecha_matriculacion <- as.POSIXct(datos$fecha_matriculacion, tz= "", format="%d/%m/%Y") 
datos$fecha_matriculacion

# clasificacion_medioambiental: character -> category
datos$clasificacion_medioambiental <- as.ordered(datos$clasificacion_medioambiental)

# fecha inicio: character -> date
datos$fecha_inicio_prestacion <- strptime(datos$fecha_inicio_prestacion, "%d/%m/%Y") 
datos$fecha_inicio_prestacion <- as.POSIXct(datos$fecha_inicio_prestacion, tz= "", format="%d/%m/%Y") 
datos$fecha_inicio_prestacion

# eurotaxi y regimen_eurotaxi: cambiar 'si' y 'no' a '0' y '1'
datos$eurotaxi[datos$eurotaxi == "SI"] <- 1
datos$eurotaxi[datos$eurotaxi == "NO"] <- 0

datos$regimen_eurotaxi[datos$regimen_eurotaxi == "SI"] <- 1
datos$regimen_eurotaxi[datos$regimen_eurotaxi == "NO"] <- 0

# Eliminamos columnas que consideramos innecesarias para nuestro trabajo
datos$Tipo <- NULL
datos$Variante <- NULL
datos$Fecha <- NULL
datos$Fecha.inicio.Regimen.Especial.Eurotaxi <- NULL
datos$Fecha.fin.Regimen.Especial.Eurotaxi <- NULL

# Comprobamos los tipos de las columnas
sapply(datos, class)


# 4. Análisis exploratorio

# Evolución de los taxis matriculados cada año
hist(datos$fecha_matriculacion, 
     breaks = "years", 
     format = "%Y", 
     freq = TRUE,
     las = 2,
     col = "lightskyblue2",
     xlab = "Fecha de matriculación",
     ylab = "Num vehiculos",
     main = "Taxis matriculados por año")

# Evolución del inicio del servicio de taxis por año
hist(datos$fecha_inicio_prestacion, 
     breaks = "years", 
     format = "%Y", 
     freq = TRUE,
     las = 2,
     col = "seagreen3",
     xlab = "Fecha de inicio de servicio",
     ylab = "Num vehiculos",
     main = " Inicio actividad taxis por año")

# Relación entre la fecha de matriculación y la fecha de inicio de la actividad de taxis

library (ggplot2)
  
qplot(datos$fecha_matriculacion, datos$fecha_inicio_prestacion,
      xlab = "Fecha de matriculación",
      ylab = "Fecha de inicio de la actividad",
      col = "red",
      main = "Relación entre fecha de matriculación e inicio de la actividad"
)


# Relación entre potencia y cilindrada según clasificación medioambiental

# Para evitar problemas con datos en blancos, los sustituimos por "0"
datos$potencia[is.na(datos$potencia)] <- 0

ggplot(datos, aes(x = cilindrada, y =potencia, colour = clasificacion_medioambiental)) + 
  geom_point(aes(size = clasificacion_medioambiental))
