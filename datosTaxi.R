#Obtenemos los datos del fichero csv
datos <- read.table("data/TaxiFlota.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE )

dim(datos) # Vemos la dimension
head(datos,5) # Mostramos toda la info de los 5 primeros
tail(datos) # Consultamos los últimos datos


# EN PRIMER LUGAR VAMOS A CAMBIAR LOS NOMBRES DE LAS COLUMNAS

#Primero quitamos todas las mayúsculas
mi.datos2 <- datos
colnames(mi.datos2) <- sapply(colnames(mi.datos2), tolower) 

# Buscamos los . y los sustituimos por _
colnames(mi.datos2) <- gsub("\\.", "_", colnames(mi.datos2)) 

#Por ultimo quitamos todas las tildes
colnames(mi.datos2) <- iconv(colnames(mi.datos2), to='ASCII//TRANSLIT')


#Si aun asi, vemos que tenemos nombres muy largos...
#Acortamos algunos de los nombres de ciertas columnas específicas 
colnames(mi.datos2)[12] <- "numero_plazas"
colnames(mi.datos2)[13] <- "fecha_inicio_prestacion"
colnames(mi.datos2)[15] <- "regimen_eurotaxi"
colnames(mi.datos2)[16] <- "fecha_inicio_eurotaxi"
colnames(mi.datos2)[17] <- "fecha_fin_eurotaxi"

colnames(mi.datos2) 



#Cambiamos los tipos

#Primero consultamos los tipos 
sapply(mi.datos2, class)

mi.datos2$codigo <- as.character(mi.datos2$codigo)
sapply(mi.datos2$codigo, class)



mi.datos2$clasificacion_medioambiental <- as.ordered(mi.datos2$clasificacion_medioambiental)
mi.datos2$clasificacion_medioambiental <- factor(mi.datos2$clasificacion_medioambiental, ordered = TRUE, levels= "0", "Eco", "C", "B")

#Cambiamos las fechas que venian tipo factor

mi.datos2$fecha <- strptime(mi.datos2$fecha, "%d/%m/%Y") 
mi.datos2$fecha

mi.datos2$fecha_matriculacion <- as.POSIXct(mi.datos2$fecha_matriculacion, tz= "CEST", format="%d/%m/%Y") 
mi.datos2$fecha_matriculacion

mi.datos2$fecha_inicio_prestacion <- as.POSIXct(mi.datos2$fecha_inicio_prestacion, tz= "CEST", format="%d/%m/%Y") 
mi.datos2$fecha_inicio_prestacion

#mi.datos2$fecha_matriculacion <- strptime(mi.datos2$fecha_matriculacion, "%d/%m/%Y") 
#mi.datos2$fecha_matriculacion

#mi.datos2$fecha_inicio_prestacion <- strptime(mi.datos2$fecha_inicio_prestacion, "%d/%m/%Y") 
#mi.datos2$fecha_inicio_prestacion

mi.datos2$fecha_inicio_eurotaxi <- strptime(mi.datos2$fecha_inicio_eurotaxi, "%d/%m/%Y") 
mi.datos2$fecha_inicio_eurotaxi

mi.datos2$fecha_fin_eurotaxi <- strptime(mi.datos2$fecha_fin_eurotaxi, "%d/%m/%Y") 
mi.datos2$fecha_fin_eurotaxi

sapply(mi.datos2, class)



#Ahora vamos a pintar los gráficos

#En este histograma vemos la evolución de los taxis matriculados cada año
hist(mi.datos2$fecha_matriculacion, 
     breaks = "years", 
     format = "%Y", 
     freq = TRUE,
     las = 2,
     col = "lightskyblue2",
     xlab = "Fecha de matriculacion",
     ylab = "Num vehiculos",
     main = "Taxis matriculados por año")

#En este histograma vemos los taxis que han iniciado su servicio por año
hist(mi.datos2$fecha_inicio_prestacion, 
     breaks = "years", 
     format = "%Y", 
     freq = TRUE,
     las = 2,
     col = "seagreen3",
     xlab = "Fecha de inicio de servicio",
     ylab = "Num vehiculos",
     main = " Inicio actividad taxis por año")

library (ggplot2)
  
qplot(mi.datos2$fecha_matriculacion, mi.datos2$fecha_inicio_prestacion,
      xlab = "Fecha de matriculación",
      ylab = "Fecha de inicio del servicio",
      col = "red"
)

qplot(mi.datos2$fecha_inicio_prestacion, mi.datos2$fecha_matriculacion)
#Podemos observar que no hay mucha variación entre el histograma anterior y este.
#Es decir, lo lógico es que los taxis sean matriculados e inicien su servicio en el mismo año.

#graph=plot_ly(x= s)

datos[datos$Combustible == "DIESEL",] # Nos devuelve todos los taxis cuyo combustible es DIESEL

plot(datos$Combustible , datos$Marca)

head(datos[datos$Número.de.Plazas > 5 & datos$Clasificación.medioambiental == "ECO",],5)

hist(datos$Potencia)

table(table(datos$Matrícula)) # Contamos las frecuencias de Código # Para ver duplicados
