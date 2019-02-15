#Obtenemos los datos del fichero csv
datos <- read.table("data/TaxiFlota.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE )

dim(datos) # Vemos la dimension
head(datos,5) # Mostramos toda la info de los 5 primeros
tail(datos) # Consultamos los Ãºltimos datos


# EN PRIMER LUGAR VAMOS A CAMBIAR LOS NOMBRES DE LAS COLUMNAS

#Primero quitamos todas las mayúsculas
mi.datos2 <- datos

colnames(mi.datos2)

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


#CAMBIAMOS LOS TIPOS

#Primero consultamos los tipos 
sapply(mi.datos2, class)

mi.datos2$codigo <- as.character(mi.datos2$codigo)
sapply(mi.datos2$codigo, class)


#Cambiamos el tipo a la columna de clasificacion
mi.datos2$clasificacion_medioambiental <- as.ordered(mi.datos2$clasificacion_medioambiental)

#Cambiamos las fechas que venian tipo factor

mi.datos2$fecha_matriculacion <- as.POSIXct(mi.datos2$fecha_matriculacion, tz= "CEST", format="%d/%m/%Y") 
mi.datos2$fecha_matriculacion

mi.datos2$fecha_inicio_prestacion <- as.POSIXct(mi.datos2$fecha_inicio_prestacion, tz= "CEST", format="%d/%m/%Y") 
mi.datos2$fecha_inicio_prestacion

#mi.datos2$fecha_matriculacion <- strptime(mi.datos2$fecha_matriculacion, "%d/%m/%Y") 
#mi.datos2$fecha_matriculacion

#mi.datos2$fecha_inicio_prestacion <- strptime(mi.datos2$fecha_inicio_prestacion, "%d/%m/%Y") 
#mi.datos2$fecha_inicio_prestacion



#CAMBIAMOS LOS SI Y NO A 0,1
mi.datos2$eurotaxi[mi.datos2$eurotaxi == "SI"] <- 1
mi.datos2$eurotaxi[mi.datos2$eurotaxi == "NO"] <- 0

mi.datos2$regimen_eurotaxi[mi.datos2$regimen_eurotaxi == "SI"] <- 1
mi.datos2$regimen_eurotaxi[mi.datos2$regimen_eurotaxi == "NO"] <- 0


#Eliminamos columnas que consideramos innecesarias para nuestro trabajo
mi.datos2$Tipo <- NULL
mi.datos2$Variante <- NULL
mi.datos2$Fecha <- NULL
mi.datos2$Fecha.inicio.Régimen.Especial.Eurotaxi <- NULL
mi.datos2$Fecha.fin.Régimen.Especial.Eurotaxi <- NULL

#Comprobamos los tipos de las columnas
sapply(mi.datos2, class)


#Ahora vamos a pintar los gráficos

#En este histograma vemos la evolución de los taxis matriculados cada año
hist(mi.datos2$fecha_matriculacion, 
     breaks = "years", 
     format = "%Y", 
     freq = TRUE,
     las = 2,
     col = "lightskyblue2",
     xlab = "Fecha de matriculación",
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

# Relación entre potencia y cilindrada según clasificación medioambiental
# Se ve una relación directa entre potencia y cilindrada y según estas variables tengan valores más bajos,
# el nivel de contaminación es menor. 
# No se puede asumir una relación directa entre los coches con nivel de contaminación 0, una vez que el número de casos
# que asume este valor es muy reducido y no se pueden sacar conclusiones.

ggplot(mi.datos2, aes(x = cilindrada, y = potencia, colour = clasificacion_medioambiental)) + geom_point()

# Evolución entre tipo de combustible y nivel de contaminación
# Los coches de contaminación 0 son exclusivamente elétricos
# Los coches de contaminación B y C son principalmente a Diesel y han aumentado en los últimos años.
# Hay también coches de contaminación de nivel C de gasolina transformada, que aumentaron a lo largo del tiempo y empezaron en caída
# Los coches eco son de gasolina - electricidad, que ha sido constante en los últimos años, gasolina - fás natural, que han bajado en los últimos años
# y GLP/Gasolina que se han mantenido estables también a lo largo del tiempo.


tmp <- mi.datos2[mi.datos2$clasificacion_medioambiental %in% c('0', 'ECO', 'B', 'C'),]
ggplot(tmp, aes(x= fecha_inicio_prestacion, y= cilindrada)) + 
  geom_point() + geom_smooth() +
  facet_grid(clasificacion_medioambiental ~ combustible)

#graph=plot_ly(x= s)

datos[datos$Combustible == "DIESEL",] # Nos devuelve todos los taxis cuyo combustible es DIESEL

plot(datos$Combustible , datos$Marca)

head(datos[datos$NÃºmero.de.Plazas > 5 & datos$Clasificación.medioambiental == "ECO",],5)

hist(datos$Potencia)

table(table(datos$Matricula)) # Contamos las frecuencias de Código # Para ver duplicados
