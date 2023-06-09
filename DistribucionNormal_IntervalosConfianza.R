library(dplyr)
library(ggplot2)
set.seed(123)
library(RColorBrewer)

###Se realiza la carga de los datos ###
setwd("C:/Users/natha/OneDrive/Documents/PEC2")
car<- read.csv("car_net.csv", header=TRUE, sep=",", dec=".",stringsAsFactors = TRUE, fileEncoding = "UTF-8")

#Se muestra un resumen de los datos del dataframe
str(car)

#Se elimina el "mts" de la columna "Ancho"
car$Ancho = gsub("mts","", car$Ancho)

#Se eliminan los espacios en blanco de las cadenas
trimws(car$Ancho, which = "both")

#Se comprueba el tipo de dato de la variable "Ancho"
typeof(car$Ancho)

#Se convierte la variable **"Ancho"** a tipo double con 2 decimales
car$Ancho= round(as.numeric(car$Ancho),2)

#El Warning message:NAs introducidos por coerción , se corresponde a la fila 34 que ya contenia un NA.

#Se convierte la variable **"Grupo"** a tipo factor
car$Grupo= as.factor(car$Grupo)


### Se muestran los cinco fabricantes de vehículos que han vendido más vehículos. ###

#Se agrupan por fabricantes
car_mas_vendidos <- aggregate(car[,3], by=list(car$Fabricante), 
                              FUN =sum, na.rm=TRUE)
#Ordenar por fabricante con mas ventas
car_mas_vendidos <- car_mas_vendidos[with(car_mas_vendidos,order(-car_mas_vendidos$x)), ]
#Se muestran los primeros 5 fabricantes con mas ventas 
car_mas_vendidos[1:5,c(1,2)]


#Se multiplica las ventas por el precio para saber los ingresos por ventas 
car$Ingreso_ventas = as.integer(as.numeric(car$Ventas) * car$Precio/10^6)

#Se agrupan por fabricantes
car_menos_vendidos <- aggregate(car[,18], by=list(car$Fabricante), 
                                FUN =sum, na.rm=TRUE)
#Ordenar por fabricante con menos ventas
car_menos_vendidos <- car_menos_vendidos[with(car_menos_vendidos,order(car_menos_vendidos$x)),]
#Se muestran los primeros 5 fabricantes con mas ventas 
car_menos_vendidos[1:5,c(1,2)]

#Grafico de sectores según ingresos con los diez fabricantes con mayores ingresos.
mas_ingresos <- aggregate(car[,18], by=list(car$Fabricante), 
                          FUN =sum, na.rm=TRUE)
mas_ingresos <- mas_ingresos[with(mas_ingresos,order(-mas_ingresos$x)),]
mas_ingresos = mas_ingresos[1:10,c(1,2)]


pielabels <- sprintf("%3.1f%s",
                     100*mas_ingresos[,2]/sum(mas_ingresos[,2]), "%")

pieLegend <- sprintf("%s", mas_ingresos[,1])

pie(mas_ingresos[,2],
    labels=pielabels,
    clockwise=TRUE,
    col = rainbow(10, alpha=0.5),
    border = rainbow(6, v=0.6),
    radius=0.7,
    cex=0.8,
    main="Fabricantes con mayores ingresos")
legend("bottomright",legend=pieLegend,bty="n",
       fill=rainbow(10, alpha=0.5))



###Intervalos de Confianza###

#Remover na's
carcom<-car[complete.cases(car), ]

#Media del Precio
mm <- mean(carcom$Precio)

#Desviacion Estandar
dm = sd(carcom$Precio)

#Nivel de confianza que queremos obtener
p = 0.90

#Tamaño del intervalo
error= qnorm(1- (1-p)/2) *dm/sqrt(117)

#Extremos
Primer_Int <- mm-error
Primer_Int

Segundo_Int <- mm+error
Segundo_Int

###Comprobacion del intervalo 90%   23800.45 28138.49
t.test(carcom$Precio, conf.level = 0.90)


###Contraste de Hipotesis###

#se contrasta si el precio de los vehículos con una muestra de 117 es superior a 29.000. con un nivel de confianza del 90%.

##Se plantean las siguientes hipotesis:

##Hipotesis Nula: media del precio  > 29.000 euros
##Hipotesis Alternativa: media del precio  < 29.000 euros

##Se realiza el calculo con un nivel de confianza del 95%

#Nivel de confianza
p= 0.95
#Varianza Poblacional
varianzaPob = var(carcom$Precio) * (dim(carcom)[1] - 1) / dim(carcom)[1]

#obtenemos el estadístico y el valor de la región de rechazo
alpha = (1-p)
region = qt((1 - alpha), df= dim(carcom)[1]-1)
estadistico = (mm-29000)/dm/sqrt(dim(carcom)[1])


#La Región de no rechazo para ??=0.05 empezó en el valor t???t1?????=1.65
#Estadístico del contraste: t=-0.0198 
#Como el estadístico de contraste NO se encuentra en la región de no rechazo, se puede rechazar que el precio medio será superior a 29.000.

#Coeficiente de correlacion 
cor(carcom$Ventas)
#round(cor(base),2) 
#Matriz de correlacion 
rcorr(as.matrix(base))


#Si el p-value es menor al nivel de significancia que nosotros escogemos, por ejemplo 5%, entonces el coeficiente es estadísticamente significativo.


#Matriz de Correlación
carcom.select=carcom %>%
  select('Ventas', 'Valor_usado', 'Precio', 'Motor', 'Caballos', 'Ejes_distancia', 'Ancho','Longitud','PesoBruto', 'Puntuacion', 'Deposito_litro', 'Consumo_litro')

cor(carcom.select)

#Grafico de dispersion entre Precio y Puntuacion
par(mar=rep(2,4))
plot(carcom.select$Precio,carcom.select$Puntuacion)

#Grafico de dispersion entre Precio y Ancho
plot(carcom.select$Precio,carcom.select$Ancho)


##Regresion lineal multiple
#Regresion lineal variable dependiente "PRECIO" y variables explicativas
#Modelo
mylm <- lm(Precio ~ Caballos+ Ancho+ Longitud+ PesoBruto+Grupo+Deposito_litro + Consumo_litro, data = carcom)
summary(mylm)

#par(mar=rep(2,4))
par(mfrow = c(2, 2))
plot(mylm)

#Hipotesis nula: $H_{0}: \beta_i =0$ 
#Hipotesis bilateral: $H_{0}: \beta_i\neq0$
anova( mylm, mylm1)

###Modelo  1
mylm1 <- lm(Precio ~ Caballos+ Ancho+ Longitud+ PesoBruto+Grupo+ Consumo_litro, data = carcom)
summary(mylm1)


#Calcular el precio
new_car <- data.frame(
 Caballos = c(175), Ancho = c(1.70), Longitud = c(4.5), PesoBruto = c(3000), Grupo = as.factor(1), Deposito_litro = c(60), Consumo_litro = (6)
)
new_car_precio = predict(mylm1, newdata = new_car)


##Analisis de los residuos
par(mfrow = c(2, 2))
plot(mylm1)

#Regresión lineal múltiple: Variable Grupo
carcom <- within(carcom, Grupo <- relevel(Grupo, ref = 4))

carcom.lm.2 <- lm(Precio ~ Caballos+ Ancho+ Longitud+ PesoBruto+Grupo+Deposito_litro + Consumo_litro, data = carcom)
summary(carcom.lm.2)

#Regresión lineal múltiple: Variable añadiendo al modelo anterior la variable Motor
carcom.lm.3 <- lm(Precio ~ Caballos+ Ancho+ Longitud+ PesoBruto+Grupo+Deposito_litro + Consumo_litro + Motor, data = carcom)
summary(carcom.lm.3)

#Regresión lineal múltiple: Variable Motor y Precio
carcom.lm.4 <- lm(Precio ~ Motor, data = carcom)
summary(carcom.lm.4)
