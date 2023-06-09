install.packages('dplyr')
install.packages('ggplot2')
install.packages("corrplot")
library(dplyr)
library(ggplot2)
library(corrplot)

##Se Cargan los datos
setwd("C:/PEC3")
car<- read.csv("car_net.csv", header=TRUE, sep=",", dec=".",stringsAsFactors = TRUE, fileEncoding = "UTF-8")

##Se realiza una inspección de los datos
summary(car)

#Limpieza - Remover na's
car<-car[complete.cases(car), ]

##Se comprueba que se eliminan los NA's
summary(car)

##Calcula la media y la desviación estandar de la variable Puntuacion para cada grupo de la variable Tipo_vehiculo. 
##Para ello necesitas agrupar los datos por dicha variable. Se pueden usar funciones de la librería plyr como ddply y 
##summarise que son de utilidad. Otra forma es usar la librería dplyr con las funciones summarise y group_by.
##Se pueden realizar también los cálculos manualmente con instrucciones propias.

sd_mean.puntuacion<-car %>% 
  group_by(Tipo_vehiculo)  %>% 
  summarize(media_puntuacion = mean(Puntuacion),
            sd_puntuacion = sd(Puntuacion)) 

sd_mean.puntuacion


##Bloxplot  Ahora obtén una comparativa de los boxplot de la variable Puntuacion para cada grupo de Tipo_vehiculo.

boxplot(Puntuacion~Tipo_vehiculo,data=car)
grid(NA,15, lwd = 2);


##myAov

myAov = aov(Puntuacion~Tipo_vehiculo,data=car)
summary(myAov)

##Comprueba los cálculos
## A partir de los resultados de ANOVA, identifica la suma de cuadrados intra grupo (SSW) y la suma de cuadrados entre grupos (SSB).
## A partir de estos valores, calcula manualmente F y comprueba que coincide con el resultado del modelo ANOVA calculado.


##Varianza Intragrupos
varx1 <- sum ((car[car$Tipo_vehiculo=="Pasajeros",]$Puntuacion - mean(car[car$Tipo_vehiculo=="Pasajeros",]$Puntuacion)) ^2 )
varx1
varx2 <- sum ((car[car$Tipo_vehiculo=="No pasajeros",]$Puntuacion - mean(car[car$Tipo_vehiculo=="No pasajeros",]$Puntuacion)) ^2 )
varx2
SSW = varx1 + varx2
SSW

##Varianza Total
xall <- c(car[car$Tipo_vehiculo=="Pasajeros",]$Puntuacion,car[car$Tipo_vehiculo=="No pasajeros",]$Puntuacion)
xall
SST <- sum( (xall - mean(xall)) ^ 2 )
SST
##Varianza Entre Grupos
SSB <- SST - SSW
SSB


## m sería la cantidad de grupos, en este caso  es igual a 2
m <-2
## N sería la cantidad de observaciones, en este caso es 117 
N <-length( xall )
N

## Calculamos F
F <- (SSB / (m-1)) / (SSW/ (N-m))
F

## Calculamos Valor Crítico
vc <- qf(0.05, m-1, N-m, lower.tail=F)
vc

##Hipotesis h0 = las medias no son distintas
## Hipotesis h1 = las medias son tan  distintas 

#Hipotesis nula: $H_{0}: \beta_i =0$ 
#Hipotesis bilateral: $H_{0}: \beta_i\neq0$

## El resultado de F no es muy grande, por lo cual corresponde con que las medias no sean tan distintas, efectivamente es así. 
##Como hemos visto el VC es mayor que F, por lo tanto aceptaremos la hipotesis nula (h0)
##Con ello podemos deducir que la variable Puntuacion no se ve afectada por el tipo de vehiculo.

Mediana = median(car$Precio)

car$GR_PRECIO <- NA
car[car$Precio <= Mediana,]$GR_PRECIO <- 1
car[car$Precio > Mediana,]$GR_PRECIO <- 2

#Calculamos anova de la puntuacion de los grupos creados en el paso anterior

myAov.GR_PRECIO = aov(Puntuacion~GR_PRECIO,data=car)
summary(myAov.GR_PRECIO)

#Se muestran datos como enteros en centimetros en la columna Ancho, se unifican a metros. 
index.Anchocms<-which(grepl( "cms" , car$Ancho ))

#Se elimina el "cms" de la columna "Ancho"
car$Ancho = gsub("cms","", car$Ancho)

#Se eliminan los espacios en blanco de las cadenas
car$Ancho<-trimws(car$Ancho, which = "both")

car[index.Anchocms,]$Ancho<-as.double(car[index.Anchocms,]$Ancho)/100

#Se comprueba el tipo de dato de la variable "Ancho"
typeof(car$Ancho)

#Se convierte la variable **"Ancho"** a tipo double con 2 decimales
car$Ancho= round(as.numeric(car$Ancho),2)

#El Warning message:NAs introducidos por coerción , se corresponde a la fila 34 que ya contenia un NA.

#Preparacion de datos
car2 <- car %>% 
  select('Ventas', 'Precio', 'Motor', 'Caballos', 'Ejes_distancia', 'Ancho', 'Longitud', 'PesoBruto', 'Deposito_litro' , 'Consumo_litro')

##Matriz de correlacion
MatrizCor = cor(car2)
MatrizCor

##Corplot
corrplot(MatrizCor, method = "circle")

# Un análisis de componentes principales tiene sentido si existen altas correlaciones entre las variables, 
#ya que esto es indicativo de que existe información
# redundante y, por lo tanto, un número menor de variables serán capaces de
# explicar gran parte de la variabilidad total.


#PCA


pcaNonStandard <- prcomp(car2)
summary(pcaNonStandard)

#PCA con datos estandarizados
pca <- prcomp(car2, scale=TRUE)

plot(pca)

plot(pcaNonStandard)

# Varianza acumulada
# A partir de los valores de varianza de cada componente, 
# haz el cálculo de la varianza acumulada. 
# El resultado debe coincidir con el que proporciona el modelo prcomp. 
# Muestra un gráfico con la varianza acumulada.

summary(pca)
prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza
prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum
ggplot(data = data.frame(prop_varianza_acum, pc = 1:10),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")


# Interpretación
# Interpreta los resultados obtenidos. 
# Calcula la varianza acumulada utilizando únicamente los 3 primeros componentes.

# #[ESPACIO RESPUESTA]

prop_varianza_acum.3.componentes.total <- sum(prop_varianza[1:3])
prop_varianza_acum.3.componentes.total

# Muestra cómo ha contribuido cada variable a cada componente. 
# Puedes restringir la salida a los 3 primeros componentes, y 
# redondear cada componente a 3-4 decimales, para hacer los 
# resultados más legibles.
# #[ESPACIO RESPUESTA]

prop_varianza_acum.3.componentes <- cumsum(prop_varianza[1:3])
barplot(prop_varianza_acum.3.componentes)

# Visualizar la proyección sobre los dos primeros componentes
# A continuación, vamos a representar todos los datos del 
# conjunto de datos proyectados sobre los dos primeros componentes 
# que ha dado como resultado el PCA. Para ello, necesitamos transformar 
# en primer lugar el conjunto de datos sobre un nuevo data frame que 
# tendrá la proyección sobre los dos primeros componentes y el grupo al 
# que pertenece cada instancia del conjunto de datos.
# 
# Para ello, podemos aplicar la función predict pasando como 
# parámetro el modelo PCA y los datos originales. Encontraréis ejemplos 
# de cómo hacerlo en los ejemplos de la actividad preparatoria.
# 
# Cuando ya tengáis los datos preparados, debéis visualizar el 
# resultado en un scatter plot. Cada punto del conjunto tendrá unas 
# coordenadas x,y en este gráfico, que son las dos primeras variables 
# del conjunto de datos proyectados. Además, debéis pintar en un color 
# diferente cada punto, en función de los valores de la variable Tipo_vehiculo.

pca.predict<-predict(pca, newdata=car)[,1:2]

graphics.off()
par("mar")
par(mar=c(1,1,1,1))
plot(pca.predict, col=car$Tipo_vehiculo)
legend("topleft", legend=levels(car$Tipo_vehiculo),cex = 0.6, pch=16, col=unique(car$Tipo_vehiculo))

