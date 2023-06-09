
setwd("C:/Users/natha/OneDrive/Documents")
car<- read.csv("Car_sales.csv", header=TRUE, sep=",", dec=".", fileEncoding = "UTF-8")
set.seed(10)


#Descripción de los datos

#Se realiza una descripción de los datos para saber número de filas, número de columnas, nombre de las columnas y tipo de datos:
  

str(car)




summary(car)


#Variables categoricas y Variables cuantitativas 






#Cambio de tipo de dato de char a date  en la variable 'Latest_Launch' que contiene formato mes/dia/año

#Se cambia el tipo de dato
car$Latest_Launch=as.Date(car$Latest_Launch, format = '%m/%d/%Y')

#cSAB = cut(SAB,c(0,20000, 50000,100000),labels=c("Bajo","Medio","Alto"))-----------------------------

#Preparación de la Base de datos

#Cambio de nombre de las variables

#Nombre de las variables actuales:
names(car)

#Se utiliza el paquete dplyr la funcion rename para realizar el cambio de nombre de cada una de las variables de la tabla 
car = dplyr::rename(car, Fabricante = Manufacturer, Modelo = Model, Ventas = Sales_in_thousands, Valor_usado = X__year_resale_value, Tipo_vehiculo = Vehicle_type, Precio =  Price_in_thousands, Motor = Engine_size,  Caballos = Horsepower, Ejes_distancia = Wheelbase,Ancho = Width, Longitud = Length, PesoBruto = Curb_weight, Deposito =  Fuel_capacity, Consumo = Fuel_efficiency, Ultimo_lanzamiento = Latest_Launch, Puntuacion = Power_perf_factor)     



#Nombre de las variables después del cambio:
names(car)




#Conversion de variables "Double" a "Integer"

#Variable Ventas es tipo "Double"
typeof(car$Ventas)
#Variable Valor_usado es tipo "Double"
typeof(car$Valor_usado)
#Variable Precio es tipo "Double"
typeof(car$Precio)
#Variable Motor es tipo "Double"
typeof(car$Motor)





#Convertir variables de miles a unidades y a su vez definirlar como enteras 
car$Ventas=as.integer(car$Ventas *10^3)
car$Valor_usado=as.integer(car$Valor_usado *10^3)
car$Precio=as.integer(car$Precio *10^3)
car$Motor =as.integer(car$Motor *10^3)




#Las variables pasan a ser tipo "Integer"
typeof(car$Ventas)
typeof(car$Valor_usado)
typeof(car$Precio)
typeof(car$Motor)



#+ En la variable "Tipo_vehiculo", Se sustituye el valor "Car" = "No pasajeros" 
#+ En la variable "Tipo_vehiculo", Se sustituye el valor "Passenger" = "Pasajeros"  

#Sustituye "Car" por "No pasajeros"
car$Tipo_vehiculo[car$Tipo_vehiculo == "Car"] <- "No Pasajeros"

#Sustituye "Passenger" por "Pasajeros"
car$Tipo_vehiculo[car$Tipo_vehiculo == "Passenger"] <- "Pasajeros"


#+ Se convierten las variables "Deposito" y "Consumo" de galón a litro y se limitan a dos decimales.
#+ Se cambia la variable "Consumo" a tipo "Double".
#+ Se renombran las variables a "Deposito_litro" y "Consumo_litro".


#Tipos de datos de las variable "Consumo" y "Deposito"
typeof(car$Consumo)
typeof(car$Deposito)

#Conversión de galón a litro de la variable "Consumo" con dos decimales y cambiar el tipo de dato a "Double"
car$Consumo=round(as.double(car$Consumo * 3.785),2)

#Conversión de galón a litro de la variable "Deposito" con dos decimales
car$Deposito=round(car$Deposito * 3.785,2)


#Tipo de dato de la variable Consumo después de cambiar a double
typeof(car$Consumo)



#Renombra la columa a "Consumo_litro"
names (car)[14] = "Consumo_litro"

#Renombra la columa a "Deposito_litro"
names (car)[13] = "Deposito_litro"

head(car)


#Conversión de Peso de Toneladas a Kilogramos
car$PesoBruto=round(as.double(car$PesoBruto * 907185),2)
car$Ejes_distancia=round(as.double(car$Ejes_distancia * 2.54),2)
car$Ancho=round(as.double(car$Ancho * 2.54),2)
car$Longitud=round(as.double(car$Longitud * 2.54),2)


car$Grupo[car$Puntuacion >= 15 & car$Puntuacion <= 39]  = "Grupo1"
car$Grupo[car$Puntuacion >= 40 & car$Puntuacion <= 79]  = "Grupo2"
car$Grupo[car$Puntuacion >= 80 & car$Puntuacion <= 119]  = "Grupo3"
car$Grupo[car$Puntuacion >= 120 & car$Puntuacion <= 159]  = "Grupo4"
car$Grupo[car$Puntuacion > 159]  = "Grupo5"

table(car$Grupo)

car$Puntuacion=as.integer(round(car$Puntuacion),0);

head(car)

#Valores Ausentes

ValoresAusentes = which(is.na(car$Valor_usado));
car$Ultimo_lanzamiento = as.character(car$Ultimo_lanzamiento);
car$year = substr(car$Ultimo_lanzamiento, 1,4);
car$year= factor(car$year);
carCompleto= car;

for (valor in  ValoresAusentes)
{
  Grupo<- car[valor,]$Grupo
  Year<- car[valor,]$year
  
  seleccion<-car[ car$Grupo == Grupo & car$year == Year,]$Valor_usado
  carCompleto[valor,]$Valor_usado<-mean(seleccion, na.rm=TRUE)

}


#CarCompleto

carCompleto<-carCompleto[complete.cases(carCompleto), ]


car_mas_caros <- carCompleto[with(carCompleto, order(-carCompleto$Precio)), ]
car_mas_caros[1:5,c(1,2,6)]

car_mas_vendidos <- carCompleto[with(carCompleto, order(carCompleto$Ventas)), ]
car_mas_vendidos[1:5,c(1,2,3)]


boxplot(carCompleto$Precio);
grid(NA,15, lwd = 2);
boxplot(Precio~Tipo_vehiculo,data=carCompleto)
grid(NA,15, lwd = 2);



barplot(table(carCompleto$Grupo));

barplot(table(carCompleto$Grupo)/dim(carCompleto)[1]*100, ylim = c(0, 60), col=palette("Pastel 2"));  




outliers <- boxplot.stats(carCompleto$Precio)$out ##Busca los valores de los outliers de la variable precio
index_outliers <- which(carCompleto$Precio %in% outliers)   #
carCompletoNet <- carCompleto[-index_outliers,]

b = outliers

##Comparar Boxplots
par(mfrow=c(1,2));
boxplot(carCompleto$Precio);
grid(NA,15, lwd = 2);
boxplot(carCompletoNet$Precio);
grid(NA,15, lwd = 2);


mean(carCompleto$Precio)
mean(carCompletoNet$Precio)


## Interpretar Resultados

##Crear carFinal y comprobar Na's
carfinal <- carCompleto[,1:17]

##Comprobar que no hay NA
any(is.na(carfinal)) 




smp_size <- floor(0.7 * nrow(carfinal))
train_ind <- sample(seq_len(nrow(carfinal)), size = smp_size)
trainDataset = carfinal[train_ind,];
testDataset = carfinal[-train_ind,];


write.csv(trainDataset, file = "Train.csv")
write.csv(testDataset, file = "Test.csv")

                       