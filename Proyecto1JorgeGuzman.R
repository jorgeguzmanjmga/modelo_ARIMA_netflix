library(TSA)   
library(forecast)
library(lmtest)

#### Introducción ####

base<-read.csv("NFLX.csv")
base<-base[c(504:756),] #Usaremos información de 2020-2021
head(base)
tail(base)

#Se utilizó un aproximado de 252 días de cotización al año
NETFLIX.ts<-ts(base$Close, end=c(2021, 2, 5), frequency=252) 
NETFLIX.ts

plot(NETFLIX.ts, main="Gráfica 1\n Precio de cierre acciones de Netflix", 
     ylab="Precio de cierre", xlab="Fecha", col="red")

plot(diff(NETFLIX.ts), 
     main="Gráfica 2\n Precio de cierre acciones de Netflix\n(Primera diferencia)" , 
     ylab="Precio de cierre", xlab="Fecha", col="red")

#### Identificacion Modelo ####

par(mfrow=c(1,2)) 
acf(as.vector(diff(NETFLIX.ts)), main="Función de autocorrelación", lag=25)
pacf(as.vector(diff(NETFLIX.ts)), main="Función de autocorrelación parcial")

eacf(as.vector(diff(NETFLIX.ts)))
#Parece proponer un ARMA(0,5) para la 1ra diferencia, es decir, ARIMA(0,1,5) 
#para serie original

####Estimación modelo #####
modelo015<-arima(as.vector(NETFLIX.ts), order=c(0,1,5))
coeftest(modelo015)


#### Validación Modelo ####

##### ARIMA(0,1,5) ####
residuos=modelo015$residuals
par(mfrow=c(1,1))
plot(residuos, main="Gráfica 5. \n Gráfico de residuos")

par(mfrow=c(1,2))
acf(as.vector(residuos), main="Función de autocorrelación")
pacf(as.vector(residuos), main="Función de autocorrelación parcial") 
#Las barras estan dentro de las barras de Bartlett
#No se tiene evidencia en contra de que sea un ruido blanco

#Prueba ruido blanco
prueba=Box.test(residuos,lag = 20,type="Ljung-Box")
prueba 
#El p-valor de la prueba Ljung Box es 0.58>0.05, no se rechaza H0,
#es decir, no hay evidencia suficiente para rechazar que los residuales 
#se comporten como ruido blanco

par(mfrow=c(1,1))
qqnorm((residuos-mean(residuos))/sd(residuos))
#Prueba normalidad en los residuos
ks.test((residuos-mean(residuos))/sqrt(var(residuos)),"pnorm",0,1) 
#El p-valor=0.2644>0.05 indica que con un nivel de significancia de 5% 
#no se rechaza la normalidad de los residuales, es decir, la muestra no presenta 
#evidencia suficiente para rechazar la normalidad.


#### Pronósticos ####

modelo<-Arima(NETFLIX.ts, order=c(0,1,5))
prediccion<-forecast(modelo, h=5) # 5 periodos hacía adelante
prediccion
plot(prediccion, main="Gráfica 8. \n Predicción para el precio de las acciones de Netflix",
     ylab="Precio de cierre", xlab="Fecha", col="red")

