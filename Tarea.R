setwd("C:/Users/jesus/Tarea5/")
rm(list=ls())

library(ggplot2)
library(tseries)
library(forecast)
library(quantmod)
library(dplyr)

set.seed(126)
AR2 <- arima.sim(model=list(order=c(2,0,0), ar=c(0.1,0.2), sd=0.1),n=100)
AR2a<-arima.sim(n=100,model=list(order=c(2,0,0), ar=c(-0.1,-0.2), sd=0.1))
AR2b<-arima.sim(n=100,model=list(order=c(2,0,0), ar=c(0.3,0.4), sd=0.1))
AR2c<-arima.sim(n=100,model=list(order=c(2,0,0), ar=c(-0.3,0.4), sd=0.1))
AR2d<-arima.sim(n=100,model=list(order=c(2,0,0), ar=c(0.2,0.2), sd=0.1))
AR2e<-arima.sim(n=100,model=list(order=c(2,0,0), ar=c(-0.2,-0.2), sd=0.1))
AR2f<-arima.sim(n=100,model=list(order=c(2,0,0), ar=c(0.7,-0.1), sd=0.1))
AR2g<-arima.sim(n=100,model=list(order=c(2,0,0), ar=c(-0.7,-0.1), sd=0.1))
####Graficando las simulaciones en primera forma####
plot.ts(AR2,ylab="Y[t]")
plot.ts(AR2a,ylab="Y[t]")
plot.ts(AR2b,ylab="Y[t]")
plot.ts(AR2c,ylab="Y[t]")
plot.ts(AR2d,ylab="Y[t]")
plot.ts(AR2e,ylab="Y[t]")
plot.ts(AR2f,ylab="Y[t]")
plot.ts(AR2e,ylab="Y[t]")
plot.ts(AR2g,ylab="Y[t]")
adf.test(AR2e)
ar.acf<-acf(AR2g,type = "correlation",plot=T)

####Probando para un n<30####
AR21<-arima.sim(n=25,model=list(order=c(2,0,0), ar=c(-0.1,-0.2), sd=0.1))
plot.ts(AR21,ylab="Y[t]")
adf.test(AR21)
acf(AR21)

####Graficando las simulaciones en segunda forma####
par(mfrow = c(2,4))
ylm <- c(min(AR2a, AR2b, AR2c, AR2d, AR2e, AR2f, AR2g), 
             max(AR2a, AR2b, AR2c, AR2d, AR2e, AR2f, AR2g))

plot.ts(AR2a, ylim = ylm, main = "phi[1] = -0.1 &  phi[2] = -0.2")
plot.ts(AR2b, ylim = ylm, main = " phi[1] = 0.3 &  phi[2] = 0.4")
plot.ts(AR2c, ylim = ylm, main = " phi[1] = -0.3 &  phi[2] =0.4")
plot.ts(AR2d, ylim = ylm, main = " phi[1] = 0.2 & phi[2] = 0.2")
plot.ts(AR2e, ylim = ylm, main = " phi[1] = -0.2 &  phi[2] =-0.2")
plot.ts(AR2f, ylim = ylm, main = " phi[1] = 0.7 &  phi[2] = 0.1")
plot.ts(AR2g, ylim = ylm, main = " phi[1] = -0.7 &  phi[2] = -0.1")

graphics.off()

####Graficando funciones de autocorrelacion simple Y Autocorrelacion parcial####
par(mfrow=c(1,2))
acf(AR2a, ylim = ylm, main = "phi[1] = -0.1 &  phi[2] = -0.2")
pacf(AR2a, ylim = ylm, main = "phi[1] = -0.1 &  phi[2] = -0.2")

par(mfrow=c(1,2))
acf(AR2b, ylim = ylm, main = " phi[1] = 0.3 &  phi[2] = 0.4")
pacf(AR2b, ylim = ylm, main = " phi[1] = 0.3 &  phi[2] = 0.4")

par(mfrow=c(1,2))
acf(AR2c, ylim = ylm, main = " phi[1] = -0.3 &  phi[2] =0.4")
pacf(AR2c, ylim = ylm, main = " phi[1] = -0.3 &  phi[2] =0.4")

par(mfrow=c(1,2))
acf(AR2d, ylim = ylm, main = " phi[1] = 0.2 & phi[2] = 0.2")
pacf(AR2d, ylim = ylm, main = " phi[1] = 0.2 & phi[2] = 0.2")

par(mfrow=c(1,2))
acf(AR2e, ylim = ylm, main = " phi[1] = -0.2 &  phi[2] =-0.2")
pacf(AR2e, ylim = ylm, main = " phi[1] = -0.2 &  phi[2] =-0.2")

par(mfrow=c(1,2))
acf(AR2f, ylim = ylm, main = " phi[1] = 0.7 &  phi[2] = 0.1")
pacf(AR2f, ylim = ylm, main = " phi[1] = 0.7 &  phi[2] = 0.1")

par(mfrow=c(1,2))
acf(AR2g, ylim = ylm, main = " phi[1] = -0.7 &  phi[2] = -0.1")
pacf(AR2g, ylim = ylm, main = " phi[1] = -0.7 &  phi[2] = -0.1")

####Forma MA(2), funciones de autocorrelacion simple y parcial####
rm(list=ls())
set.seed(126)

layout(matrix(c(1,1,2,3) ,2,2,byrow=TRUE))
MA2a<-arima.sim(list(order=c(0,0,2),ma=c(0.3,0.2)),n=100)
plot(MA2a,main=(expression(MA(2)~~~~theta==c(0.3,0.2))))
acf(MA2a , main=(expression(MA(2)~~~~theta==c(0.3,0.2))))
pacf(MA2a ,main=(expression(MA(2)~~~~theta==c(0.3,0.2))))

layout(matrix(c(1,1,2,3) ,2,2,byrow=TRUE))
MA2b<-arima.sim(list(order=c(0,0,2),ma=c(-0.1,-0.3)),n=100)
plot(MA2b,main=(expression(MA2b~~~~theta==c(-0.1,-0.3))))
acf(MA2b , main=(expression(MA2b~~~~theta==c(-0.1,-0.3))))
pacf(MA2b ,main=(expression(MA2b~~~~theta==c(-0.1,-0.3))))

layout(matrix(c(1,1,2,3) ,2,2,byrow=TRUE))
MA2c<-arima.sim(list(order=c(0,0,2),ma=c(-0.3,0.5)),n=100)
plot(MA2c,main=(expression(MA2c~~~~theta==c(-0.3,0.5))))
acf(MA2c , main=(expression(MA2c~~~~theta==c(-0.3,0.5))))
pacf(MA2c ,main=(expression(MA2c~~~~theta==c(-0.3,0.5))))

layout(matrix(c(1,1,2,3) ,2,2,byrow=TRUE))
MA2d<-arima.sim(list(order=c(0,0,2),ma=c(0.7,0.1)),n=100)
plot(MA2d,main=(expression(MA2d~~~~theta==c(0.7,0.1))))
acf(MA2d , main=(expression(MA2d~~~~theta==c(0.7,0.1))))
pacf(MA2d ,main=(expression(MA2d~~~~theta==c(0.7,0.1))))

layout(matrix(c(1,1,2,3) ,2,2,byrow=TRUE))
MA2e<-arima.sim(list(order=c(0,0,2),ma=c(-0.1,0.1)),n=100)
plot(MA2e,main=(expression(MA2e~~~~theta==c(-0.1,0.1))))
acf(MA2e , main=(expression(MA2e~~~~theta==c(-0.1,0.1))))
pacf(MA2e ,main=(expression(MA2e~~~~theta==c(-0.1,0.1))))


#### Modelos o procesos ARMA ####
set.seed(126)
ARIMAA <- list(order = c(2,0,2),
               ar = c(-0.1,-0.2),
               ma = c(0.3,0.2))

mu = 5
# Simulamos el proceso arma + mu (media)
ARIMAA.sim <- arima.sim(n = 100, model = ARIMAA) + mu 

class(ARIMAA.sim)
plot.ts(ARIMAA.sim)
acf(ARIMAA.sim) 
pacf(ARIMAA.sim) 

# ARMA(2,2) = AR(2) + MA(2)
# Estimar esos parametros
arima(x = ARIMAA.sim, order=c(2,0,2))


set.seed(126)
ARIMAA2 <- list(order = c(2,0,2),
               ar = c(0.3,0.4),
               ma = c(-0.1,-0.3))

mu = 4
# Simulamos el proceso arma + mu (media)
ARIMAA2.sim <- arima.sim(n = 100, model = ARIMAA) + mu 

class(ARIMAA2.sim)
plot.ts(ARIMAA2.sim)
acf(ARIMAA2.sim) 
pacf(ARIMAA2.sim) 

# ARMA(2,2) = AR(2) + MA(2)
# Estimar esos parametros
arima(x = ARIMAA2.sim, order=c(2,0,2))














