# practica-14
#Genera 5 series de tiempo con nombre multiple1. 
#Los rangos de estas series son serie 1) de 15 a 35 , 2) de 100 a 200 , 3) 29 a 49 y 4) 42 a 162 5)30 a 150. 
#Las series deben tener 250 mensules e inician en el tercer mes del 1990.

s1<-sample(15:35, 250, replace= T)
s2<-sample(100:200, 250, replace= T)
s3<-sample(29:49, 250, replace= T)
s4<-sample(42:162, 250, replace= T)
s5<-sample(30:150, 250, replace= T)



#Hacer estos datos como data frame

multiple1<-data.frame(s1,s2,s3,s4,s5)
multiple1

#Generar cada una de las columnas en series de tiempo,,,,
ts1<-ts(multiple1[1:250,1], frequency = 12, start=c(1990,3))
ts2<-ts(multiple1[1:250,2], frequency = 12, start=c(1990,3))
ts3<-ts(multiple1[1:250,3], frequency = 12, start=c(1990,3))
ts4<-ts(multiple1[1:250,4], frequency = 12, start=c(1990,3))
ts5<-ts(multiple1[1:250,5], frequency = 12, start=c(1990,3))

#1.1)de la serie 1, 2 series de tiempo y dividenlas (con funcion en r) 
#en 2 series que vayan de 1990 a 2003 y de 1998 a 2006 y después grafíquenlas.

p1<- (window(ts1, start=1990, end=2003))
p2<- (window(ts2, start=1998, end=2006))

graf1<- plot(cbind(p1,p2), col="blue")

#1.2) dividir la series 3 y 5 a traves de corchetes desde la observacion 150 a la 250...

d1<-ts3[150:250]
d2<-ts5[150:250]

#1.2) De 2 series (la que quieran) series obtener y generar 2 data frame de la tendencia y 
#la estacionalidad 

ds1<-decompose(ts1)
ten1<-ds1$trend
est1<-ds1$seasonal

ds2<-decompose(ts2)
ten2<-ds2$trend
est2<-ds2$seasonal

dfe<-data.frame(est1, est2)
dft<-data.frame(ten1, ten2)

#5.3) De las 4 series obtener correlación, covarianza y la función de autocorrelación.
cor(ts1, ts2)
cor(ts2, ts3)
cor(ts3, ts4)
cor(ts4, ts5)

cov(ts1, ts2)
cov(ts2, ts3)
cov(ts3, ts4)
cov(ts4, ts5)

a<-acf(ts1)
b<-acf(ts2)
c-acf(ts3)
d<-acf(ts4)
e<-acf(ts5)

#1.3 Obtener la función de autocorrrelacion de las 4 series de tiempo
#y analizar si el correlograma para saber si alguna de las series tienen ruido blanco.

a<-acf(ts1) #*
b<-acf(ts2) #no tiene
c-acf(ts3)#*
d<-acf(ts4)#no tiene
e<-acf(ts5)#*

#*tienen ruido blanco

#1.4 Grafique las 5 series en una sola grafica con leyenda y todos los argumentos de la función plot

x11()
plot(ts1, main="series", col="blue")
lines(ts2, col= "green")
lines(ts3, col= "violet")
lines(ts4, col= "pink")
lines(ts5, col= "red")
legend("topleft", lty=1, col=c("blue", "green", "violet", "pink", "red"),
       legend(c="s1", "s2", "s3", "s4", "s5"), pch=10)

#Detectar estacionalidad, tendencia o ciclo en cada una….. ( si es que hay)

plot(ts1)
plot(ts2)
plot(ts3)
plot(ts4)
plot(ts5)

ds1<-decompose(ts1)
ds2<-decompose(ts2)
ds3<-decompose(ts3)
ds4<-decompose(ts4)
ds5<-decompose(ts5)

plot(ts1)
plot(ds2)
plot(ds3)
plot(ds4)
plot(ds5)

no tienen tendencia, ni ciclo, ni estacionalidad









### impprten una serie de tiempo  de la pagina del inegi y expliquen cada linea de este script 

base<- tempfile() #crea una carpeta temporal

download.file("http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/regulares/enigh/nc/2014/microdatos/NCV_Gastopersona_2014_concil_2010_dbf.zip", base) #guarda la pagina a la carpeta temporal
#baje la base de gastos realizados por cada integrante de hogar 
#de http://www.beta.inegi.org.mx/proyectos/enchogares/regulares/enigh/nc/2014/default.html


base<- read.csv("http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/regulares/enigh/nc/2014/microdatos/NCV_Gastopersona_2014_concil_2010_dbf.zip")
a<-base$gasto_tri

#baje la base de gastos realizados por cada integrante de hogar 
#de http://www.beta.inegi.org.mx/proyectos/enchogares/regulares/enigh/nc/2014/default.html

View(base)
