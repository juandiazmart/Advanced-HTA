2+2
10*2
20-8

help.start()

x<-rnorm(50) # Aqui hay un comentario

y<-rnorm(x)

plot(x,y,main = "Aqui va un titulo",sub="Aqui va un subtitulo")

ls()

rm(x,y)

x <- 1:20

w <- 1 + sqrt(x)/2

?plot

z<-rnorm(2*length(x))

length(z)
length(x)

juanpablo<-rnorm(100,mean=50,sd=20)

juanpablo

kg <- c(30,40,20,57,65)

dosis <- kg^2 + 100
dosis

######## Semana 2

x<-c(6,5,1,5,7)
c(1,3,4,2,6)->z # No es tan comun

1/x
y<-c(x,0,x)
length(y)

v<-x + y

v

getwd()  #### reconocer el directorio en el que estamos
setwd("~/") ### especifical el directorio que queremos trabajar

max(x)
?min
mean(x)
x<-c(x,NA)  
max(x,na.rm = T)  # na.rm remueve los missing values
mean(x,na.rm = T)

sum(x,na.rm = T)/(length(x)-1)

a<-1:30 # generar secuencias
b<-2*1:15

seq(from=1,to=30,by=1)
seq(-5,5,.2)

x
z<-x>6
#operadores logicos <,<=,>,>=,==,!=

x==6

x<-c(6,5,1,5,7)
x1<-x>6
x1
x2<-x!=7
x2

x1 & x2

x1 | x2


# missing values

x<-c(6,5,1,5,7)
x<-c(x,NA,NA,NA)  

is.na(x)

sum(is.na(x))

caracter<-c("hola","juan")
caracter
