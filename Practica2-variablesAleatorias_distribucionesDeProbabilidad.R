################################################################################
################ PROBABILIDAD Y VARIABLES ALEATORIAS ##########################

##########################################################################
### C�LCULO DE COMBINACIONES
# N�mero de grupos distintos de tama�o m tomados a partir de n elementos,
# es decir, combinaciones de n elementos tomados de m en m
choose(n,m)

###################################
### VARIABLES ALEATORIAS DISCRETAS
x <- c(los valores que toma la variable aleatoria x separados por comas)
px <- c(la probabilidad de cada uno de los valores de x separados por comas)
Fx <- cumsum(px)
# Esperanza o media de una v.a.
EX <- sum(x*px)  
# tambi�n se puede hacer como
EX <- weighted.mean(x,px)
EX 

# Varianza de una v.a.
VARX <- sum((x-EX)^2 * px)
# o
x2 <-x^2
EX2 <- sum(x2*px)
VARX<-EX2- (EX^2) 

## Representaci�n gr�fica 

# pdf
plot(x,px,type="h",xlim=c(,),main="poner titulo") 
## type="l" une los puntos con lineas, type="h" dibuja una barra vertical en cada punto
## xlim define los l�mites inferior y superior del eje de las x (le tenemos que pasar un
## vector con los dos valores). Tambi�n podemos usar los par�metros xlab="etiqueta eje x" 
## e ylab="etiqueta eje y" para poner nombres en los ejes
# plot(x,px,type="h",xlim=c(,),main="poner titulo",xlab="x",ylab="p(x)")

# cdf
plot.discreteCDF(x,Fx)
# Antes de usar la funci�n plot.discreteCDF hay que ejecutar el siguiente c�digo

#################################################################################################
######################################## desde aqu� #############################################
#################################################################################################
## plot.discreteCDF es una funci?n que nos permite realizar las gr�ficas de funciones
## de distribuci�n (cdf) para variables aleatorias discretas
## x: vector con los valores que toma la varialbe aleatoria X
## Fx: vector con las probabilidades acumuladas para los valores que hay en x (P(X <= x))
plot.discreteCDF <- function(x,Fx){
  # n�mero de valores de x
  n <- length(x)
  # margen para la que representaci�n del gr�fico se vea mejor (espacio que dejamos antes 
  # del primer y despues del ultimo valor de x) 
  margen <- mean(x[2:n]- x[1:n-1])
  
  # Dibujamos un gr�fico en blanco incluyendo solo los limites
  plot(x,Fx,type="n",xlab="x",ylab="F(x)",xlim=c(x[1]-margen,x[n]+margen),ylim=c(0,1))  
  
  # Dibujamos los segmentos horizontales sobre el grafico anterior
  segments(x[1]-margen,0,x[1],0)
  segments(x[1:n-1],Fx[1:n-1],x[2:n],Fx[1:n-1])
  segments(x[n],1,x[n]+margen,1)
  
  # Dibujamos los puntos para indicar el segmento donde esta incluido cada valor de x
  lines(x,Fx,type="p",pch=16)
  
  # Le ponemos t�tulo al gr?fico
  title("CDF")
}
#################################################################################################
######################################## hasta aqu� #############################################
###################################################################################


####################################
### Variables Aleatorias Continuas 


## pdf
fx <- function(x){poner aqu� la ecuaci�n de la funci�n de densidad}

## cdf
# si conocemos la f�rmula la cdf la podemos definir de forma similar
Fx <- function(x){poner aqu� la ecuaci�n de la funci�n de distribuci�n acumulada}


## Integrar fx para calcular probabilidades del tipo P(a < X < b)
a<- limite inferior
b<- limite superior
integrate(fx,lower=a,upper=b)$value 


## E(X)
# si ya est� definido x y fx:
xfx <- function(x){x*fx(x)}
EX <- integrate(xfx,lower=a ,upper=b)$value
EX

# si ya est� definido x y fx
fx2 <- function(x){x^2*fx(x)}
EX2 <- integrate(fx2,lower=a ,upper=b)$value
EX2

## VAR(X)
VARX<-EX2- (EX^2) 
VARX



## Representaci�n gr�fica 

# pdf
a <- limite inferior
b <- limite superior
x <- seq(a,b,0.01)
y <- fx(x)
plot(x,y,type="l",xlim=c(a,b),main="poner titulo")
# otra forma de hacerlo
# plot(fx,limite inferior,limite superior)


# cdf
a <- limite inferior
b <- limite superior
x <- seq(a,b,0.01)
y <- Fx(x)
plot(x,y,type="l",xlim=c(a,b),main="poner t�tulo")
# otra forma de hacerlo
plot(Fx,limite inferior,limite superior)



######################
### MUESTREO #########

#Para establecer la semilla aleatoria y que todos obtengamos el mismo resultado
set.seed(valor de la semilla)

muestra <- sample(x, size, replace = FALSE, prob = NULL)
# x: el conjunto de valores de la variable aleatoria
# size: el tama�o de la muestra que queremos generar
# "replace" pude tomar valores TRUE o FALSE e indica si el muestro se hace con o sin reemplazamiento
# a "prob" se le pasa un vector del mismo tama�o que el que le pasamos en el par�metro "x", de esta forma le indicamos
# la probabilidad de elegir cada uno de los elementos en "x". Si no se le da ning�n valor al par�metro "prob" R 
# entiende que todos los elementos en "x" tienen la misma probabilidad de ser elegidos.

# obtener gr�fico de barras de la muestra
barplot(table(muestra))

######################################
### DISTRIBUCIONES DISCRETAS #########

### DISTRIBUCI�N BINOMIAL

dbinom(x, size, prob) ## p(x)=P(X=x)
pbinom(x, size, prob) ## F(x)=P(X<=x)
qbinom(p, size, prob) ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) >= p
rbinom(n, size, prob) ## generar una muestra de tama�o n a partir de una distribuci�n binomial

### DISTRIBUCI�N DE POISSON

dpois(x, lambda)    ## p(x)=P(X=x)
ppois(x, lambda)    ## F(x)=P(X<=x)
qpois(p, lambda)    ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) >= p
rpois(n, lambda)    ## generar una muestra de tama�o n a partir de una distribuci�n Poisson


######################################
### DISTRIBUCIONES CONTINUAS #########


### DISTRIBUCI�N UNIFORME

dunif(x, min=a, max=b)  ## f(x)
punif(x, min=a, max=b)  ## F(x)=P(X<=x)
qunif(p, min=a, max=b)  ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) = p
runif(n, min=a, max=b)  ## generar una muestra de tama�o n a partir de una distribuci�n uniforme


### DISTRIBUCI?N EXPONENCIAL

dexp(x, rate = valor de lambda)  ## f(x)
pexp(x, rate = valor de lambda)  ## F(x)=P(X<=x)
qexp(p, rate = valor de lambda)  ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) = p
rexp(n, rate = valor de lambda)  ## generar una muestra de tama�o n a partir de una distribuci�n exponencial

### DISTRIBUCI?N GAMMA

dgamma(x, valor de alpha, valor de lambda)  ## f(x)
pgamma(x, valor de alpha, valor de lambda)  ## F(x)=P(X<=x)
qgamma(p, valor de alpha, valor de lambda)  ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) = p
rgamma(n, valor de alpha, valor de lambda)  ## generar una muestra de tama�o n a partir de una distribuci�n exponencial

### DISTRIBUCI�N NORMAL

dnorm(x, mean = media, sd = desvi.estand.)  ## f(x)
pnorm(x, mean = media, sd = desvi.estand.)  ## F(x)=P(X<=x)
qnorm(p, mean = media, sd = desvi.estand.)  ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) = p
rnorm(n, mean = media, sd = desvi.estand.)  ## generar una muestra de tama�o n a partir de una distribuci�n normal


