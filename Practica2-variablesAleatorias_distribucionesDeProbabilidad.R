################################################################################
################ PROBABILIDAD Y VARIABLES ALEATORIAS ##########################

##########################################################################
### CÁLCULO DE COMBINACIONES
# Número de grupos distintos de tamaño m tomados a partir de n elementos,
# es decir, combinaciones de n elementos tomados de m en m
choose(n,m)

###################################
### VARIABLES ALEATORIAS DISCRETAS
x <- c(los valores que toma la variable aleatoria x separados por comas)
px <- c(la probabilidad de cada uno de los valores de x separados por comas)
Fx <- cumsum(px)
# Esperanza o media de una v.a.
EX <- sum(x*px)  
# también se puede hacer como
EX <- weighted.mean(x,px)
EX 

# Varianza de una v.a.
VARX <- sum((x-EX)^2 * px)
# o
x2 <-x^2
EX2 <- sum(x2*px)
VARX<-EX2- (EX^2) 

## Representación gráfica 

# pdf
plot(x,px,type="h",xlim=c(,),main="poner titulo") 
## type="l" une los puntos con lineas, type="h" dibuja una barra vertical en cada punto
## xlim define los límites inferior y superior del eje de las x (le tenemos que pasar un
## vector con los dos valores). También podemos usar los parámetros xlab="etiqueta eje x" 
## e ylab="etiqueta eje y" para poner nombres en los ejes
# plot(x,px,type="h",xlim=c(,),main="poner titulo",xlab="x",ylab="p(x)")

# cdf
plot.discreteCDF(x,Fx)
# Antes de usar la función plot.discreteCDF hay que ejecutar el siguiente código

#################################################################################################
######################################## desde aquí #############################################
#################################################################################################
## plot.discreteCDF es una funci?n que nos permite realizar las gráficas de funciones
## de distribución (cdf) para variables aleatorias discretas
## x: vector con los valores que toma la varialbe aleatoria X
## Fx: vector con las probabilidades acumuladas para los valores que hay en x (P(X <= x))
plot.discreteCDF <- function(x,Fx){
  # número de valores de x
  n <- length(x)
  # margen para la que representación del gráfico se vea mejor (espacio que dejamos antes 
  # del primer y despues del ultimo valor de x) 
  margen <- mean(x[2:n]- x[1:n-1])
  
  # Dibujamos un gráfico en blanco incluyendo solo los limites
  plot(x,Fx,type="n",xlab="x",ylab="F(x)",xlim=c(x[1]-margen,x[n]+margen),ylim=c(0,1))  
  
  # Dibujamos los segmentos horizontales sobre el grafico anterior
  segments(x[1]-margen,0,x[1],0)
  segments(x[1:n-1],Fx[1:n-1],x[2:n],Fx[1:n-1])
  segments(x[n],1,x[n]+margen,1)
  
  # Dibujamos los puntos para indicar el segmento donde esta incluido cada valor de x
  lines(x,Fx,type="p",pch=16)
  
  # Le ponemos título al gr?fico
  title("CDF")
}
#################################################################################################
######################################## hasta aquí #############################################
###################################################################################


####################################
### Variables Aleatorias Continuas 


## pdf
fx <- function(x){poner aquí la ecuación de la función de densidad}

## cdf
# si conocemos la fórmula la cdf la podemos definir de forma similar
Fx <- function(x){poner aquí la ecuación de la función de distribución acumulada}


## Integrar fx para calcular probabilidades del tipo P(a < X < b)
a<- limite inferior
b<- limite superior
integrate(fx,lower=a,upper=b)$value 


## E(X)
# si ya está definido x y fx:
xfx <- function(x){x*fx(x)}
EX <- integrate(xfx,lower=a ,upper=b)$value
EX

# si ya está definido x y fx
fx2 <- function(x){x^2*fx(x)}
EX2 <- integrate(fx2,lower=a ,upper=b)$value
EX2

## VAR(X)
VARX<-EX2- (EX^2) 
VARX



## Representación gráfica 

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
plot(x,y,type="l",xlim=c(a,b),main="poner título")
# otra forma de hacerlo
plot(Fx,limite inferior,limite superior)



######################
### MUESTREO #########

#Para establecer la semilla aleatoria y que todos obtengamos el mismo resultado
set.seed(valor de la semilla)

muestra <- sample(x, size, replace = FALSE, prob = NULL)
# x: el conjunto de valores de la variable aleatoria
# size: el tamaño de la muestra que queremos generar
# "replace" pude tomar valores TRUE o FALSE e indica si el muestro se hace con o sin reemplazamiento
# a "prob" se le pasa un vector del mismo tamaño que el que le pasamos en el parámetro "x", de esta forma le indicamos
# la probabilidad de elegir cada uno de los elementos en "x". Si no se le da ningún valor al parámetro "prob" R 
# entiende que todos los elementos en "x" tienen la misma probabilidad de ser elegidos.

# obtener gráfico de barras de la muestra
barplot(table(muestra))

######################################
### DISTRIBUCIONES DISCRETAS #########

### DISTRIBUCIÓN BINOMIAL

dbinom(x, size, prob) ## p(x)=P(X=x)
pbinom(x, size, prob) ## F(x)=P(X<=x)
qbinom(p, size, prob) ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) >= p
rbinom(n, size, prob) ## generar una muestra de tamaño n a partir de una distribución binomial

### DISTRIBUCIÓN DE POISSON

dpois(x, lambda)    ## p(x)=P(X=x)
ppois(x, lambda)    ## F(x)=P(X<=x)
qpois(p, lambda)    ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) >= p
rpois(n, lambda)    ## generar una muestra de tamaño n a partir de una distribución Poisson


######################################
### DISTRIBUCIONES CONTINUAS #########


### DISTRIBUCIÓN UNIFORME

dunif(x, min=a, max=b)  ## f(x)
punif(x, min=a, max=b)  ## F(x)=P(X<=x)
qunif(p, min=a, max=b)  ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) = p
runif(n, min=a, max=b)  ## generar una muestra de tamaño n a partir de una distribución uniforme


### DISTRIBUCI?N EXPONENCIAL

dexp(x, rate = valor de lambda)  ## f(x)
pexp(x, rate = valor de lambda)  ## F(x)=P(X<=x)
qexp(p, rate = valor de lambda)  ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) = p
rexp(n, rate = valor de lambda)  ## generar una muestra de tamaño n a partir de una distribución exponencial

### DISTRIBUCI?N GAMMA

dgamma(x, valor de alpha, valor de lambda)  ## f(x)
pgamma(x, valor de alpha, valor de lambda)  ## F(x)=P(X<=x)
qgamma(p, valor de alpha, valor de lambda)  ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) = p
rgamma(n, valor de alpha, valor de lambda)  ## generar una muestra de tamaño n a partir de una distribución exponencial

### DISTRIBUCIÓN NORMAL

dnorm(x, mean = media, sd = desvi.estand.)  ## f(x)
pnorm(x, mean = media, sd = desvi.estand.)  ## F(x)=P(X<=x)
qnorm(p, mean = media, sd = desvi.estand.)  ## calcular el cuantil sabiendo la probabilidad: Valor a que cumple que P(X<=a) = p
rnorm(n, mean = media, sd = desvi.estand.)  ## generar una muestra de tamaño n a partir de una distribución normal


