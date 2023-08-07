

############################ PRÁCTICA 4 y 5 


#====================== Intervalos de Confianza

#### NOTA: Todas las funciones utilizadas en esta práctica están en la librería PASWR2.
#### Cargar la librería antes de comenzar la práctica

library(PASWR2)

##### Estudiar la normalidad de los datos (suponemos que los datos están guardados en un objeto llamado 'datos') 

qqnorm(datos)
qqline(datos)


###################################################################################
####### SIMULACIÓN DE INTERVALOS DE CONFIANZA CON MUESTRAS OBTENIDAS DE UNA NORMAL

norsim <- function(sims = 100, n = 36, mu = 100, sigma = 18, 
                   conf.level = 0.95){
  alpha <- 1 - conf.level
  CL <- conf.level * 100
  ll <- numeric(sims)
  ul <- numeric(sims)
  for (i in 1:sims){
    xbar <- mean(rnorm(n , mu, sigma))
    ll[i] <- xbar - qnorm(1 - alpha/2)*sigma/sqrt(n)
    ul[i] <- xbar + qnorm(1 - alpha/2)*sigma/sqrt(n)
  }
  notin <- sum((ll > mu) + (ul < mu))
  percentage <- round((notin/sims) * 100, 2)
  SCL <- 100 - percentage
  plot(ll, type = "n", ylim = c(min(ll), max(ul)), xlab = " ", 
       ylab = " ")
  for (i in 1:sims) {
    low <- ll[i]
    high <- ul[i]
    if (low < mu & high > mu) {
      segments(i, low, i, high)
    }
    else if (low > mu & high > mu) {
      segments(i, low, i, high, col = "red", lwd = 5)
    }
    else {
      segments(i, low, i, high, col = "blue", lwd = 5)
    }
  }
  abline(h = mu)
  cat(SCL, "\b% of the random confidence intervals contain Mu =", mu, "\b.", "\n")
}

# uso de norsim: calcula y hace la gráfica con tantos intervalos de confinza como el valor indicado en "sims" y 
# con el nivel de confianza indicado en "conf.level". Cada intervalo de confianza se calcula de una muestra de 
# tamaño n de una normal con media "mu" y desviación típica "sigma". 
norsim(sims="numero de intervalos a simular", n="tamaño de la muestra", mu="media poblacional",
       sigma="desviación típica poblacional", conf.level="nivel de confianza de los intervalos")

# En lugar de implementarla, podemos usar la función cisim del paquete PASWR2"


###############################################################
####### COMANDOS PARA EL CÁLCULO DE LOS INTERVALOS DE CONFIANZA

## MEDIA POBLACIONAL( SIGMA CONOCIDA ) 
# si tenemos la muestra (el conjunto de datos)
z.test(x=datos , sigma.x=valor.de.sigma , conf.level = 0.95)$conf
# si no tenemos la muestra (el conjunto de datos) solo el valor de la media muestral
zsum.test(mean.x=media.muestral, sigma.x = valor.de.sigma , n.x = tamaño.muestra, conf.level = 0.95)$conf

## MEDIA POBLACIONAL ( SIGMA DESCONOCIDA ) 
# si tenemos la muestra (el conjunto de datos)
t.test(x=datos , conf.level = 0.95)$conf
# si no tenemos la muestra (el conjunto de datos) solo el valor de la media muestral
tsum.test(mean.x=media.muestral, s.x = desviacion.tipica.muestral , n.x = tamaño.muestra, conf.level = 0.95)$conf


## DIFERENCIA DE MEDIAS ( SIGMAS CONOCIDAS )
# si tenemos la muestra (el conjunto de datos)
z.test(x=datos.x, y=datos.y, sigma.x=valor.de.sigmax, sigma.y=valor.de.sigmay, conf.level = 0.95)$conf
# si no tenemos la muestra (el conjunto de datos) solo el valor de la media muestral
zsum.test(mean.x=media.muestral.x, sigma.x=valor.de.sigmax, n.x=tamaño.muestra.x,
          mean.y=media.muestral.y, sigma.y=valor.de.sigmay, n.y=tamaño.muestra.y, conf.level = 0.95)$conf

## DIFERENCIA DE MEDIAS ( SIGMAS DESCONOCIDAS )

####### SIGMAS DESCONOCIDAS pero IGUALES
# si tenemos la muestra (el conjunto de datos)
t.test(x=datos.x, y=datos.y, var.equal=TRUE, conf.level = 0.95)$conf
# si no tenemos la muestra (el conjunto de datos) solo el valor de la media muestral
tsum.test(mean.x=media.muestral, s.x = desviacion.tipica.muestral , n.x = tamaño.muestra, 
          mean.y=media.muestral, s.y = desviacion.tipica.muestral , n.y = tamaño.muestra, 
          var.equal=TRUE, conf.level = 0.95)$conf

####### SIGMAS DESCONOCIDAS pero DISTINTAS
# si tenemos la muestra (el conjunto de datos)
t.test(x=datos.x, y=datos.y, var.equal=FALSE, conf.level = 0.95)$conf
# si no tenemos la muestra (el conjunto de datos) solo el valor de la media muestral
tsum.test(mean.x=media.muestral, s.x = desviacion.tipica.muestral , n.x = tamaño.muestra, 
          mean.y=media.muestral, s.y = desviacion.tipica.muestral , n.y = tamaño.muestra, 
          var.equal=FALSE, conf.level = 0.95)$conf

## DIFERENCIA DE MEDIAS ( MUESTRAS PAREADAS )
t.test(x=datos.x, y=datos.y, paired=TRUE, conf.level = 0.95)$conf

## VARIANZA POBLACIONAL

s2<- var(datos)
conf.level<-0.95
n<-length(datos)
Dfchisq <- n-1

ChiIzda <- qchisq((1-conf.level)/2,Dfchisq) ## Cuantil cola Izquierda
ChiDcha <- qchisq(conf.level+(1-conf.level)/2,Dfchisq) ## Cuantil cola Derecha

round(c((n-1)*s2/ChiDcha, (n-1)*s2/ChiIzda),2) ## Intervalo de Confianza


## COCIENTE DE VARIANZAS


var.test(x=datos.x, y=datos.y, conf.level = 0.95)$conf


## PROPORCION POBLACIONAL

prop.test(x=Casos.favorables, n=tamaño.muestral, conf.level = 0.95,correct=FALSE)$conf  #### Sin corregir

prop.test(x=Casosfavorables, n=tamaño.muestral, conf.level = 0.95,correct=TRUE)$conf   #### Corregido

############################################################################################
####### CALCULO DEL TAMAÑO DE MUESTRA NECESARIO PARA ESTIMAR LA MEDIA DADO EL MARGEN DE ####
####### ERROR b Y EL NIVEL DE CONFIANZA                                                 ####
############################################################################################

nsize(b=margen.de.error, sigma=valor.de.sigma, conf.level=0.95, type = "mu")
