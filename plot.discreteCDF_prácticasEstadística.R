# Antes de usar la funcion plot.discreteCDF hay que ejecutar el siguiente c�digo
#################################################################################################
#################################################################################################
## plot.discreteCDF es una funci�n que nos permite realizar las gr�ficas de funciones
## de distribuci�n (cdf) para variables aleatorias discretas
## x: vector con los valores que toma la variable aleatoria X
## Fx: vector con las probabilidades acumuladas para los valores que hay en x (P(X <= x))
plot.discreteCDF <- function(x,Fx){
  # n�mero de valores de x
  n <- length(x)
  # margen para la que representaci�n del gr�fico se vea mejor (espacio que dejamos antes
  # del primer y despu�s del ultimo valor de x)
  margen <- mean(x[2:n]- x[1:n-1])
 
  # Dibujamos un gr�fico en blanco incluyendo solo los l�mites
  plot(x,Fx,type="n",xlab="x",ylab="F(x)",xlim=c(x[1]-margen,x[n]+margen),ylim=c(0,1)) 
 
  # Dibujamos los segmentos horizontales sobre el gr�fico anterior
  segments(x[1]-margen,0,x[1],0)
  segments(x[1:n-1],Fx[1:n-1],x[2:n],Fx[1:n-1])
  segments(x[n],1,x[n]+margen,1)
 
  # Dibujamos los puntos para indicar el segmento donde esta incluido cada valor de x
  lines(x,Fx,type="p",pch=16)
 
  # Le ponemos titulo al gr�fico
  title("CDF")
}

