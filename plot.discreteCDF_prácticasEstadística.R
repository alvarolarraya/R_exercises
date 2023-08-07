# Antes de usar la funcion plot.discreteCDF hay que ejecutar el siguiente código
#################################################################################################
#################################################################################################
## plot.discreteCDF es una función que nos permite realizar las gráficas de funciones
## de distribución (cdf) para variables aleatorias discretas
## x: vector con los valores que toma la variable aleatoria X
## Fx: vector con las probabilidades acumuladas para los valores que hay en x (P(X <= x))
plot.discreteCDF <- function(x,Fx){
  # número de valores de x
  n <- length(x)
  # margen para la que representación del gráfico se vea mejor (espacio que dejamos antes
  # del primer y después del ultimo valor de x)
  margen <- mean(x[2:n]- x[1:n-1])
 
  # Dibujamos un gráfico en blanco incluyendo solo los límites
  plot(x,Fx,type="n",xlab="x",ylab="F(x)",xlim=c(x[1]-margen,x[n]+margen),ylim=c(0,1)) 
 
  # Dibujamos los segmentos horizontales sobre el gráfico anterior
  segments(x[1]-margen,0,x[1],0)
  segments(x[1:n-1],Fx[1:n-1],x[2:n],Fx[1:n-1])
  segments(x[n],1,x[n]+margen,1)
 
  # Dibujamos los puntos para indicar el segmento donde esta incluido cada valor de x
  lines(x,Fx,type="p",pch=16)
 
  # Le ponemos titulo al gráfico
  title("CDF")
}

