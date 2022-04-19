# --------------------------------------
# Recocido simulado
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



# Defino la funcion a optimizar
fobjetivo = function(x){
  0.5 * ((x[1]^4 - 16*x[1]^2 + 5*x[1]) + (x[2]^4 - 16*x[2]^2 + 5*x[2]))
}

# Dibujar la funcion que quiero optimizar

# seleccion de colores
require(colorRamps)
my.cols <- matlab.like(9)

# Limites de graficacion para x1 y x2
x1 <- seq(-5, 5, length.out=100)
x2 <- seq(-5, 5, length.out=100)

# crear mallado con x1 y x2, y evaluar cada par de puntos en la funcion fr
z <- fobjetivo(expand.grid(x1, x2))

# grafica de contorno
par(mfrow=c(2,1))
contour(x1, x2, matrix(z$Var, length(x1)), col=my.cols, lwd=1.8, lty=2, nlevels = 12, xlab='x1', ylab='x2')




#-------------------Repeticiones-------------------
  
  #Dimensiones
      minSpaceRange = -5
      maxSpaceRange = 5
  
  # Paso 0 (inicializacion)
      x = c(4, 5)
      fx = fobjetivo(x)
      k = 0
      t = 1000
      err = 1e-70
      alfa = 0.99
      areaFlag = 0
      temperatura = c(t)
  
      points(x[1], x[2], col="blue", pch=19)
      
  # Variables grafica temperatura
      yTemperatura = c(t)
  
  while(k < 2000 && t > err){
    k = k + 1 
    
    # Paso 1: Dentro del espacio de búsqueda
        repeat{
          areaFlag = 0
          dk = rnorm(2, mean = 0, sd = 0.6) # sigma pequenna, saltos pequennos; sigma grande, saltos grandes
          xhat = x + dk
          
          for(i in 1:2){
            if(xhat[i] >= minSpaceRange && xhat[i] <= maxSpaceRange){
              areaFlag = areaFlag+1
            }
          }
          if(areaFlag == 2){
            break;
          }
        }
        
        fxhat = fobjetivo(xhat)
    
    #Paso 2
        if(fxhat < fx){
          x = xhat
          fx = fxhat
          points(x[1], x[2], col="green", pch=20)
        }
        else{
          if(runif(1,0,1) < exp((fobjetivo(x)-fobjetivo(xhat))/t) ){
            x = xhat
            points(x[1], x[2], col="green", pch=20)
          }
          else{
            x = x
            points(x[1], x[2], col="red", pch=20)
          }
        }
    
    # Paso 3
        t = alfa*t
        yTemperatura = c(yTemperatura, t)
  }
      
      
  # Impresión de resultados
    plot(yTemperatura, type="l",col="blue",lwd=3)
    options(digits=8) # digitos para los decimales
    cat("Solucion x1=", x[1], "x2=", x[2], "con valor f(x1,x2)=", fx, '\n')