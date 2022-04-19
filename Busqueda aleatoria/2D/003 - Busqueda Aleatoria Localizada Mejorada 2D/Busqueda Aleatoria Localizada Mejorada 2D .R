# --------------------------------------
# Búsqueda aleatoria localizada mejorada en 2D
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



# Defino la función a optimizar
  fobjetivo = function(x){
    -20*exp((-0.2)*sqrt((0.5)*(x[1]^2+x[2]^2))) - exp((0.5)*(cos(2*pi*x[1])+cos(2*pi*x[2]))) + 20 + exp(1)
  }

# Dibujar la función que quiero optimizar

  # Selección de colores
  require(colorRamps)
  my.cols <- matlab.like(9)
  
  # Límites de graficación para x1 y x2
  x1 <- seq(-20, 20, length.out=100)
  x2 <- seq(-20, 20, length.out=100)
  
  # Crear mallado con x1 y x2, y evaluar cada par de puntos en la función fobjetivo
  z <- fobjetivo(expand.grid(x1, x2))
  
  # Gráfica de contorno
  mat = matrix(z$Var, length(x1))
  contour(x1, x2, mat, col=my.cols, lwd=1, lty=2, nlevels = 20, xlab='x1', ylab='x2')


  
#-------------------Repeticiones-------------------
#Variables iniciadoras
cont = 1
mejor = 1000000000
peor = -100000
promedio = 0
iteracionesGenerales = 30
iteracionesParciales = 10000

# Repeticiones generales
while(cont <= iteracionesGenerales){
  cont = cont + 1
  #Dimensiones
  minSpaceRange = -20
  maxSpaceRange = 20
  desviacion = 50.5
  prome = 0
  
  # Paso 0 (inicialización)
  x = c(20, 20)
  fx = fobjetivo(x)
  b = c(0, 0)
  k = 0
  areaFlag = 0
  
  points(x[1], x[2], col="blue", pch=19)
  
  # Búsqueda aleatoria localizada mejorada
  while(k < iteracionesParciales){
    k = k + 1 
    
    # Paso 1:
    repeat{
      areaFlag = 0
      dk = rnorm(2, mean = prome, sd = desviacion) # sigma pequenna, saltos pequennos; sigma grande, saltos grandes
      xhat = x + b + dk
      
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
      b = 0.2*b + 0.4*dk
      fx = fxhat
      points(x[1], x[2], col="green", pch=19)
    }
    #Paso 3
    else{
      repeat{
        areaFlag = 0
        xhat = x + b - dk
        for(i in 1:2){
          if(xhat[i] >= minSpaceRange && xhat[i] <= maxSpaceRange){
            areaFlag = areaFlag+1
          } 
        }
        if(areaFlag == 2){
          break;
        }
        else{
          dk = rnorm(2, mean = prome, sd = desviacion) # sigma pequenna, saltos pequennos; sigma grande, saltos grandes
        }
      }
      fxhat = fobjetivo(xhat)
      if(fxhat < fx){
        x = xhat
        b = b - 0.4*dk
        points(x[1], x[2], col="green", pch=19)
      }
      #Paso 4
      else{
        b = 0.5*b
        #points(xhat[1], xhat[2], col="red", pch=19)
      }
    }
  }
  
  options(digits=8) # Dígitos para los decimales
  cat("Solución x1=", x[1], "x2=", x[2], "con valor f(x1,x2)=", fx, '\n')
  if(fx < mejor){
    mejor = fx
  }
  if(fx > peor){
    peor = fx
  }
  promedio = promedio + fx
}


# Impresiones de resultados 
promedio
promedio = promedio/iteracionesGenerales
cat('El mejor es:', mejor)
cat('El peor es:', peor)
cat('El promedio es:', promedio)
