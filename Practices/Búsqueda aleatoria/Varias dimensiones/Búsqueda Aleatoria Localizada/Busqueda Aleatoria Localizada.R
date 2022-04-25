# --------------------------------------
# Búsqueda aleatoria localizada en varias dimensiones
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



# Defino la función a optimizar
  fobjetivo = function(x){
    6*x[1] + 12*x[2] + 10*x[3]
  }

#Dimensiones
noDim = 3
minSpaceRange = 0
maxSpaceRange = 500

# Paso 0 (inicialización)
x = c(200, 0, 0)
fx = fobjetivo(x)
k = 0
areaFlag = 0

# Búsqueda aleatoria localizada
while(k < 50000){
  k = k + 1 
  
  # Paso 1: Dentro del espacio de búsqueda
  repeat{
    areaFlag = 0
    dk = rnorm(noDim, mean = 0, sd = 1) # sigma pequenna, saltos pequennos; sigma grande, saltos grandes
    xhat = x + dk
    
    for(i in 1:noDim){
      if(xhat[i] >= minSpaceRange && xhat[i] <= maxSpaceRange){
        areaFlag = areaFlag+1
      } 
    }
    if(areaFlag == noDim){ #Condiciones de salida
      # Ejemplo de restricciones (si no se desean, se pueden comentar)
      if((5*xhat[1]+8*xhat[2]+13*xhat[3] <= 4000) && (12*xhat[1]+18*xhat[2]+14*xhat[3] <= 6000) && (xhat[1]+xhat[2]+xhat[3] <= 600)){ #Quitar esto
        break;
      }
    }
  }
  fxhat = fobjetivo(xhat)
  
  #Paso 2
  if(fxhat > fx){
    x = xhat
    fx = fxhat
  }
}

# Impresión de resultados
options(digits=8) # Dígitos para los decimales
for(i in 1:noDim){
  cat('Solución x',i, ': = ', x[i], '\n')
}
cat('f(x1,x2,x3) =', fx)