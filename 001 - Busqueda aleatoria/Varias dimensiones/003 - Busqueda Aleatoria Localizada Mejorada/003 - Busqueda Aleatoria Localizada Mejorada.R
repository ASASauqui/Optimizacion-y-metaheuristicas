# --------------------------------------
# Búsqueda aleatoria localizada mejorada en varias dimensiones
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
maxSpaceRange = 300
desviacion = 1.3
promedio = 1

# Paso 0 (inicialización)
x = c(0, 0, 0) #Cambiar manualmente
fx = fobjetivo(x)
b = c(0, 0, 0) #Cambiar manualmente
k = 0
areaFlag = 0


while(k < 5000){
  k = k + 1 
  
  # Paso 1:
  repeat{ # Ver si está dentro del espacio de búsqueda
    areaFlag = 0
    dk = rnorm(noDim, mean = promedio, sd = desviacion) # sigma pequenna, saltos pequennos; sigma grande, saltos grandes
    xhat = x + b + dk
    
    for(i in 1:noDim){
      if(xhat[i] >= minSpaceRange && xhat[i] <= maxSpaceRange){
        areaFlag = areaFlag+1
      } 
    }
    if(areaFlag == noDim){ # Condiciones de salida
      # Ejemplo de restricciones (si no se desean, se pueden comentar)
      if((5*xhat[1]+8*xhat[2]+13*xhat[3] <= 4000) && (12*xhat[1]+18*xhat[2]+14*xhat[3] <= 6000) && (xhat[1]+xhat[2]+xhat[3] <= 600)){ #Quitar esto)
        break;
      }
    }
  }
  fxhat = fobjetivo(xhat)
  
  #Paso 2
  if(fxhat > fx){
    x = xhat
    b = 0.2*b + 0.4*dk
    fx = fxhat
  }
  #Paso 3
  else{
    repeat{ #Ver si está dentro del espacio de búsqueda
      areaFlag = 0
      xhat = x + b - dk
      for(i in 1:noDim){
        if(xhat[i] >= minSpaceRange && xhat[i] <= maxSpaceRange){
          areaFlag = areaFlag+1
        } 
      }
      if(areaFlag == noDim){ #Condiciones de salida
        # Ejemplo de restricciones (si no se desean, se pueden comentar)
        if((5*xhat[1]+8*xhat[2]+13*xhat[3] <= 4000) && (12*xhat[1]+18*xhat[2]+14*xhat[3] <= 6000) && (xhat[1]+xhat[2]+xhat[3] <= 600)){ #Quitar esto)
          break;
        }
        dk = rnorm(noDim, mean = promedio, sd = desviacion) # sigma pequenna, saltos pequennos; sigma grande, saltos grandes
      }
      else{
        dk = rnorm(noDim, mean = promedio, sd = desviacion) # sigma pequenna, saltos pequennos; sigma grande, saltos grandes
      }
    }
    fxhat = fobjetivo(xhat)
    if(fxhat > fx){
      x = xhat
      b = b - 0.4*dk
    }
    #Paso 4
    else{
      b = 0.5*b
    }
  }
}

# Impresión de resultados
options(digits=8) # Dígitos para los decimales
for(i in 1:noDim){
  cat('Solución x',i, ': = ', x[i], '\n')
}
cat('f(x1,x2,x3) =', fx)
