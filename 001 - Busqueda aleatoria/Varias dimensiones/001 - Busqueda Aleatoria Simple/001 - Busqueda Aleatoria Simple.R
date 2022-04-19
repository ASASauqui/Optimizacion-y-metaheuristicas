# --------------------------------------
# Búsqueda aleatoria simple en varias dimensiones
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



# Defino la función a optimizar
  fobjetivo = function(x){
    6*x[1] + 12*x[2] + 10*x[3]
  }

#Dimensiones
noDim = 3

# Paso 0 (inicialización)
x = c(0, 0, 0)
fx = fobjetivo(x)
k = 0

# Búsqueda aleatoria simple
while(k < 500000){
  k = k + 1 
  
  # Paso 1:
  xhat = runif(noDim, min=0, max=300)
  fxhat = fobjetivo(xhat)
  
  # Ejemplo de restricciones (si no se desean, se pueden comentar)
  if((5*xhat[1]+8*xhat[2]+13*xhat[3] <= 4000) && (12*xhat[1]+18*xhat[2]+14*xhat[3] <= 6000) && (xhat[1]+xhat[2]+xhat[3] <= 600) && (xhat[1] >= 200)){
    if(fxhat > fx){
      x = xhat
      fx = fxhat
    }
  }
}

# Impresión de resultados
options(digits=8) # Dígitos para los decimales
for(i in 1:noDim){
  cat('Solución x',i, ': = ', x[i], '\n')
}
cat('f(x1,x2,x3) =', fx)