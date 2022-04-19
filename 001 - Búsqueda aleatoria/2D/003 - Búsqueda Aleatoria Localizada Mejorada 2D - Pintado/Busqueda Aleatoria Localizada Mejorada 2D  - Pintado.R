# --------------------------------------
# Búsqueda aleatoria localizada mejorada en 2D (pintado)
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



# Defino la función a optimizar
  fobjetivo = function(x){
    0.5*((x[1]^4 - 16*x[1]^2 + 5*x[1]) + (x[2]^4 - 16*x[2]^2 + 5*x[2]))
  }


# Dibujar la función que quiero optimizar

  # Selección de colores
  require(colorRamps)
  my.cols <- matlab.like(9)
  
  # Lista con puntos
  puntos = c()
  puntos1 = c()
  
  # Limites de graficación para x1 y x2
  x1 <- seq(-4, 4, length.out=100)
  x2 <- seq(-4, 4, length.out=100)
  
  # Crear mallado con x1 y x2, y evaluar cada par de puntos en la función fobjetivo
  z <- fobjetivo(expand.grid(x1, x2))
  
  # Gráfica de contorno
  
  mat = matrix(z$Var, length(x1))


# Paso 0 (inicialización)
x = c(4, 4)
fx = fobjetivo(x)
b = c(0, 0)
k = 0
iteracionesParciales = 500

# Búsqueda aleatoria localizada mejorada
while(k < iteracionesParciales){
  k = k + 1 
  
  # Paso 1:
  dk = rnorm(2, mean = 0, sd = 1.2) # sigma pequenna, saltos pequennos; sigma grande, saltos grandes
  xhat = x + b + dk
  fxhat = fobjetivo(xhat)
  
  if(fxhat < fx){
    x = xhat
    b = 0.2*b + 0.4*dk
    fx = fxhat
    puntos <- c(puntos,x[1], x[2])
  }
  else if(fobjetivo(x-dk+b) < fx){
    x = x + b - dk
    fx = fobjetivo(x-dk+b)
    b = b - 0.4*dk
    puntos <- c(puntos,x[1], x[2])
  }
  else{
      b = 0.5*b
      puntos1 <- c(puntos1,xhat[1], xhat[2])
  }
}

#Impresión de valores
options(digits=8) # Dígitos para los decimales
cat("Solución x1=", x[1], "x2=", x[2], "con valor f(x1,x2)=", fx, '\n')

#Impresión del gráfico
matriz = matrix(puntos, ncol=2, byrow=TRUE)
matriz1 = matrix(puntos1, ncol=2, byrow=TRUE)
filled.contour(x1, x2, mat, nlevels = 100, plot.axes = {
  points(matriz, col="green", pch=19)
  points(matriz1, col="red", pch=19)
  axis(1)
  axis(2)
  # contour(x1, x2, mat, add = TRUE, lwd = 1, lty = 2) # Descomentar en caso de querer ver los relieves marcados
}
)

