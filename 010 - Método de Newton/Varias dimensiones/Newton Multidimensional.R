# --------------------------------------
# Método de Newton multidimensional
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library(Deriv)

# Graficacion
    fx = function(x){
      0.5*((x[1]^4 - 16*x[1]^2 + 5*x[1])+(x[2]^4 - 16*x[2]^2 + 5*x[2]))
    }
    
    require(colorRamps)
    my.cols <- matlab.like(9)
    
    # Limites de graficacion para x1 y x2
    x1 = seq(-4, 4, length.out=100)
    x2 = seq(-4, 4, length.out=100)
    
    # Crear mallado con x1 y x2
    z <- fx(expand.grid(x1,x2))
    
    # Grafica de contorno
    contour(x1, x2, matrix(z$Var, length(x1)), col=my.cols, lwd=1.8, lty=1, nlevels=14, xlab='x1', ylab='x2')


# Codigo normal
    # Función a evaluar
    f = function(x1, x2){
      0.5*((x1^4 - 16*x1^2 + 5*x1)+(x2^4 - 16*x2^2 + 5*x2))
    }
    # Derivadas
    f1 = Deriv(f, nderiv = 1)
    f2 = Deriv(f1, nderiv = 1)

# Inputs
    x = c(0,4)
    err1 = 1e-8
    err2 = 1e-8
    
    points(x[1], x[2], col='black', pch=19)


cat('x*=', x, ' f(x*)=', f(x[1],x[2]), 'norma=', sqrt(sum((f1(x[1],x[2]))^2)), '\n')
repeat{
  # Sys.sleep(1)
  fx = f(x[1],x[2])
  xprev = x
  
  fgradiente = f1(xprev[1],xprev[2])      #Gradiente
  
  h = matrix(f2(x[1],x[2]), 2, 2)         #Hessiano
  
  s = -solve(h) %*% fgradiente            #Buscar direccion
  x = x+s                                 #Actualizar el vector
  points(x[1], x[2], col='red', pch=19)
  fx1 = f(x[1],x[2])
  cat('x*=', x, ' f(x*)=', f(x[1],x[2]), 'norma=', sqrt(sum((f1(x[1],x[2]))^2)), '\n')
  if( abs(fx1-fx) < err1 || sqrt(sum((f1(x[1],x[2]))^2)) < err2){
    break
  }
}

# Impresión de resultados
cat('x*=', x, ' f(x*)=', f(x[1],x[2]))