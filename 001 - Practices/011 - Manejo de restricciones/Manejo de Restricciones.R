# --------------------------------------
# Manejo de restricciones
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library(Deriv)

# Graficacion
    # Función objetivo
    fx = function(x){
      ((x[1]-1)^2) + ((x[2]-5)^2)
    }
    # Restricción 1
    res1 = function(x){
      (-x[1]^2) + x[2] - 4
    }
    # Restricción 2
    res2 = function(x){
      (-(x[1]-2)^2) + x[2] - 3
    }

# Colores
    require(colorRamps)
    my.cols <- matlab.like(9)

# Limites de graficacion para x1 y x2
    x1 = seq(-2, 10, length.out=100)
    x2 = seq(-2, 10, length.out=100)

# Crear mallado con x1 y x2
    z <- fx(expand.grid(x1,x2))

# Grafica de contorno
    contour(x1, x2, matrix(z$Var, length(x1)), col=my.cols, lwd=1.8, lty=1, nlevels=14, xlab='x1', ylab='x2')
    lines(x2, x1^2 + 4, col = 'blue', lwd=2)
    lines(x2, (x1-2)^2 + 3, col = 'brown', lwd=2)


    
    
    
    
# Codigo normal

mejor = 10000
for(i in 1:100000){
  r1 = runif(1,0,1)
  r2 = runif(1,0,1)
  
  f = function(x1, x2){
    (x1-1)^2 + (x2-5)^2 + r1*(-x1^2 + x2 - 4) + r2*(-(x1-2)^2 + x2 - 3)
  }
  
  f1 = Deriv(f, nderiv = 1)
  f2 = Deriv(f1, nderiv = 1)
  
  # Inputs
  x = c(10,-2)
  err1 = 1e-10
  err2 = 1e-10
  
  points(x[1], x[2], col='black', pch=19)
  
  repeat{
    # Sys.sleep(1)
    fxx = f(x[1],x[2])
    xprev = x
    
    fgradiente = f1(xprev[1],xprev[2])      #Gradiente
    
    h = matrix(f2(x[1],x[2]), 2, 2)         #Hessiano
    
    s = -solve(h) %*% fgradiente            #Buscar direccion
    x = x+s                                 #Actualizar el vector
    # points(x[1], x[2], col='violet', pch=19)
    fx1 = f(x[1],x[2])
    if( abs(fx1-fxx) < err1 || sqrt(sum((f1(x[1],x[2]))^2)) < err2){
      break
    }
  }
  
  if(res1(x) <= 0 && res2(x) <= 0){
    if(fx(x) < mejor){
      points(x[1], x[2], col='violet', pch=19)
      cat('r1=', r1, 'r2=', r2, '\n')
      mejor = fx(x)
      cat('x*=', x, ' f(x*)=', fx(x), 'res1=', res1(x), 'res2=', res2(x), '\n')
    }
  }
}
