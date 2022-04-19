# --------------------------------------
# Secante 1
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library(Deriv)

require(colorRamps)
my.cols <- matlab.like(9)

fobjetivo = function(x){
  x[1]^2 + x[2]^2 - 2*x[1]
}

# Limites de graficacion para x1 y x2
x1 <- seq(-6, 6, length.out=100)
x2 <- seq(-6, 6, length.out=100)

# crear mallado con x1 y x2, y evaluar cada par de puntos en la funcion fr
z <- fobjetivo(expand.grid(x1, x2))

# grafica de contorno
contour(x1, x2, matrix(z$Var, length(x1)), col=my.cols, lwd=1.8, lty=2, nlevels = 14, xlab='x1', ylab='x2')

fx = function(x){
  x^2 - 2*x
}

fy = function(y){
  y^2
}

fx1 = Deriv(fx, nderiv = 1)
fx2 = Deriv(fx1,nderiv = 1)

fy1 = Deriv(fy, nderiv = 1)
fy2 = Deriv(fy1,nderiv = 1)

fxy = function(x, y){
  x^2 + y^2 - 2*x
}




secante = function(x1, x2, err){
  ax = 0
  ay = 0
  repeat{
    points(x1[1], x1[2], col="green", pch=19)
    points(x2[1], x2[2], col="blue", pch=19)
    ax = x2[1] - fx1(x2[1])/((fx1(x2[1])-fx1(x1[1]))/(x2[1]-x1[1]))
    ay = x2[2] - fy1(x2[2])/((fy1(x2[2])-fy1(x1[2]))/(x2[2]-x1[2]))
    points(ax, ay, col="red", pch=19)
    if(fx1(ax) > 0){
      x2[1] = ax
    }
    else{
      x1[1] = ax 
    }
    if(fy1(ay) > 0){
      x2[2] = ay
    }
    else{
      x1[2] = ay 
    }
    # cat(x1, ' ')
    # cat(x2, '\n')
    # cat(sqrt((fx1(ax)^2)+(fy1(ay)^2)),'\n')
    # cat(ax, '  ', ay,'\n')
    cat(sqrt((fx1(ax)^2)+(fy1(ay)^2)),'\n')
    if(sqrt((fx1(ax)^2)+(fy1(ay)^2)) < err){
      break
    }
  }
  
  cat('x*=', ax, 'f(x*)=', fx(ax), '\n')
  cat('y*=', ay, 'f(y*)=', fy(ay), '\n')
  cat('f(x,y)*=', fxy(ax,ay))
}

secante(c(-5,-5), c(4,4), 1e-8)

