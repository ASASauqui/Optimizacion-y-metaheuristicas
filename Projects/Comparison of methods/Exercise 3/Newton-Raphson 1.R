# --------------------------------------
# Newton-Raphson 1
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

NewtonRaphson = function(x, y, err){
  repeat{
    points(x, y, col="green", pch=19)
    xprev = x
    yprev = y
    x = xprev - (fx1(x)/fx2(x))
    y = yprev - (fy1(y)/fy2(y))
    # cat(abs(sqrt(x^2 + y^2)), '\n')
    cat(x, ' ')
    cat(y, '\n')
    cat(abs(fxy(x,y)-fxy(xprev,yprev)) / abs(fxy(xprev,yprev)), '\n')
    if(abs(fxy(x,y)-fxy(xprev,yprev)) / abs(fxy(xprev,yprev)) < err ){
      points(x, y, col="red", pch=19)
      break
    }
  }
  cat('X*=', x, 'f(x*)=', fx(x), 'f1(x*)=', fx1(x), 'f2(x*)=', fx2(x), '\n')
  cat('y*=', y, 'f(y*)=', fy(y), 'f1(y*)=', fy1(y), 'f2(y*)=', fy2(y), '\n')
  cat('f(x,y) = ', fxy(x,y))
}


NewtonRaphson(-5, -5, 1e-2)

