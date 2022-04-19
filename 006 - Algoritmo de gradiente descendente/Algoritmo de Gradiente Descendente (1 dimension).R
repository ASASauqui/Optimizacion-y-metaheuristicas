# --------------------------------------
# Algoritmo de gradiente descendente
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library(Deriv)

# Función a evaluar
f = function(x){
  x^4 - 10*x^2 + 5*x
}

# Derivadas
f1 = Deriv(f, nderiv=1)
f2 = Deriv(f1, nderiv=1)

# Límites
li = -4
ls = 4

# Graficación
par(mfrow=c(1,1))
curve(f, li, ls, lwd=2, col='red')

# Inputs
alpha = 1e-2
err = 1e-14
x = 0
k = 0

cat('Iteracion', 0, 'x=', x, 'f(x)=', f(x), 'f1(x)=', f1(x), '\n')

repeat{
  k = k+1
  Sys.sleep(1.0)
  xnew = x - alpha*f1(x)
  
  cat('Iteracion', k, 'x=', xnew, 'f(x)=', f(xnew), 'f1(x)=', f1(xnew), '\n')
  curve(f, li, ls, lwd=2, col='red')
  points(x, f(x), col='blue', pch=19)
  
  if(abs(f(xnew)-f(x)) < err){
    break
  }
  x = xnew
}