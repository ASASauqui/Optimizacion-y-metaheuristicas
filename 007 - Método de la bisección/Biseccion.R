# --------------------------------------
# Método de la bisección
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



rm(list=ls())
library(Deriv)

# Función a evaluar
f = function(x){
  # 0 <= x <= 2
  x^2 + 2*exp(-x)
  # x^2 - 16*x^2 + 5*x
}

# Derivadas
f1 = Deriv(f,nderiv=1)
f2 = Deriv(f1,nderiv=1)

# Límites
li = -4
ls = 4

# Graficación
par(mfrow=c(1,1))
curve(f1, li, ls, lwd=2, col='red')
abline(h=0, lty=2, col='gray')

# Inputs
a = 0
b = 2

points(a, f1(a), col='violet', pch=19)
points(b, f1(b), col='black', pch=19)
err = 1e-10

k = 0
while(abs(a-b) > err){
  k = k+1
  alpha = (a+b)/2
  points(alpha, f1(alpha), col='blue', pch=19)
  Sys.sleep(0.5)
  fa = f1(a)
  falpha = f1(alpha)
  
  if(fa*falpha < 0){
    b = alpha
  }
  else{
    a = alpha
  }
  curve(f1, li, ls, lwd=2, col='red')
  abline(h=0, lty=2, col='gray')
  points(a, f1(a), col='violet', pch=19)
  points(b, f1(b), col='black', pch=19)
  cat('Iteracion', k, 'a=',a, 'b=', b, '\n')
}

# Impresión de resultados
fa = f(a)
cat("X*= ", a, " f(a)= ", fa)