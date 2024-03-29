# --------------------------------------
# M�todo de Newton-Raphson
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library(Deriv)

# Funci�n a evaluar
f = function(x){
  x^4 - 16*x^2 + 5*x
}

# Derivadas
f1 = Deriv(f, nderiv = 1)
f2 = Deriv(f1,nderiv = 1)

# Funci�n del m�todo de Newton-Raphson
NewtonRaphson = function(x, err){
  repeat{
    xprev = x
    x = xprev - (f1(x)/f2(x))
    if( abs(x-xprev) < err ){
      break
    }
  }
  cat('X*=', x, 'f(x*)=', f(x), 'f1(x*)=', f1(x), 'f2(x*)=', f2(x))
}

# Llamada a la funci�n
NewtonRaphson(-1.5, 1e-4)