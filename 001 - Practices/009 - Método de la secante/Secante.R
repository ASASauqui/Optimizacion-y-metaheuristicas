# --------------------------------------
# Método de la secante
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library(Deriv)

# Función a evaluar
f = function(x){
  x^4 - 16*x^2 + 5*x
}

# Derivadas
f1 = Deriv(f, nderiv = 1)
f2 = Deriv(f1,nderiv = 1)

# Función del método de la secante
secante = function(x1, x2, err){
  a = 0
  repeat{
    a = x2 - f1(x2)/((f1(x2)-f1(x1))/(x2-x1))
    if(f1(a) > 0){
      x2 = a
    }
    else{
      x1 = a 
    }
    if(f1(a) < err){
      break
    }
  }
  
  cat('x*=', a, 'f(x*)=', f(a))
}

# Llamada a la función
secante(-4, 4, 1e-9)