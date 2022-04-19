# --------------------------------------
# Búsqueda lineal con derivadas
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library(Deriv)

# Función a evaluar
f = function(x){
  x^4 - 16*x^2 + 5*x
}

# Límites
li = -4
ls = 4
par(mfrow=c(3,1))
curve(f, li, ls, lwd=2, col='red')

# Primera derivada
f1 = Deriv(f, nderiv=1)

# Graficación de la primera derivada
curve(f1, li, ls, lwd=2, col='red')
abline(h=0, lty=2)

# Segunda derivada
f2 = Deriv(f, nderiv=2)

# Variables a usar
paso = 0.000001
err = 0.00002

for(x in seq(li, ls, paso)){
  if(abs(f1(x)) <= err){
    points(x, f1(x))
    if(f2(x) > 0){
      cat('Es un minimo con x=', x, 'f(x)=', f(x), '\n')
    }
    else{
      cat('Es un maximo con x=', x, 'f(x)=', f(x), '\n')
    }
  }
}

# Impresión de resultados de iteraciones
cat('El total de iteraciones es:', length(seq(li, ls, paso)))