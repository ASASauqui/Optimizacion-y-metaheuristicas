# --------------------------------------
# Método de ajuste cuadrático
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



# Función a evaluar
f = function(x){
  x^4 + 2*exp(-x)
}

curve(f, 0, 2, col='red', lwd=2)

# Función del método de ajuste cuadrático
cuadratico = function(k, a, b, c){
  points(a, f(a), col='red', pch=19)
  points(b, f(b), col='black', pch=19)
  points(c, f(c), col='blue', pch=19)
  for(i in 1:k){
    Sys.sleep(1.0)
    x = (f(a)*(b^2-c^2) + f(b)*(c^2-a^2) + f(c)*(a^2-b^2))
    x = (1/2)*(x/(f(a)*(b-c) + f(b)*(c-a) + f(c)*(a-b)))
    
    if(x > b){
      if(f(x) > f(b)){
        c = x
      }
      else{
        a = b
        b = x
      }
    }
    else if(x < b){
      if(f(x) > f(b)){
        a = x
      }
      else{
        c = b
        b = x
      }
    }
    cat('Iteracion:', i, ' x=', b, ' f(x)=', f(b), '\n')
    curve(f, 0, 2, col='red', lwd=2)
    points(a, f(a), col='red', pch=19)
    points(b, f(b), col='black', pch=19)
    points(c, f(c), col='blue', pch=19)
  }
  
  res = c(b, f(b))
}

# Llamada a la función
res = cuadratico(30, 0, 1.5, 2)
res