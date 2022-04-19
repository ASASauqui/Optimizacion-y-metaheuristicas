# --------------------------------------
# Método de la sección dorada
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



# Función a evaluar
f = function(x){
  x^2 + 2*exp(-x)
}

eAbs = function(x, y){
  abs(x-y)
}

# Función del método de la sección dorada
dorada = function(a,b,e){
  tao = 1/1.618033
  
  alfa1 = a*(1-tao) + b*tao
  alfa2 = a*tao + b*(1-tao)
  
  while(eAbs(f(alfa1), f(alfa2)) > e){
    if(f(alfa1) > f(alfa2)){
      b = alfa1
      alfa1 = alfa2
      alfa2 = a*tao + b*(1-tao)
    }
    else{
      a = alfa2
      alfa2 = alfa1
      alfa1 = a*(1-tao) + b*tao
    }
  }
  
  res = c(alfa1, f(alfa1))
}

# Llamada a la función
res = dorada(0,2,1e-12)
res