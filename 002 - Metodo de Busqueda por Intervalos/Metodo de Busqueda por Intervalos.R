# --------------------------------------
# Método de búsqueda por intervarlos
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------


# Función a evaluar
fobjetivo = function(x){
  x^2+x^3
}

# Curva inicialización
minimo = -6
maximo = 6

curve(fobjetivo, minimo, maximo, col="red", lwd=2)

# Inicialización
s = 1e-2  # Tamanno de paso
a = 3  # Valor inicial
k = 1   # Factor de expansión
b = a+s

paro = FALSE
aux = 0

points(a, fobjetivo(a), col='green', pch=19)
points(b, fobjetivo(b), col='blue', pch=19)
  
if(fobjetivo(b) > fobjetivo(a)){
  aux = a
  a = b
  b = aux
  s = -1*s
  curve(fobjetivo, minimo, maximo, col='red', lwd=2)
  points(a, fobjetivo(a), col='green', pch=19)
  points(b, fobjetivo(b), col='blue', pch=19)
}


while(paro != TRUE){
  c1 = b+s
  if(fobjetivo(c1) > fobjetivo(b)){
    if(a > c1){
      aux = a
      a = c1
      c1 = aux
      paro = TRUE
      
      print('Intervalo del mínimo local:')
      cat('a=', a, ' c=', c1, '\n')
      curve(fobjetivo, minimo, maximo, col='red', lwd=2)
      points(a, fobjetivo(a), col='green', pch=19)
      points(b, fobjetivo(c1), col='blue', pch=19)
    }
  }
  else{
    a = b
    b = c1
    s = s*k
    print('Intervalo del mínimo local:')
    cat('a=', a, ' b=', b, '\n')
    curve(fobjetivo, minimo, maximo, col='red', lwd=2)
    points(a, fobjetivo(a), col='green', pch=19)
    points(b, fobjetivo(b), col='blue', pch=19)
  }
}

cat(a,"-",c1)

