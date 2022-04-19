# --------------------------------------
# Búsqueda Tabú
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



# Lectura de datos
  datos =  scan("C:\\Users\\alans\\Desktop\\Códigos\\Optimización y metaheurísticas\\015 - Búsqueda tabú\\Datasets\\ks_50_0", what = numeric())
  datos = matrix(datos, ncol=2, byrow=T)
  datos
  
  N = datos[1,1]
  Peso = datos[1,2]
  Beneficios = c()
  Pesos = c()
  
  
  for(i in 1:N){
    Beneficios = c(Beneficios, datos[i,1])
    Pesos = c(Pesos, datos[i,2])
  }

  
  
  

# ---------------- Funciones ----------------
f = function(x){
  sum(x*Beneficios)
}

g = function(x){
  sum(x*Pesos) <= Peso
}




# ---------------- Variables ----------------
k = 0
tabu = list()
tabuActive = 0
tabuNumber = 3
# x = c(1,0,0,1,1,1,0,1)
x = sample(0, N, replace = TRUE)
for(i in 1:N){
  random = runif(1,0,1);
  if(random <= 2/9){
    x[i] = 1
  }
}
# x = c(1,1,1,1,1,0,0,1,1,1,1,0,1,0,0,0,0,0,0)
xParcial = c()
xHat = sample(-10, N, replace = TRUE) 
xBest = sample(-10, N, replace = TRUE) 
it = 0
checkTabu = TRUE






# ---------------- Algoritmo ----------------
while(k < 30){
  k = k+1
  
  a = c()
  if(length(tabu) != 0){
    for(i in length(tabu):1){
      a = c(a, tabu[[i]])
    }
  }
  
  
  xHat = sample(0, N, replace = TRUE) 
  
  
  for(i in 1:N){
    xParcial = x
    checkTabu = TRUE
    
    if(length(tabu) != 0){
      for(j in 1:length(tabu)){
        if(tabu[[j]] == i){
          checkTabu = FALSE
        }
      }
    }
    
    if(checkTabu == TRUE){
      xParcial[i] = !xParcial[i]
      
      if(f(xParcial) > f(xHat)){
        if(g(xParcial)){
          xHat = xParcial
          it = i
        }
      }
    }
  }
  
  cat('K=', k,'x=', x, '\n')
  cat('f(x)=', f(x), '   Tabu=', a, '   Move=', it, '\n')
  
  tabu = c(tabu, a=list(it))
  if(k > 1){
    tabuActive = tabuActive + 1
  }
  
  
  if(f(xHat) > f(xBest)){
    xBest = xHat
  }
  
  x = xHat
  
  if(tabuActive == tabuNumber){
    tabu = tabu[-1]
    tabuActive = tabuNumber-1
  }
  
}


cat('óptimo = ', f(xBest), '\n')
print(g(xBest))
cat('Arreglo = ', xBest)
