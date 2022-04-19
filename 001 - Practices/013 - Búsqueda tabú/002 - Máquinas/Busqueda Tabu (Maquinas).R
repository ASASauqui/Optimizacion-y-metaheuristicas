# --------------------------------------
# Búsqueda Tabú - Máquinas
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



# Matrices
tiempoProcesamiento = c(c(15, 12, -1), 
                        c(7,  20, 10),
                        c(-1, 13, 20),
                        c(15, -1, 30),
                        c(25, 13, 14),
                        c(15, 20, -1))

tiempoAjuste = c(c(0,  5, 15, 10,  5, 35),
                 c(0,  0,  5, 15, 10, 45),
                 c(0,  0,  0, 10, 20, 15),
                 c(0,  0,  0,  0,  7, 24),
                 c(0,  0,  0,  0,  0, 13),
                 c(0,  0,  0,  0,  0,  0))

tiempoProcesamiento = matrix(tiempoProcesamiento, ncol = 3, byrow = TRUE)
tiempoAjuste = matrix(tiempoAjuste, ncol = 6, byrow = TRUE)




# ---------------- Funciones ----------------
# Defino la funcion a optimizar
f = function(x){
  res = c(0,0,0)
  for(i in 1:6){
    if(tiempoProcesamiento[i, x[i]] != -1){
      res[x[i]] = res[x[i]] + tiempoProcesamiento[i, x[i]]
    }
  }
  
  for(i in 1:3){
    for(j in 1:6){
      if(x[j] == i){
        prev = j
        break
      }
    }
    for(j in 1:6){
      if(x[j] == i){
        res[i] = res[i] + tiempoAjuste[prev, j]
        prev = j
      }
    }
  }
  
  return(res)
}


g = function(x){
  itsValid = TRUE
  
  for(i in 1:6){
    if(tiempoProcesamiento[i, x[i]] == -1){
      itsValid = FALSE
    }
  }
  
  return(itsValid)
}




# ---------------- Variables ----------------
N = 6
NoMaquinas = 3

k = 0
tabu = list()
tabuActive = 0
tabuNumber = 3

repeat{
  x = sample(1:3, N, replace = T) # (1,2,1,3,2,1)
  if(g(x)){
    break
  }
}
xParcial = c()
xHat = sample(1, N, replace = TRUE) 
xBest = sample(1, N, replace = TRUE) 
xHat
f(xBest)
it = 0
checkTabu = TRUE






# ---------------- Algoritmo ----------------
while(k < 30){
  k = k+1
  
  # Vacia la lista Tabu en otra para ser impresa
  a = c()
  if(length(tabu) != 0){
    for(i in length(tabu):1){
      a = c(a, tabu[[i]])
    }
  }
  
  
  xHat = sample(1, N, replace = TRUE) 
  
  
  for(i in 1:N){
    checkTabu = TRUE
    if(length(tabu) != 0){
      for(j in 1:length(tabu)){
        if(tabu[[j]] == i){
          checkTabu = FALSE
        }
      }
    }
    
    if(checkTabu == TRUE){
      for(j in 1:NoMaquinas){
        
        if(x[i] != j){
        
          xParcial = x
          xParcial[i] = j
          
          if(max(f(xParcial)) < max(f(xHat))){
            if(g(xParcial) == TRUE){
              xHat = xParcial
              it = i
            }
          }
        }
        
      }
    }
  }
  
  cat('K=', k,'x=', x, '\n')
  cat('f(x)=', f(x), '   Tabu=', a, '   Move=', it, '\n\n')
  
  tabu = c(tabu, a=list(it))
  if(k > 1){
    tabuActive = tabuActive + 1
  }
  
  
  if(max(f(xHat)) < max(f(xBest))){
    xBest = xHat
  }
  
  x = xHat
  
  if(tabuActive == tabuNumber){
    tabu = tabu[-1]
    tabuActive = tabuNumber-1
  }
  
}

# Impresión de resultados
cat('Soluciones para cada máquina = ',f(xBest), '\n')
print(g(xBest))
cat('Máquinas que deben ir en cada operación = ', xBest)



