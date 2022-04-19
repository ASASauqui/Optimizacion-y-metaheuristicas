# --------------------------------------
# Problema de la mochila
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



# Defino la función a optimizar
fobjetivo = function(x){
  sum(x)
}




#-------------------Repeticiones-------------------
      f = function(x, b){
        sum(x*b)
      }
      
      g = function(x, w){
        sum(x*w)
      }
      
# Lectura de datos
      datos =  scan("C:\\Users\\alans\\Desktop\\Códigos\\Optimización y metaheurísticas\\013 - Mochila\\Datasets\\ks_50_0", what = numeric())
      datos = matrix(datos, ncol=2, byrow=T)
      
      b = c()
      w = c()
      for(i in 2:(length(datos)/2) ){
        b = c(b, datos[i,1])
        w = c(w, datos[i,2])
      }
      wmax = datos[1,2]
  
      # wmax
      # print(b)
      # print(w)
  
      
      
  # Paso 0 (inicialización)
      k = 0
      t = 1000
      err = 1e-70
      alfa = 0.99
      areaFlag = 0
  
  # Mochila Variables
      x = sample(0:1, datos[1,1], replace = T)
      # b = c(4, 2, 10, 1, 2)
      # w = c(12, 1, 4, 1, 2)
      # wmax = 15
      fx = f(x, b)
      xhat = 0
      fxhat = 0
      
  # Variables gráfica temperatura
      yTemperatura = c(t)
  
  # Incumbente
      xmejor = c()
      fmejor = -10000000
  
  
  while(k < 4000 && t > err){
    k = k + 1 
    
    # Paso 1: Generar vector xhat y ver si cumple restricción
        dk = sample(1:datos[1,1], 3, replace = FALSE)
        xhat = x
        for(i in 1:length(dk)){
          xhat[dk[i]] = !xhat[dk[i]]
        }
        
        if(g(xhat,w) <= wmax){
          fxhat = f(xhat,b)
        }
        else{
          fxhat = f(xhat,b) - 10000
        }
    
    #Paso 2
        if(fxhat > fx){
          x = xhat
          fx = fxhat
        }
        else{
          if(runif(1,0,1) < exp((f(x,b)-fxhat)/t) ){
            x = xhat
            # cat(x, fx, '\n')
          }
          else{
            x = x
          }
        }
          
        if(fxhat > fmejor && g(xhat,w) <= wmax){
          fmejor = fxhat
          xmejor = xhat
        }
    
    # Paso 3
        t = alfa*t
        yTemperatura = c(yTemperatura, t)
  }
      
  # Impresión de resultados
    plot(yTemperatura, type="l",col="blue",lwd=3)
    options(digits=8) # Dígitos para los decimales
    cat("Solución x=", xmejor, "con valor f(x)=", fmejor, '\n')