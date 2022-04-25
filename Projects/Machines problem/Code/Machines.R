# --------------------------------------
# Problema de las máquinas
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library("xlsx")

# Lectura de datos
    ubicacion = "D:\\Universidad-Ingeniería en Inteligencia Artificial\\Semestre 5\\Optimización y Metaheurísticas I\\Bloque II\\Proyecto\\Instancia1.sjupm"
    datos =  scan(ubicacion, what = numeric())
    
    # Datos
        noOperaciones = datos[1]
        noMaquinas = datos[2]
      
    # Leer matriz de tiempos de procesamiento
        tiemposProcesamiento = read.table(file = ubicacion, skip=2)
        tiemposProcesamiento = as.matrix(tiemposProcesamiento)
        tiemposProcesamiento = matrix(tiemposProcesamiento, ncol = noOperaciones, byrow = FALSE)
        # tiemposProcesamiento
    
    # Leer matrices de tiempos de ajuste
        tiempoAjuste = read.table(file = ubicacion, skip=2+noMaquinas+1)
        n1 = length(tiempoAjuste[,1])
        n2= length(tiempoAjuste[1,])
        numeros = c()
        for(i in 1:n1){
          for(j in 1:n2){
            numeros = c(numeros, tiempoAjuste[i,j])
          }
        }
        tiempoAjuste = array(numeros, dim=c(noOperaciones, noOperaciones, noMaquinas))
        for(i in 1:noMaquinas){
          tiempoAjuste[ , , i] = matrix(tiempoAjuste[ , , i], ncol = noOperaciones, byrow = FALSE)
        }
        for(i in 1:noMaquinas){
          tiempoAjuste[ , , i] = t(tiempoAjuste[,,i])
        }


# Defino la funcion a optimizar
    fobjetivo = function(x){
        # Rellenar vector resultados 
            res = c()
            for(i in 1:noMaquinas){
              res = c(res,0)
            }
            
            
        # Cálculo de suma de tiempos de procesamiento por máquina
            for(i in 1:noOperaciones){
              res[x[1,i]] = res[x[1,i]] + tiemposProcesamiento[x[1,i], x[2,i]]
            }
        
            
        # Cálculo de suma de tiempos de ajuste por cada máquina
            for(i in 1:noMaquinas){
                    
                # Toma la primera ocurrencia que tenga el número de máquina i
                    for(j in 1:noOperaciones){
                      if(x[1,j] == i){
                        prev = x[2,j]
                        break
                      }
                    }
                    
                # Suma de los tiempos de ajuste por cada máquina
                    for(j in 1:noOperaciones){
                      if(x[1,j] == i){
                        res[i] = res[i] + tiempoAjuste[prev, x[2,j], i]
                        prev = x[2,j]
                      }
                    }
            }

        return(res)
    }




    
    
    
#-------------------Repeticiones-------------------

# While general (comentar esto y lo de abajo si se ocupa una sola solución)
# Valores generales
    ciclos = 100
    mejorXGeneral = c()
    mejorFGeneral = 1e1000
    l = 0
    numeroBueno = 0
    numeroBuenoG = 0
    numeroBuenoMaximoG = 0
    
# Escribir en excel
    FResultadosTotalesGeneral = c()
    XMaquinaResultadosTotalesGeneral = c()
    XOperacionesResultadosTotalesGeneral = c()
    IDs = c()
    ciclos1 = ciclos + 1
    for(i in 1:ciclos1){
      IDs = c(IDs, i, i) 
    }
    
while(l < ciclos){
  l = l+1
  
# Paso 0 (inicialización)
  # x = c(1,1,1,2,2,2,2,3,3,3, 6,5,4,9,8,1,2,10,7,3)
    x = c(sample(1:noMaquinas, noOperaciones, replace = T), sample(1:noOperaciones, noOperaciones, replace = F))
    x = matrix(x, ncol = noOperaciones, byrow = T)
    fx = fobjetivo(x)
    k = 0
    t = 1000
    err = 1e-70
    alfa = 0.99

# Variables gráfica temperatura
    yTemperatura = c(t)
    
# Incumbente
    fmejor = 1e100
    xmejor = x

  while(k < 2000 && t > err){
    k = k + 1 
    
    # Paso 1 - Generar vector xhat
      dk = sample(1:noOperaciones, 2, replace = FALSE)
      xhat = x
      
      # Cambio de máquina
        if(k%%2 == 0){
          for(i in 1:length(dk)){
              repeat{
                random = sample(1:noMaquinas,1)
                if(xhat[1,dk[i]] != random){
                  xhat[1,dk[i]] = random
                  break
                }
              }
          }
        }
      # Swap de operación
        else{
          dk= matrix(dk, ncol = 2, byrow = TRUE)
          for(i in 1:length(dk[,1])){
            random = dk[i,]
            a = xhat[,random[1]]
            xhat[,random[1]] = xhat[,random[2]]
            xhat[,random[2]] = a
          }
        }
      
      fxhat = fobjetivo(xhat)
      
    #Paso 2
      if(max(fxhat) < max(fx)){
        x = xhat
        fx = fxhat
      }
      else{
        if(runif(1,0,1) < exp( (max(fx) - max(fxhat)) / t ) ){
          x = xhat
        }
        else{
          x = x
        }
      }
      
      if(max(fxhat) < max(fmejor)){
        # print('Entre')
        fmejor = fxhat
        xmejor = xhat
        numeroBueno = k
      }
    
    # Paso 3
      t = alfa*t
      yTemperatura = c(yTemperatura, t)
  }

# While general (comentar esto y lo de abajo si se ocupa una sola solución)
  if(max(fmejor) < max(mejorFGeneral)){
    mejorXGeneral = xmejor
    mejorFGeneral = fmejor
    numeroBuenoG = numeroBueno
    numeroBuenoMaximoG = l
  }
  xmejor[2, ] = xmejor[2, ] - 1
  cat("Solución x=", '\n', xmejor[1,], '\n', xmejor[2,], '\n', 'y f(x)=', fmejor, '\n')
  FResultadosTotalesGeneral = c(FResultadosTotalesGeneral,fmejor)
  XMaquinaResultadosTotalesGeneral = c(XMaquinaResultadosTotalesGeneral,c(xmejor[1, ],xmejor[2,]))
}

plot(yTemperatura, type="l",col="blue",lwd=3)

options(digits=8) # digitos para los decimales
# cat("Solución x=", '\n', xmejor[1,], '\n', xmejor[2,], '\n', 'y f(x)=', fmejor, '\n')



cat("Solución general x=", '\n', mejorXGeneral[1,], '\n', mejorXGeneral[2,], '\n', 'y f(x)=', mejorFGeneral, '\n')






# Plots

# Colores
require(colorRamps)
require(plotly)
my.cols <- matlab.like(10)
# 
# Gráfica vacía
fig <- plot_ly()


for(i in 1:noMaquinas){
  arreglo =  sample(-1, 40, replace = TRUE)
  arreglo = matrix(arreglo, ncol=20, byrow = T)
  prev = 0
  
  # Toma la primera ocurrencia que tenga el número de máquina i
    apuntador = 1
    for(j in 1:noOperaciones){
      if(mejorXGeneral[1,j] == i){
        arreglo[1,apuntador] = tiemposProcesamiento[mejorXGeneral[1,j], mejorXGeneral[2,j]]
        arreglo[2,apuntador] = mejorXGeneral[2,j]
        apuntador = apuntador + 2
      }
    }
  
  # Toma la primera ocurrencia que tenga el número de máquina i
    for(j in 1:noOperaciones){
      if(mejorXGeneral[1,j] == i){
        prev = mejorXGeneral[2,j]
        break
      }
    }
  
  # Suma de los tiempos de ajuste por cada máquina
    apuntador = 2
    primero = TRUE
    for(j in 1:noOperaciones){
      if(mejorXGeneral[1,j] == i && primero == FALSE){
        arreglo[1,apuntador] = tiempoAjuste[prev, mejorXGeneral[2,j], i]
        prev = mejorXGeneral[2,j]
        apuntador = apuntador + 2
      }
      if(mejorXGeneral[1,j] == i && primero == TRUE){
        primero = FALSE
      }
    }
    
    anterior = 0
    for(j in 1:length(arreglo[1,])){
      if(arreglo[1,j] == -1){
        break
      }
      
      if(j%%2 == 1){
        fig <- add_trace(fig,
                         x = c(anterior, anterior + arreglo[1,j]), # x0, x1
                         y = c(i, i),  # y0, y1
                         mode = "lines",
                         line = list(color = my.cols[2], width = 30),
                         showlegend = F,
                         hoverinfo = "text",
  
                         # Create custom hover text
                         text = paste("Operacion: ", arreglo[2,j]-1, "<br>",
                                      "Tiempo de operacion: ", arreglo[1,j]),
  
                         evaluate = T  # needed to avoid lazy loading
        )
        fig <- add_text(fig,
                        x=(arreglo[1,j]/2)+anterior,
                        y=i,
                        text=arreglo[2,j]-1)
      }
      else if(j%%2 == 0){
        fig <- add_trace(fig,
                         x = c(anterior, anterior + arreglo[1,j]), # x0, x1
                         y = c(i, i),  # y0, y1
                         mode = "lines",
                         line = list(color = my.cols[10], width = 15),
                         showlegend = F,
                         hoverinfo = "text",
                         
                         # Create custom hover text
                         text = paste("Tiempo de ajuste: ", arreglo[1,j], "<br>"),
                         
                         evaluate = T  # needed to avoid lazy loading
        )
      }
      anterior = anterior + arreglo[1,j]
    }
}

yText = c('Maquiná 1', 'Máquina 2', 'Máquina 3')
fig <- layout(fig,
              
              # Axis options:
              # 1. Remove gridlines
              # 2. Customize y-axis tick labels and show task names instead of numbers
              
              xaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6")),
              
              yaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6"),
                           tickmode = "array", tickvals = 1:3, ticktext = unique(yText),
                           domain = c(0, 0.9)),
              
              plot_bgcolor = "#333333",  # Chart area color
              paper_bgcolor = "#333333") # Axis area color


# Impresión de grafica
fig
mejorXGeneral[2, ] = mejorXGeneral[2, ] - 1
print(mejorXGeneral)
numeroBuenoG
numeroBuenoMaximoG
    
    


# Excel
  FResultadosTotalesGeneral = c(FResultadosTotalesGeneral, mejorFGeneral)
  XMaquinaResultadosTotalesGeneral = c(XMaquinaResultadosTotalesGeneral, mejorXGeneral[1,], mejorXGeneral[2,])

  FResultadosTotalesGeneral = matrix(FResultadosTotalesGeneral, ncol = 3, byrow = T)
  XMaquinaResultadosTotalesGeneral = matrix(XMaquinaResultadosTotalesGeneral, ncol = 10, byrow = T, dimnames=list(IDs))

  write.xlsx(FResultadosTotalesGeneral, file="Resultados.xlsx", sheetName = "FResultados", append = F)
  write.xlsx(XMaquinaResultadosTotalesGeneral, file="Resultados.xlsx", sheetName = "Maquinas", append = T)
  
  
  