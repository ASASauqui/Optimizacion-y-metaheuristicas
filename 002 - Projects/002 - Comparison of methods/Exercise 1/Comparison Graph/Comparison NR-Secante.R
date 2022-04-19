# --------------------------------------
# Comparison NR-Secante
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library(Deriv)

f = function(x){
  x^2 + x^4
}

puntos1 = c()
puntos2 = c()

f1 = Deriv(f, nderiv = 1)
f2 = Deriv(f1,nderiv = 1)

NewtonRaphson = function(x, err){
  for(i in 1:10){
    xprev = x
    x = xprev - (f1(x)/f2(x))
    puntos1 <- c(puntos1,f(x))
    if( abs(x-xprev) < err ){
      break
    }
  }
  puntos1
}

secante = function(x1, x2, err){
  a = 0
  for(i in 1:10){
    a = x2 - f1(x2)/((f1(x2)-f1(x1))/(x2-x1))
    if(f1(a) > 0){
      x2 = a
    }
    else{
      x1 = a 
    }
    puntos2 <- c(puntos2,f(a))
    if(abs(f1(a)) < err){
      break
    }
  }
  puntos2
}


puntosa = NewtonRaphson(-4, 1e-8)
puntosb = secante(-4, -3, 1e-8)
puntosa
puntosb

grafica = function(){
  plot(1, type = "n",
       xlab = "", ylab = "",
       xlim = c(0, 10), ylim = c(0, 70))
  legend("topleft",
         c("Newton-Raphson","Secante"),
         fill=c("red","blue"))
  for(i in 2:10){
    lines(c(i-1,i), c(puntosa[i-1],puntosa[i]), col="red", lwd=1.5)
    lines(c(i-1,i), c(puntosb[i-1],puntosb[i]), col="blue", lwd=1.5)
  }
}

grafica()
