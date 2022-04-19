# --------------------------------------
# Secante 1
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library(Deriv)

f = function(x){
  x^2 + x^4
}

f1 = Deriv(f, nderiv = 1)
f2 = Deriv(f1,nderiv = 1)

grafica = function(){
  z = seq(-4,4, length=100)
  plot(z,f(z), type="l",col="blue",lwd=3, xlab="", ylab="", main='f(x) = x^2 + x^4')
}

grafica()

secante = function(x1, x2, err){
  a = 0
  for(i in 1:10){
    print(i)
    points(x1, f(x1), pch=19, col=rgb(0,1,0, 0.6), cex=1.5)
    points(x2, f(x2), pch=19, col=rgb(0.5,0.5,1, 0.6), cex=1.5)
    a = x2 - f1(x2)/((f1(x2)-f1(x1))/(x2-x1))
    if(f1(a) > 0){
      x2 = a
    }
    else{
      x1 = a 
    }
    if(abs(f1(a)) < err){
      break
    }
  }
  
  cat('x*=', a, 'f(x*)=', f(a))
  points(a, f(a), pch=19, col=rgb(1,0,0,1), cex=1.5)
}

secante(-4, 3, 1e-8)

