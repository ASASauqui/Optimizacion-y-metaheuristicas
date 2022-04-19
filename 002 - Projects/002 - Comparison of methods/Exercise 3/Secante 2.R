# --------------------------------------
# Secante 2
#
# Author : Alan Samuel Aguirre Salazar
# --------------------------------------



library(Deriv)

# grafica()

f = function(x){
  x^2
}

f1 = Deriv(f, nderiv = 1)
f2 = Deriv(f1,nderiv = 1)

grafica = function(){
  z = seq(-5,4, length=100)
  plot(z,f1(z), type="l",col=rgb(0,0,1,0.5),lwd=3, xlab="", ylab="", main='f(x) = x^2 + x^4')
}

grafica()

secante = function(x1, x2, err){
  a = 0
  for(i in 1:10){
    points(x1, f1(x1), pch=19, col=rgb(0,1,0, 0.6), cex=1.5)
    points(x2, f1(x2), pch=19, col=rgb(0.5,0.5,1, 0.6), cex=1.5)
    a = x2 - f1(x2)/((f1(x2)-f1(x1))/(x2-x1))
    lines(c(x1,a), c(f1(x1),0), col="red", lwd=1.5)
    lines(c(x2,a), c(f1(x2),0), col="red", lwd=1.5)
    segments(a,0,a,f1(a), col="black", lwd=1.5)
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
  points(a, f1(a), pch=19, col=rgb(1,0,0,1), cex=1.5)
}

secante(-5, 4, 1e-8)

