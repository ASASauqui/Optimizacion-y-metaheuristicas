# --------------------------------------
# Newton-Raphson 2
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
  z = seq(-4,1, length=100)
  plot(z,f1(z), type="l",col=rgb(0,0,1,0.5),lwd=3, xlab="", ylab="", main='f(x) = x^2 + x^4')
}

grafica()

NewtonRaphson = function(x, err){
  for(i in 1:10){
    points(x, f1(x), pch=19, col=rgb(0,1,0,0.6), cex=1.5)
    xprev = x
    x = xprev - (f1(x)/f2(x))
    lines(c(xprev,x), c(f1(xprev),0), col="red", lwd=1.5)
    segments(x,0,x,f1(x), col="black", lwd=1.5)
    if( abs(x-xprev) < err ){
      break
    }
  }
  cat('X*=', x, 'f(x*)=', f(x), 'f1(x*)=', f1(x), 'f2(x*)=', f2(x))
  points(x, f1(x), pch=19, col=rgb(1,0,0,0.6), cex=1.5)
}


NewtonRaphson(-4, 1e-8)

