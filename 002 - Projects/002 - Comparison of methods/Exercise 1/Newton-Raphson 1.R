# --------------------------------------
# Newton-Raphson 1
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

NewtonRaphson = function(x, err){
  for(i in 1:10){
    print(i)
    points(x, f(x), pch=19, col=rgb(0,1,0,0.6), cex=1.5)
    xprev = x
    x = xprev - (f1(x)/f2(x))
    if( abs(x-xprev) < err ){
      break
    }
  }
  cat('X*=', x, 'f(x*)=', f(x), 'f1(x*)=', f1(x), 'f2(x*)=', f2(x))
  points(x, f(x), pch=19, col=rgb(1,0,0,0.6), cex=1.5)
}


NewtonRaphson(-4, 1e-8)

