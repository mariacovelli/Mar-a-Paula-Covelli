Fx<-function(x) log(x+2)
Gx<-function(x) sin(x)

plot(Fx, xlim = c(-3,0),ylim=c(-3,0), col="red", ylab = "y")
par(new = T)
plot(Gx, xlim = c(-3,0),ylim=c(-3,0), col="blue", main = "Gráficas", ylab = "y")

#metodo posicion falsa o regula false

f <- function(x) log(x+2)-sin(x)


PosicionFalsa= function(f, xi, xf, max, t)
{
  fi = f(xi)
  ff = f(xf)
  
  Ei = c() 
  Ej = c() 
  issues = c() 
  ite = c() 
  
  i = 1
  last = xf
  cat(formatC(c("i","x_i","f(x)","Error est."), width = -15, format = "f", flag = " "),"\n")
  
  while (f(xi) != 0 ) 
  {  
    x2 <-(xi*ff-xf*fi)/(ff-fi)
    f2 <- f(x2)
    if(abs(f2)<= t){
      break
    }
    ite <- c(ite, i)
    Error <-abs(xf - xi)
    issues <- c(issues, Error)
    if(sign(f2) == sign(fi)){
      xi <- x2
      last <- x2
      fi <- f2
    }
    else{
      xf <- x2
      last <- x2
      ff <- f2
    }
    cat(formatC( c(i,x2,f(x2),Error), digits = 12,width=-15, format = "f", flag = " "), "\n")
    i <- i+1
  }
  cat("Raíz de funcion: ",x2, " con error <=", round(abs(x2 - last),16), "Iteraciones: ", i)
  
  cat("\nFinalmente los puntos son X:",x2," y Y igual a:",Fx(x2))
  #Errores Ei vs Ei+1
  for(b in 1:i){
    if(b!=i){
      Ei[b]<-issues[b]
      Ej[b]<-issues[b+1]  
    }
  }
  plot(Ei,Ej, type = "b", xlab = "Error i",ylab="Error i+1")
}

PosicionFalsa(f,-1.5,-1,1000,1e-8)




