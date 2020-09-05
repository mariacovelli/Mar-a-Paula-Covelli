#metodo posicion falsa o regula false

f <- function(x) cos(2*x)^2-x^2;


PosicionFalsa= function(f, xi, xf, max, t)
{
  fi = f(xi)
  ff = f(xf)
  
  Ei = c() 
  Ej = c() 
  issues = c() 
  ite = c() 
  
  x<-seq(xi,xf,0.01)
  plot(x,f(x),type="l",col="red")
  abline(h=0,col="blue")
  
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
  #Errores Ei vs Ei+1
  for(b in 1:i){
    if(b!=i){
      Ei[b]<-issues[b]
      Ej[b]<-issues[b+1]  
    }
  }
  plot(Ei,Ej, type = "b", xlab = "Error i",ylab="Error i+1")
}

PosicionFalsa(f,0,1,1000,1e-8)

