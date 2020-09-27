demostrarmultiplicidad<-function(Fx,x){
 valor<-Fx(x) 
  cat("\nEl resultado de reemplazar ",x," en la ecuacion es: ",valor,"\n")
}

Newton = function(Fx,Gx,x0,tol,maxiter){
  listaErrorAnt = c()
  listaErrorAct = c()
  k = 0
  dx = 0
  cat(formatC( c("x"," f(x)","Error Ngen ", "Error New"), width = -14, format = "f", flag = " "), "\n")
  repeat{
    correccion = Fx(x0)/Gx(x0)
    x1 = x0 - correccion
    eAnterior = dx
    dx = abs(x1-x0)
    cat(formatC( c(x1 ,Fx(x1), dx,eAnterior), digits=10, width = -10, format = "f", flag = " "), "\n")
    x0 = x1
    k = k+1
    
    if(k>1){
      listaErrorAnt = c(listaErrorAnt , eAnterior)
      listaErrorAct = c(listaErrorAct, dx)
    }
    if(dx <= tol || k > maxiter ) break;}
  points(listaErrorAnt, listaErrorAct, col = "blue")
  lines(listaErrorAnt, listaErrorAct, col = "blue")
  if(k > maxiter){
    cat("Se alcanzó el máximo número de iteraciones.\n")
  }
}
Fx <- function(x){
  return (exp(1)^x - x - 1 )
}
Gx <- function(x){
  return (exp(1)^x - 1)
}
#Demostrar multiplicidad 2 cuando x=0
x<-0
demostrarmultiplicidad(Fx,x)
#-------------------------------------
plot.function(Fx, xlim=c(0,0.5), ylim=c(0,0.5),  main = "Error i+1 vs Error i",xlab = " Error i ",ylab = " Error i+1 ",col="white")
options(digits = 10)
Newton(Fx,Gx, 2, 1e-8, 30)

