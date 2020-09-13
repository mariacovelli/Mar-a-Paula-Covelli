library(pracma)

f <- function(x){ 
  return (exp(sin(x)-cos(x^2)))
}

evaluar <- function(x, p){
  grado <- length(p)-1
  tay <- 0
  cont <- 0
  for(i in p){
    tay <-  tay + i*(x^grado)
    grado <- grado - 1
    cont <- cont + 2
  }
  cat("\n-------------------------------------")
  return (cat("\nEvaluado en Taylor:", tay, "\n", 
              "Evaluado en la ecuación:", exp(sin(x)-cos(x^2)),"\n", 
              "Num de operaciones:", cont, "\n",
              "Error relativo:", tay-exp(sin(x)-cos(x^2)), "\n"))
  
}

p <- taylor(f, 1, 3)
particion <- (2^-8-(-2^-8))/10 #10 punto o intervalos
puntos <- c()

for(i in 1:10){
  puntos <- c(puntos, x)
  x <- x + particion
}

for(i in puntos){
  cat(evaluar(i, p))
}