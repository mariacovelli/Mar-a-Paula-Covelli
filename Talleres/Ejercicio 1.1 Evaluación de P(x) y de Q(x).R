rm(list=ls())
library(Rmpfr)

horner <- function(polinomio, x){#implementación del método de Horner, antes debemos conocer los coeficientes del polinomio a evluar
  eval <- polinomio[1]
  i <-0
  
  for(j in polinomio[2:length(polinomio)]){
    eval <- x*eval + j
    i <- i + 2
  }
  return (eval)
}

porderivar <- function(polinomio){#Esta función se encarga de encontrar los coeficientes de cada una de las variables a  derivar, acompañadas por su respectivo exponente
  
  grado <- length(polinomio)-1
  deriv <- c()
  for(i in polinomio[1:length(polinomio)-1]){
    aux <- i*grado
    deriv <- c(deriv, aux)
    grado <- grado - 1
  }
  return (deriv)#retorna un vector con los nuevos coeficientes (derivada)
}


polinomio = c()

for (i in 1:50)#permite crear el polinomio x+x^2+...+x^50 
{
  polinomio = c(polinomio,1)
}


options(digits=15)
Q = ((1.00000000001^51)-1)/(1.00000000001-1)
P = 1+horner(polinomio,1.00000000001)#horner solo nos ayuda a evaluar el polinomio sin la suma de 1 inicial
derivada<-porderivar(polinomio)
DP=horner(derivada,1.00000000001)

cat("\nEl valor de P(x) es de: ",P)
cat("\nEl valor de la derivada de P es: ",DP)
cat("\nEl valor de Q(x) es de: ",Q)
cat("\nEl error absoluto generado entre P(X) y Q(x) Es: ",abs(P-Q),"\n")
cat("El error relativo generado entre P(x) y Q(x) Es: ",abs(P-Q)/Q*100,"%\n")

