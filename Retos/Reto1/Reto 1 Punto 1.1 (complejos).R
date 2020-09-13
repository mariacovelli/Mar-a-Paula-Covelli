#Punto 1 Evaluación de la primera y segunda derivada de un polinomio con una modificación de Horner 

#función inicial x^4-5x^3-9x^2+155x-250
#primera derivada 4x^3-15x^2-18x+155
#segunda derivada 12x^2-30x-18

horner <- function(coeficientes, x){#implementación del método de Horner, antes debemos conocer los coeficientes de la derivada
  eval <- coeficientes[1]
  i <-0
  
  for(j in coeficientes[2:length(coeficientes)]){
    eval <- x*eval + j
    i <- i + 2
  }
  return(cat("\nEl resultado de la evaluación en ",x," es: ",eval ,"\nEl numero de operaciones realizadas fue de: ", i))
}
porderivar <- function(coeficientes){#Esta función se encarga de encontrar los coeficientes de cada una de las variables  derivar, acompañadas por su respectivo exponente
  
  grado <- length(coeficientes)-1
  deriv <- c()
  for(i in coeficientes[1:length(coeficientes)-1]){
    aux <- i*grado
    deriv <- c(deriv, aux)
    grado <- grado - 1
  }
  return (deriv)#retorna un vector con los nuevos coeficientes (derivada)
}


x0 <- complex(real = 2, imaginary = 2)#Valor con el que se van a evaluar las derivadas

coeficientes <- c(1,-5,-9,155,-200)#del polinomio original

#1)primera derivada

derivada<-porderivar(coeficientes)

cat("\nPrimera derivada:")
cat ("\nLos coeficientes de la derivada son",derivada)
horner(derivada,x0)
cat("\n---------------------------------------------------------------")

#2)segunda derivada 

derivada<-derivar(derivada)

cat("\nSegunda derivada:")
cat ("\nLos coeficientes de la derivada son",derivada)
horner(derivada,x0)

