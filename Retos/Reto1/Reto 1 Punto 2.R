 f <- function(x) x^3-2*x^2+4*x/3-8/27 #Ecuación
 a<-0 #Límite inferior
 b<-1 #Límite superior
 maxiter=50000 #cantidad máxima de iteraciones
 tol=1*10^-15  #toleracia a trabajar
 
  stopifnot(is.numeric(a), is.numeric(b), length(a) == 1, length(b) == 1)#nos permite detectar si hay algo incorrecto en los datos de entrada

  x1 <- a # límite inferior
  f1 <- f(x1) #Valor del polinomio evaluado en el límite inferior

  x2 <- b# límite inferior
  f2 <- f(x2) #Valor del polinomio evaluado en el límite superior
 
  if (f1 * f2 > 0) #condición para saber si hay raíz dentro de dicho intervalo
    stop("No hay ninguna raíz en los intervalos planteados [a, b].")
  #Si no hay raíz no se va a seguir con el codigo
  
  x3 <- 0.5 * (a + b) #Punto medio del intervalo
  niter <- 1
  
  while (niter<=maxiter) {
    f3 <- f(x3)#evaluación de la función en el punto medio
    
    if (abs(f3) < tol) { 
        x0 <- x3
        break
      }
    if (f1 * f3 < 0) 
        b <- x3
    else a <- x3
    
    if ((b - a) < tol * max(abs(b), 1)) {
      x0 <- 0.5 * (a + b)
      break
    }
    
    denominador <- (f2 - f1) * (f3 - f1) * (f2 - f3)
    numerador <- x3 * (f1 - f2) * (f2 - f3 + f1) + f2 * x1 * (f2 - f3) + f1 * x2 * (f3 - f1)
    if (denominador == 0) {
      dx <- b - a
    }
    
    else {
      dx <- f3 * numerador/denominador
    }
    
    x <- x3 + dx
    if ((b - x) * (x - a) < 0) {
      dx <- 0.5 * (b - a)
      x <- a + dx
    }
    
    if (x1 < x3) {
      x2 <- x3
      f2 <- f3
    }
    
    else {
      x1 <- x3
      f1 <- f3
    }
    
    niter <- niter + 1
    if (abs(x - x3) < tol) {
      x0 <- x
      break
    }
    x3 <- x
  }
  
  if (niter > maxiter) 
  cat("Se ha alcanzado el númeor máximo de iteraciones")
  prec <- min(abs(x1 - x3), abs(x2 - x3))


  cat("\nLos resultados obtenidos por el método de Brent son los siguientes:")
  
  cat("\nValor de la raíz",x0)
  cat("\nFuncion evaluada en la raíz",f(x0))
  cat("\nPrecisión obtenida",prec)
  cat("\nNúmero de iteraciones",niter)
  