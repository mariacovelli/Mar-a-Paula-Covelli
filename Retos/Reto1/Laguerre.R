p<-function (x) x^4-5*x^3-9*x^2+155*x-250
x0<-1

function (p, x0, nmax = 25, tol = .Machine$double.eps^(1/2)) 
{

  n <- length(p) - 1
  p1 <- polyder(p)#ecuación derivada 1
  p2 <- polyder(p1)#ecuación derivada 2
  y0 <- polyval(p, x0)

  if (abs(y0) < tol) 
    return(x0)
  for (m in 1:nmax) {
    a <- polyval(p1, x0)/y0
    a2 <- a^2
    b <- a2 - polyval(p2, x0)/y0
    x <- x0 - n/(a + a/abs(a) * sqrt((n - 1) * (n * b - a2)))
    y <- polyval(p, x)
    if (abs(y) < tol) 
      break
  }
  return(x)
}


cat("\nx0:",x0)
cat("\np(x0)",p(x0))#valor del polinomio sin derivar


