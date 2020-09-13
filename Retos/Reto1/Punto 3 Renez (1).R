remez <- function(infe,supe,grado,error){
  
  f <- function (x) exp(sin(x)-cos(x^2))
  #para un grado n se necesita n+1 coeficientes 
  longitud = (2*(2))/(grado+1)
  x = c()
  y = c()
  valor_punto = 0
  absoluto = 0
  relative = 0
  i =1
  while (length(x)<grado)
  {
    if (length(x) == 0 )
    {
      x[i]= infe
      i = i +1
    }
    x[i] = (x[i-1]+(longitud))
    print (x[i])
    i = i+1
    
  }
  j = 1
  while (j < grado+1 )
    
  {
    y[j]= f(x[j])
    print (y[j])
    j = j+1
  }
  y[j] = cos(supe*(1/2))
  
  if(grado == 1){
    n = rbind(c(1,x[1]),
              c(0,1))
  }
  if(grado == 2){
    n = rbind(c(1,x[1],(x[1])^2),
              c(1,x[2],(x[2])^2),
              c(0,1,2*(1))
    )
  }
  if(grado == 3){
    n = rbind(c(1,x[1],(x[1])^2,(x[1])^3),
              c(1,x[2],(x[2])^2,(x[2])^3),
              c(1,x[3],(x[3])^2,(x[3])^3),
              c(0,1,2*((2^-8)/2))
    )
  }
  if(grado == 4){
    n = rbind(c(1,x[1],(x[1])^2,(x[1])^3,(x[1])^4),
              c(1,x[2],(x[2])^2,(x[2])^3,(x[2])^4),
              c(1,x[3],(x[3])^2,(x[3])^3,(x[3])^4),
              c(1,x[4],(x[4])^2,(x[4])^3,(x[4])^4),
              c(0,1,2*(1),3*(1)^2,4*(1))) 
  }
  
  z=solve(n,y)
  
  if(grado == 1){
    h = function(x) z[1]+(z[2]*x)
  }
  if(grado == 2){
    h = function(x) z[1]+(z[2]*x)+(z[3]*x^2)
  }
  if(grado == 3){
    h = function(x) z[1]+(z[2]*x)+(z[3]*x^2)+(z[4]*x^3)
  }
  if(grado == 4){
    h = function(x) z[1]+(z[2]*x)+(z[3]*x^2)+(z[4]*x^3)+(z[5]*x^4)
  }
  
  plot(f, xlim = c(-10,10),ylim=c(-10,10), col="red", ylab = "y")
  par(new = T)
  plot(h, xlim = c(-2,2),ylim=c(-2,2), col="blue", main = "Remez vs. ecuación", ylab = "y")
  
  plot(f, xlim = c(-2^-8,2^-8),ylim=c(-2,2), col="red", ylab = "y")
  par(new=T)
  plot(h, xlim = c(-2^-8,2^-8),ylim=c(-2,2), col="blue", main = "Remez vs. ecuación", ylab = "y")
 
   valor_punto = -2
  absoluto = abs((f(valor_punto)- h(valor_punto)))*error
  relative  = ((absoluto / h(valor_punto))*100)*error
  
  return(cat("Punto:", valor_punto, "\n ", "\nError absoluto",absoluto,"\nError relativo:", relative, "\n"))
  
}

remez (-2^-8,2^-8,4,2^-90)




