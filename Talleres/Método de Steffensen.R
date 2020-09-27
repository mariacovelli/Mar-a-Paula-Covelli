#Método Steffensen
f = function(x){
  return (x^2-cos(x))
}

Gx = function(x) return (sqrt(cos(x)))
aux =0
puntoFijo = function(a,b,i){
  if((Gx(a)-a)*(Gx(b)-b) < 0){
    x=(a+b)/2
    iteraciones = 0
    aux = 0
    dx = 0
    tol = 1e-8
    while (Gx(x) != x & iteraciones < i) {
      dx=abs(a-b)/2
      if(dx > tol){
        if (Gx(x) < x){
          b = x
        }
        else {a = x}
      }
      else {
        break
        }  
      x=(a+b)/2
      aux = c(aux,x)
      iteraciones = iteraciones+1
    }
    vectorAux <<- aux
    return(x)
  }
  else{
    cat("No tiene raiz la funcion en ese intervalo\n")
  }
}
aitken = function(x,x1,x2){
  resultado = x2 - (((x2 - x1)^2)/(x2 -2*x1+x)) 
  return(resultado)
}

i =18
iteracion = 3
puntoFijo(0,1,i)

resultadosAitken<-c()
iteraciones<-c()
while(iteracion < i ){
  cat(iteracion,"", aitken(vectorAux[iteracion-2],vectorAux[iteracion-1],vectorAux[iteracion]),"\n")
  iteracion = iteracion +1
  }
cat("Resultado sin aceleracion: ", puntoFijo(0,1,i), "\n")
cat("Resultado con aceleracion: ", aitken(vectorAux[i-3],vectorAux[i-2],vectorAux[i-1]), "\n")
cat("Valor de la solución de Wolfram: 0.82413","\n")

