#Eficiencia de métodos directos (punto a)
matTriangular = function(tamano)
{
  suma = c()
  for (z in tamano) 
  {
    mat= matrix(1,nrow = z, ncol = z)
    cont = 0
    numop=0
    for (i in 1:z) 
    {
      for (j in 1:i) 
      {
        cont = cont + mat[i,j]
        numop=numop+1
        }   
    }
    #en una matriz triangular solo interesan los elementos de la diagonal hacia arriba
    suma = c(suma,cont)
    cont = 0
    cat("\nnúmero de operaciones: ",numop-1)
    }
  return(suma)
}
val = c(4,8,12,16,20,24,28,32,36,40)
#tasa covergencia:secuencia que se va acercando a su límite
graph = matTriangular(val)
plot(val,graph,main="Orden de convergencia",xlab = "Tamaño",ylab = "sumasdematriz", type = 'b')
