rm(list=ls())

cat("\nPasar base 10 a binarios\n")

parteDecimal = function(x,cantidadDeBits)
{
  numero=""
  for (i in 1:cantidadDeBits)
  {
    x = x*2;
    
    
    if(x >= 1)
    {
      numero = paste(numero,"1",sep="")
      x = x - 1;
    }
    else
    {
      numero = paste(numero,"0",sep="")
    }
  }
  return(numero)
}

parteEntera = function(x)
{
  acomulado=0
  nexp=1
  while(x>0)
  {
    digito = x%%2
    acomulado =  acomulado+nexp*digito
    nexp=nexp*10
    x = x%/%2
  }
  return (acomulado)
}


num1=11.25
num2=0.66666666
num3=30.6
num4=99.9

num1entero=trunc(11.25)
num2entero=trunc(0.666666666)
num3entero=trunc(30.60000)
num4entero=trunc(99.9)


cat("Primer Numero a Base 10: ", parteEntera(num1entero),".",parteDecimal(num1-num1entero,10), "\n")
cat("Segundo Numero a Base 10: ", parteEntera(num2entero),".",parteDecimal(num2-num2entero,10), "\n")
cat("Tercer Numero a Base 10: ", parteEntera(num3entero),".",parteDecimal(num3-num3entero,10), "\n")
cat("Cuarto Numero a Base 10: ", parteEntera(num4entero),".",parteDecimal(num4-num4entero,10), "\n")


