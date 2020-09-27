rm(list=ls())

cat("\nPasar base 10 a binarios\n")

parteDecimal = function(x)
{
  valor=0
  expo=1
  while(x>0)
  {
    digito = x%%2
    valor =  valor+expo*digito
    expo=expo*10
    x = x%/%2
  }
  return (valor)
}

parteEntera = function(x)
{
  valor=0
  expo=1
  while(x>0)
  {
    digito = x%%2
    valor =  valor+expo*digito
    expo=expo*10
    x = x%/%2
  }
  return (valor)
}

num1=11.25
num2=0.66666666
num3=30.6
num4=99.9

num1entero=trunc(11.25)
num2entero=trunc(0.666666666)
num3entero=trunc(30.60000)
num4entero=trunc(99.9)


cat("Primer Numero a Base 10: ", parteEntera(num1entero),".",parteDecimal((num1-num1entero)*100), "\n")
cat("Segundo Numero a Base 10: ", parteEntera(num2entero),".",parteDecimal((num2-num2entero)*100), "\n")
cat("Tercer Numero a Base 10: ", parteEntera(num3entero),".",parteDecimal((num3-num3entero)*10), "\n")
cat("Cuarto Numero a Base 10: ", parteEntera(num4entero),".",parteDecimal((num4-num4entero)*10), "\n")

