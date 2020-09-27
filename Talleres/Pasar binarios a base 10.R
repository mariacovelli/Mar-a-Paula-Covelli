cat("Convertir números binarios a base 10\n")


parteEntera = function(b)#retorna la parte entera del numero binario en base 10
{
  contadorPotencias = 0
  digito = 0
  entero=0
  
  while(b > 0)
  {
    digito = b %% 10
    entero = entero + (digito*(2^contadorPotencias))
    contadorPotencias = contadorPotencias + 1
    b = b %/% 10
  }
  return(entero)
}

parteDecimal = function(b)#retorna la parte decimal del numero binario en base 10
{
  copiab = b
  contadorPotencias = 0
  digitos = 0
  digito = 0
  decimal=0
  
  while(b > 0)
  {
    digitos = digitos+1
    b = b %/% 10
  }
  contadorPotencias = -digitos;
  while(copiab > 0)
  {
    digito = copiab %% 10
    decimal = decimal + (digito*(2^contadorPotencias))
    contadorPotencias = contadorPotencias + 1
    copiab = copiab %/% 10
  }
  return(decimal)

}

cat("\nLos resultados obtenidos en base 10 son los siguientes: " )
cat("\nPrimer Numero: ",parteEntera(101010101),"\n")
cat("Segundo Numero: ",parteEntera(1011) + parteDecimal(101),"\n")
cat("Tercer Numero: ",parteEntera(10111) + parteDecimal(010101010101010101),"\n")
cat("Cuarto Numero: ",parteEntera(111) + parteDecimal(11111111111111))