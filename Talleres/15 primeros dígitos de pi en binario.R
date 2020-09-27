
cat("15 primeros bits de pi en número binario")
decimalesPI= pi - 3

cat("11")#para unicamente tener que obtener los valores después de la coma
cat(".")
for (i in 1:13)#ya que tenemos dos digitos de pi por ende el ciclo solo tiene que ir hasta 13
{
  decimalesPI = decimalesPI*2;#binario es base 2
  
  if(decimalesPI >= 1)
  {
    cat("1")
    decimalesPI = decimalesPI - 1;
  }
  else
  {
    cat("0")
  }
}