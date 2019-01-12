#5.-
#función para generar RUT válidos
dv <- function(rut){
  rut = as.character(rut)
  x = as.numeric(rev(strsplit(rut,NULL)[[1]]))
  Multiplo = rep(2:7,length.out=length(x))
  y = sum(x*Multiplo)
  z = 11 - y + floor(y/11)*11
  key = c(1:11)
  val = c(1:9,"k",0)
  dv = val[match(z, key)]
  return(dv)
}

#test
dv(19296595)

#lista para ejecutar 5.000 Ruts válidos
Rut_creados<-list()
  t<-proc.time ()
    for ( i  in sample ( 0000000 : 9999999 ,5000 ,replace = F )) {
      rut <- print (paste ( i , " - " , dv ( i )))
        Ruts_validos <- c ( Rut_creados , rut )

}
#tiempo en recorre la lista de RUT realizados
proc.time () -  t
#test
#user   system   elapsed 
1.270   0.120   4.104 


#6.-

lista<-" porque la llama que llama estando en llamas me llama, alguien más llama "
splitLista<-strsplit(lista, " ")[[1]]
splitEspacioLista<-tolower(splitLista)
unlistLista<-unlist(splitEspacioNoticia)
tablapalabras<-table(unlistLista)
dfPalabrasNoticia<-as.data.frame(tablapalabras)

#8.-
list()
contarSaldoNegativo<-function(saldoCuenta){
  if(saldoCuenta<0){
    +1
  }else{
    0 
  }
}
