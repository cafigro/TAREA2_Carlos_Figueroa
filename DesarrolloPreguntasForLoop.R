##Ejercicio 1: Las mp tienen los nombres de las personas a las cuales est�n asociadas,
##realice una funci�n que cuente cu�ntos ni�os hay por cada una, y entregue una estad�stica
##de cu�ntos ni�os hay por mp.


rm(list=ls())
listaDocumentos <- list(c("mp","Juan","Christofer"),c("of","av01","ampr"),c("of","av01","ante"),
                        c("of","av08","arme"),c("of","av02","ante"),c("of","av07","ampr"),
                        c("of","av03","dape"),c("of","av01","meca"),c("of","av02","dape"),
                        c("mp","Antonia"),c("mp","Christian","Mario"),
                        c("mp","Jose","Pedro","Antonela"),c("of","av05","meca"),
                        c("of","av04","dape"),c("of","av02","arme"))
Ejercicio1<-function(listaDocumentos){
mp1<-0
mp2<-0
mp3<-0
mp4<-0
ni�os1<-0
ni�os2<-0
ni�os3<-0
for (i in 1:length(listaDocumentos)){
  if(listaDocumentos[[i]][1]=="mp"){
    if(length(listaDocumentos[[i]])==2){
      mp1<-mp1+1
      ni�os1<-ni�os1+length(listaDocumentos[[i]])-1
    }else if(length(listaDocumentos[[i]])==3){
      mp2<-mp2+1
      ni�os2<-length(listaDocumentos[[i]])-1
    }else if(length(listaDocumentos[[i]])==4){
      mp3<-mp3+1
      ni�os3<-length(listaDocumentos[[i]])-1
    }else{
   mp4<-mp4+1
    
    }}}
Aux<-c(mp1,ni�os1,mp2,ni�os2,mp3,ni�os3,mp4)
return(Aux)                        }
ni�os<-Ejercicio1(listaDocumentos)
    
print(paste("Se cuentan con",ni�os[1],"mp de", ni�os[2],"ni�os" ))
print(paste("Se cuentan con",ni�os[3],"mp de", ni�os[4],"ni�os" ))
print(paste("Se cuentan con",ni�os[5],"mp de", ni�os[6],"ni�os" ))
print(paste("Se cuentan con",ni�os[7],"mp de m�s de", ni�os[6],"ni�os" ))

##Ejercicio 2:  Los oficios est�n compuestos por el c�digo al cual pertenecen, construya
##una funci�n que almacene los c�digos y las tem�ticas a las que est�n asociadas.
##listaDocumentos se ejecuta nuevamente  en caso que el codigo se copie en otra ventana, este funcione.

listaDocumentos <- list(c("mp","Juan","Christofer"),c("of","av01","ampr"),c("of","av01","ante"),
                        c("of","av08","arme"),c("of","av02","ante"),c("of","av07","ampr"),
                        c("of","av03","dape"),c("of","av01","meca"),c("of","av02","dape"),
                        c("mp","Antonia"),c("mp","Christian","Mario"),
                        c("mp","Jose","Pedro","Antonela"),c("of","av05","meca"),
                        c("of","av04","dape"),c("of","av02","arme"))

Ejercicio2<-function(listaDocumentos){
  vectorav01<-c("av01")
  vectorav02<-c("av02")
  vectorav03<-c("av03")
  vectorav04<-c("av04")
  vectorav05<-c("av05")
  vectorav07<-c("av07")
  vectorav08<-c("av08")
  for (i in 1:length(listaDocumentos)){
    if(listaDocumentos[[i]][1]=="of"){
      if(listaDocumentos[[i]][2]=="av01"){
        vectorav01<-c(vectorav01,listaDocumentos[[i]][3])
      }else if(listaDocumentos[[i]][2]=="av02"){
        vectorav02<-c(vectorav02,listaDocumentos[[i]][3])
      }else if(listaDocumentos[[i]][2]=="av03"){
        vectorav03<-c(vectorav03,listaDocumentos[[i]][3])
      }else if(listaDocumentos[[i]][2]=="av04"){
        vectorav04<-c(vectorav04,listaDocumentos[[i]][3])
      }else if(listaDocumentos[[i]][2]=="av05"){
        vectorav05<-c(vectorav05,listaDocumentos[[i]][3])
      }else if(listaDocumentos[[i]][2]=="av07"){
        vectorav07<-c(vectorav07,listaDocumentos[[i]][3])
      }else  if(listaDocumentos[[i]][2]=="av08"){
        vectorav08<-c(vectorav08,listaDocumentos[[i]][3])
      }}else{
        NULL}}
  listaOf<-list(vectorav01,vectorav02,vectorav03,vectorav04,vectorav05,vectorav07,vectorav08)
  return(listaOf)}

Respuesta2<-Ejercicio2(listaDocumentos)
print(Respuesta2)

##Ejercicio 3(2 ptos): Construya una funci�n que act�e de juez y retorne aprobada o reprobada
##para los diferentes oficios, y entregue la cantidad que hay de cada una
##listaDocumentos se ejecuta nuevamente  en caso que el codigo se copie en otra ventana, este funcione.

listaDocumentos <- list(c("mp","Juan","Christofer"),c("of","av01","ampr"),c("of","av01","ante"),
                        c("of","av08","arme"),c("of","av02","ante"),c("of","av07","ampr"),
                        c("of","av03","dape"),c("of","av01","meca"),c("of","av02","dape"),
                        c("mp","Antonia"),c("mp","Christian","Mario"),
                        c("mp","Jose","Pedro","Antonela"),c("of","av05","meca"),
                        c("of","av04","dape"),c("of","av02","arme"))

Ejercicio3<-function(listaDocumentos){
  aprobado<-0
  reprobado<-0
  of<-0
  for (i in 1:length(listaDocumentos)){
    if(listaDocumentos[[i]][1]=="of"){
      juez<-sample(1:10,1)
      of<-of+1
      if (juez%%2==0){
        aprobado<-aprobado+1
      }else{
        reprobado<-reprobado+1
      }
    }
    Info<-c(of,aprobado,reprobado)
  }
  
  return (Info)
}
Juez<-Ejercicio3(listaDocumentos)
print(paste("Llegaron",Juez[1],"oficios, de los cuales",Juez[2],"son aprobados y",Juez[3],"son reprobados"))
       