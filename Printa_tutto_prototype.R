print.from.file("Prova.R","","adult",c(paste0(0:9,"a"),paste0(0:9,"b")))

print.from.file<-function(filename,folder,name,avoid)
{
  browser()
  source(filename,local=TRUE)
  rm(filename)
  lista <- ls()
 
  print.mat = function(m, mat.name,mat.type=9,folder=folder) {
    if(mat.type==9)
    {
      squares <- paste0("Sq", 1:8)
      name<- c("11","12","13","21","22","23",
               "31","32")
    }else{
      squares <- paste0("Sq", c(1,2,4))
      name<- c("11","12","21")
    }
    
    for (i in 1:length(name)) {
      svg(paste0(getwd(), folder,
                 mat.name, "_", name[i], ".svg")
      )
      draw(m[[squares[i]]])
      dev.off()
    }
  }
  browser()
  lista<-lista[grepl(name,lista)]
  
  for(i in 1:lenght(avoid))
  {
    lista<-lista[!grepl(avoid[i],lista)]
  }
  ### Stampa la matrice
  for(i in 1:length(lista))
  {
    print.mat(get(lista[i]),lista[i],lenght(get(lista[i]))-2)
  }
}
