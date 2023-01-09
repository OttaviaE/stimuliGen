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

print.dist = function(resp.list, mat.name,mat.type=9,folder=folder) {
  for (i in 1:length(resp.list)) {
    svg(paste0(getwd(), folder,
               mat.name, "_", names(resp.list)[i], ".svg")
    )
    draw(resp.list[[i]])
    dev.off()
  }
}


select.dist = function(dist.list, selection) {
  resp = list()
  for (i in 1:length(selection)) {
    resp[[i]] = dist.list[[selection[i]]]
    names(resp)[[i]] = selection[i]
  }
  return(resp)
}


print.from.file<-function(filename,folder,name,avoid,delete)
{
  source(filename,local=TRUE)
  #rm(filename)
  lista <- ls()
 
  lista<-lista[grepl(name,lista)]
  
  for(i in 1:length(avoid))
  {
    lista<-lista[!grepl(avoid[i],lista)]
  }
  ### Stampa la matrice
  for(i in 1:length(lista))
  {
    if(class(get(lista[i]))=="Raven_matrix")
       {
      print.mat(get(lista[i]),mat.name=lista[i],mat.type=length(get(lista[i]))-2,folder=folder)
    }else{
      if(grepl(delete,lista[i]))
      {
        element<-unlist(strsplit(lista[i],"_"))
        new_name<-paste0(element[element!=delete],collapse = "")
      }
      print.dist(get(lista[i]),mat.name=new_name,mat.type=length(get(lista[i]))-2,folder=folder)
    }
    
  }
}


print.from.file("Prova.R","/","adult",c(paste0(0:9,"a"),paste0(0:9,"b")),"resp")