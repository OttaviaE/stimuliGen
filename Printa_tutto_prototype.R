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

##Aggiungere alla matrice
print.from.file<-function(filename,folder,name,avoid,delete,mat.type)
{
  source(filename,local=TRUE)
  #rm(filename)
  lista <- ls()
  if(!any("name"==lista))
  {
    stop("Idiot, check that there is not rm() in filename!")
  }
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
      print.mat(get(lista[i]),mat.name=lista[i],mat.type=mat.type[i],folder=folder)
    }else{
      if(grepl(delete,lista[i]))
      {
        element<-unlist(strsplit(lista[i],"_"))
        new_name<-paste0(element[element!=delete],collapse = "")
      }
      print.dist(get(lista[i]),mat.name=new_name,mat.type=mat.type[i-length(mat.type)],folder=folder)
    }
    
  }
}

mat.type<-rep(9,52)
mat.type[1:3]<-4
print.from.file(filename="MatriciAdult.R",folder="/Adults/",name="adult",
                avoid=c("dist",paste0(0:9,"a"),paste0(0:9,"b"),paste0(0:9,"c"),paste0(0:9,"d")),
                delete = "resp",mat.type)

print.from.file(filename="pratica.R",folder="/",name="pratica",
                avoid=c("dist",paste0(0:9,"a"),paste0(0:9,"b"),paste0(0:9,"c"),paste0(0:9,"d")),
                delete = "resp",mat.type)
