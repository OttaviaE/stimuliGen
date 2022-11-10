draw.field<- function(obj, main = NULL) {
  library(DescTools)
#  par(mfrow = c(1, 1))
#  plot.new()

    Canvas(xlim=16,mar=c(1,1,1,1), main = main)
    for(j in 1:length(obj$shape))
    {
      if(obj$visible[[j]]==1)
      {
        if(obj$num[[j]][1]==1){
          DrawRegPolygon(x = obj$pos.x[[j]], y = obj$pos.y[[j]], rot = obj$rotation[[j]], 
                         radius.x = obj$size.x[[j]], radius.y = obj$size.y[[j]], nv = obj$nv[[j]],
                         lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]])
        }else{
          DrawCircle(x = obj$pos.x[[j]], y = obj$pos.y[[j]], 
                     r.out = obj$size.x[[j]],r.in= obj$size.y[[j]], theta.1=obj$theta.1[[j]],
                     theta.2=obj$theta.2[[j]], nv = obj$nv[[j]],
                     lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]])
          
        }
      }
    }
    
  }


replace <- function(obj,index,obj2) {
  UseMethod("replace")
}

replace.field<-function(obj,index,obj2)
{
  for(i in 1:length(obj))
  {
    obj[[i]][[index]]<-obj2[[i]][[1]]
  }
  return(obj)
}

hide<- function(obj,index) {
  UseMethod("hide")
}

hide.field<-function(obj,index="Full")
{
  if(any(index=="Full"))
  {
    index<-1:length(obj$shape)
  }
    obj$visible[index]<-integer(length(index))
  return(obj)
}


#Esempio di applicazione
#shapes_list("Shapes_list-07-11-Ottavia.R")

shapes_list<-function(filename)
{

  source(filename,local=TRUE)
  rm(filename)
  name <- ls()
  
  table<-data.frame(
    name=name,
    num_shapes=integer(length(name)),
    small=logical(length(name)),
    fill= logical(length(name)),
    rotate= logical(length(name))
  )
  for(r in 1:length(name))
  {
    f<-get(name[r])
    obj<-f()
    tags<-unlist(obj$tag)

    if(any(tolower(tags)=="simple"))
    {
        table$num_shapes[r] <- 1
    }else if(any(tolower(tags)=="compose2"))
    {
      table$num_shapes[r] <- 2
    }else if(any(tolower(tags)=="compose4"))
    {
      table$num_shapes[r] <- 4
    }
    table$fill[r]<-any(tolower(tags)=="fill")
    table$small[r]<-any(tolower(tags)=="small")
    table$rotate[r]<-any(tolower(tags)=="rotate")
  }
  return(table)
}

