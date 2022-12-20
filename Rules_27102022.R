##Rules methods 
rotation <- function(obj,n,...) {
  UseMethod("rotation")
}

size <- function(obj,n,...) {
  UseMethod("size")
}

diff_shapes <- function(obj,n,...) {
  UseMethod("diff_shapes")
}

margin <- function(obj,n,rule,...) {
  UseMethod("margin")
}  

logic <- function(obj,n,rule,seed,...) {
  UseMethod("logic")
}

movement <- function(obj,n,rule,...) {
  UseMethod("movement")
}

identity <- function(obj,...) {
  UseMethod("identity")
}


fill <- function(obj,n,...) {
  UseMethod("fill")
}

numeric_progression <- function(obj,rules,n,...) {
  UseMethod("numeric_progression")
}

mental_transformation <- function(obj,n,rule,...) {
  UseMethod("mental_transformation")
}

create_dice <- function(obj,...) {
  UseMethod("create_dice")
}


##Rules

fill.field<-function(obj,n,rule,...){
  if(grepl("par",rule))
  {
    index<-c("line1h","line2h","line12h","line1","line2","line12",
             "line1inv","line2inv","line12inv")
  }else if(grepl("line",rule)){
    index <- rep(c("line12","line12h","line12inv"),3)
  }else{
    index <- rep(c("white","grey","black"),3)
  }
  
  if(grepl("multi",rule))
  {
    set.seed(n)
    new<-Map("c",obj$shade,sample(1:length(obj$shape),length(obj$shape)))
    obj$shade <-lapply(new, function(x,i,n)
           {
             pos <- index==x[1]
             
             if(is.na(sum(pos)))
             {
               return(index[n+as.numeric(x[2])])
             }else{
               pos <- which(pos)
               return(index[pos+n+as.numeric(x[2])])
             }
           },i=index,n=n)
    
  }else{
    obj$shade <- lapply(obj$shade, function(x,i,n)
    {
      pos <- index==x
      if(is.na(sum(pos)))
      {
        return(index[n])
      }else{
        pos <- which(pos)
        return(index[pos+n])
      }
    },index,n)
  }
  
  return(obj)
}


identity.field <- function(obj,...) {
 return(obj)
}

movement.field<-function(obj,n,rule,x=0,y=0,...) {
  
  if(rule=="x"){
    obj$pos.x[[1]]<-obj$pos.x[[1]]+18*(n-1)
  }else if(rule=="y"){
    obj$pos.y[[1]]<-obj$pos.y[[1]]-12*(n-1)
  }else{
    obj$pos.x[[1]]<-rep(x,length(obj$pos.x[[1]]-20*(n-1)))
    obj$pos.y[[1]]<-rep(y,length(obj$pos.y[[1]]-20*(n-1)))
  }
  return(obj)
}
# movement.field<-function(obj,n,rule,...) {
#   if(rule=="mov_hrl"){
#     obj[[1]]$pos.x<-obj[[1]]$pos.x+18*(n-1)
#   }else if(rule=="mov_hlr"){
#     obj[[1]]$pos.x<-obj[[1]]$pos.x-18*(n-1)
#   }else if(rule=="mov_vud"){
#     obj[[1]]$pos.y<-obj[[1]]$pos.y-12*(n-1)
#   }else if(rule=="mov_vdu"){
#     obj[[1]]$pos.y<-obj[[1]]$pos.y+12*(n-1)
#   }
#   return(obj)
# }

rotation.field<-function(obj,n,...) {
  obj$rotation<-Map('+', obj$rotation,(n-1)*pi/4)
  obj$theta.1<-Map('+', obj$theta.1,(n-1)*pi/4)
  obj$theta.2<-Map('+', obj$theta.2,(n-1)*pi/4)
  return(obj)
}

size.field<-function(obj,n,...) {
  obj$size.x<-Map('/', obj$size.x,(n*.9))
  obj$size.y<-Map('/', obj$size.y,(n*.9))
  return(obj)
}

margin.field<-function(obj,n,rules,...){
  index<-c(3:1,3:1,3:1)
  if(grepl("lwd",rules)){
    obj$lwd<- lapply(obj$lwd,function(x,i,n){i[x+n]+1},index,n)
  }else if(grepl("lty",rules)){
    obj$lty<- lapply(obj$lty,function(x,i,n){i[x+n]},index,n)
    }
  return(obj)
}

diff_shapes.field<-function(obj,n,rule,...) {
  if(length(obj$visible)!=3)
  {
    stop("You must have at least three forms to change shapes!")
  }
  if(grepl("inv",rule))
  {
    index<-c(3:1,3:1,3:1) #TL-LR
  }else{
    index<-c(1:3,1:3,1:3) #TR-LL
  }

  pos<-which(obj$visible==1)
  if(length(pos)>1){
    obj$visible[pos]<-0
    obj$visible[index[n]]<-1
  }else {
    obj$visible[pos]<-0
    obj$visible[index[pos+n]]<-1
  }
 
  return(obj)
}


logic.field<-function(obj,n,rule,seed,...) {
  if(length(obj$shape)<3)
  {
    stop("You must have three forms to apply a logical AND !")
  }
  ##gestione di più immagini
  domain<-1:length(obj$shape)
  obj$visible[domain]<-1
  set.seed(seed)
  fixed<-sample(domain,round(length(obj$shape)/5))
  domain<-setdiff(domain,fixed)
  half<-length(domain)%/%2
  index<-list()
  index[[1]]<-sample(domain,half)
  index[[2]]<-sample(setdiff(domain,index[[1]]),half)
  
  if(rule=="AND"){
    index[[3]]<-union(index[[1]],index[[2]])
    obj$visible[index[[n]]]<-0
  }else if(rule=="OR"){
    if(n<3){
      obj$visible[index[[n]]]<-0
    }
  }else if(rule=="XOR"){

    index[[3]]<-union(setdiff(domain,union(index[[1]],index[[2]])),fixed)
    obj$visible[index[[n]]]<-0
  }
  return(obj)
}


mental_transformation.field<-function(obj,n,rule,seed,...) {
  if(length(obj$shape)<2)
  {
    stop("You must have two forms to apply a mental transformation!")
  }
  set.seed(seed)
  if(grepl("line",rule))
  {
    index<-sample(c("line12","line12inv","line12both","line12both","line12both"),1)
  }else{
    index<-sample(c("white","grey","black","black","black"),1)
  }
  
  if(grepl("fill",rule))
  {
    obj<-hide(obj)
    if(n!=1){
      pos<-c(1,2,1)
      obj$visible[[pos[n]]]<-1
      obj$shade<-lapply(obj$shade, function(x,i)
      {
        return(i)
      },i=index)
    }else{
      obj$visible[[n]]<-1
    }
      
  }else if(grepl("rotation",rule))
  {
    
  }
  return(obj)
}

### Progressione numerica

create_dice.field<-function(object)
{
  #if(!any(unlist(object$tag)=="small")) ##idealmente risolvi
  #{
  #  stop("The function need to be resizeable")
  #}
  object<-movement(object,1,"pos",-10,11)
  object<-size(object,4)
  object2<-object
  for(row in 1:3)
  {
    obj<-movement(object,row,"y")
    if(row>1)
    {
      object2<-cof(object2,obj)
    }
    
    for( col in 2:3 )
    {
      obj<-movement(obj,2,"x")
      object2<-cof(object2,obj)
      
    }
  }
  object2<-hide(object2)
  return(object2)
}

# numeric_progression.field<-function(obj,n,rules,...){
#   index<-matrix(1:9,ncol=3,byrow=TRUE)
#   visibility<-matrix(obj$visible,ncol=3,byrow=TRUE)
#   visibility[1,1]<-1
#   if(grepl("h",rules) & grepl("inv",rules)){
#     n<-4-n
#     obj=show(obj,index[1:n,1])
#   }else if(grepl("v",rules) & grepl("inv",rules) ){
#     n<-4-n
#     obj=show(obj,index[1,1:n])
#   }else if(grepl("h",rules) & grepl("x2",rules)){
#     if(sum(visibility>1)){browser()}
#     obj=show(obj,index[1:n,colSums(visibility)>=1])
#   }else if(grepl("v",rules) & grepl("x2",rules)){
#     obj=show(obj,index[rowSums(visibility)>=1,1:n])
#   }else if(grepl("h",rules)){
#     obj=show(obj,index[1:n,1])
#   }else if(grepl("v",rules)){
#     obj=show(obj,index[1,1:n])
#   }
#   return(obj)
# }



logic_rules <- function(obj,n,...) {
  UseMethod("logic_rules")
}



logic_rules.Raven_matrix<-function(obj,rule) {
  
  if(length(obj[[1]]$shape)!=4)
  {
    stop("You must have four forms to apply a logical AND !")
  }
  
  squares<-paste0("Sq",1:9)
  if(rule=="OR"){
    ele<-list(Sq1=1,Sq2=2,Sq3=c(1,2),
              Sq4=3,Sq5=4,Sq6=c(3,4),
              Sq7=c(1,3),Sq8=c(2,4),Sq9=1:4)
    
  }else if(rule=="AND"){
    ele<-list(Sq1=c(1,2,4),Sq2=c(1,2,3),Sq3=c(1,2),
              Sq4=c(1,3,4),Sq5=c(1,2,4),Sq6=c(1,4),
              Sq7=c(1,4),Sq8=c(1,2),Sq9=1)
    
  }else if(rule=="XOR"){
    ele<-list(Sq1=1,Sq2=c(1,4),Sq3=4,
              Sq4=c(1,2),Sq5=1:4,Sq6=c(3,4),
              Sq7=2,Sq8=c(2,3),Sq9=3)
    
  }
  for(i in squares)
  {
    new<-integer(4)
    new[ele[[i]]]<-1
    obj[[i]]$visible<-new
  }
  obj$hrule<-c(obj$hrule,rule)
  obj$vrule<-c(obj$vrule,rule)
  attr(obj, "class") <- "Raven_matrix"
  return(obj)
}

numeric_progression.Raven_matrix<-function(obj,rules,n=1,...){
  squares<-paste0("Sq",1:9)
  for(i in 1:length(squares))
  {
    if(sum(obj[[squares[i]]]$visible)>1)
    {
      stop("You need just one shape for square")
    }
    elements<-decof(obj[[squares[i]]])
    f<-elements[obj[[squares[i]]]$visible==1][[1]]
   
    obj[[squares[i]]]<-create_dice(f)
    ind<-  c(1,2,4,5,7,3,8,6,9)
    if(rules=="TL-LR-increasing")
    {
      if(i<=3){
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:i])
      }else if(i<=6){
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:(i-2)])
      }else{
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:(i-4)])
      }
    }else if(rules=="LL-TR")
    {
      if(i<=3){
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:(i+2)])
      }else if(i<=6){
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:(i-2)])
      }else{
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:(i-6)])
      }
    }
    else if("v.increasing"==rules)
    {
      if(i<=3){
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:(1*n)])
      }else if(i<=6){
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:(2*n)])
      }else{
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:(3*n)])
      }
    }
    else if("h.increasing"==rules)
    {
      if(i<=3){
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:(i*n)])
      }else if(i<=6){
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:((i-3)*n)])
      }else{
        obj[[squares[i]]] <- show(obj[[squares[i]]],ind[1:((i-6)*n)])
      }
    }
  }
  obj$hrule<-c(obj$hrule,"quant")
  obj$vrule<-c(obj$vrule,"quant")
  return(obj)
}
  
  
  



