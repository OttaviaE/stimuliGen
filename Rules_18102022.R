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

logic <- function(obj,n,seed,rule,...) {
  UseMethod("logic")
}

movement <- function(obj,n,rule,...) {
  UseMethod("movement")
}

identity <- function(obj,...) {
  UseMethod("identity")
}

##Rules
identity.field<-function(obj,...) {
  return(obj)
}

movement.field<-function(obj,n,rule,...) {
  if(rule=="mov_hrl"){
    obj$pos.x<-obj$pos.x+18*(n-1)
  }else if(rule=="mov_hlr"){
    obj$pos.x<-obj$pos.x-18*(n-1)
  }else if(rule=="mov_vud"){
    obj$pos.y<-obj$pos.y-12*(n-1)
  }else if(rule=="mov_vdu"){
    obj$pos.y<-obj$pos.y+12*(n-1)
  }
  return(obj)
}

rotation.field<-function(obj,n,...) {
  obj$rotation<-obj$rotation+n*pi/6
  return(obj)
}

size.field<-function(obj,n,...) {
  obj$size.x<-obj$size.x/n
  obj$size.y<-obj$size.y/n
  return(obj)
}

margin.field<-function(obj,n,rules,...){
  index<-c(3:1,3:1,3:1)
  if(grepl("lwd",rules)){
    obj$lwd<- index[obj$lwd+n]
  }else if(grepl("lty",rules)){
    obj$lty<-index[obj$lty+n]
    }
  return(obj)
}

diff_shapes.field<-function(obj,n,...) {
  if(length(obj$visible)!=3)
  {
    stop("You must have at least three forms to change shapes!")
  }
  index<-c(3:1,3:1,3:1)
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



logic.field<-function(obj,n,seed,rule,...) {
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

# AND.Raven_matrix<-function(obj) {
#   
#   if(length(obj[[1]]$shape)!=5)
#   {
#     stop("You must have five forms to apply a logical AND !")
#   }
#   
#   squares<-paste0("Sq",1:9)
#   
#   obj[[1]]$visible<-c(1,1,1,0,0)
#   obj[[2]]$visible<-c(1,0,1,0,1)
#   obj[[3]]$visible<-c(1,0,1,0,0)
#   
#   obj[[4]]$visible<-c(1,1,0,1,0)
#   obj[[5]]$visible<-c(1,0,0,1,1)
#   obj[[6]]$visible<-c(1,0,0,1,0)
#   
#   obj[[7]]$visible<-c(1,1,0,0,0)
#   obj[[8]]$visible<-c(1,0,0,0,1)
#   obj[[9]]$visible<-c(1,0,0,0,0)
#   return(obj)
# }