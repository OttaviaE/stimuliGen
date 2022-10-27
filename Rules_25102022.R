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


##Rules

fill.field<-function(obj,n,...){
  index <- rep(c("white","grey","black"),3)
   pos <- index==obj$shade
  if(sum(pos)==0)
  {
    obj$shade<-index[n] 
  }else{
    pos <- which(pos)
    obj$shade<-index[pos+n]
  }
  return(obj)
}


identity.field <- function(obj,...) {
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
  obj$rotation<-obj$rotation+(n-1)*pi/4
  return(obj)
}

size.field<-function(obj,n,...) {
  obj$size.x<-obj$size.x/(n*.9)
  obj$size.y<-obj$size.y/(n*.9)
  return(obj)
}

margin.field<-function(obj,n,rules,...){
  index<-c(3:1,3:1,3:1)
  if(grepl("lwd",rules)){
    obj$lwd<- index[obj$lwd+n]+1
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
    ele<-list(Sq9=1,Sq8=2,Sq7=c(1,2),
              Sq6=3,Sq5=4,Sq4=c(3,4),
              Sq3=c(1,3),Sq2=c(2,4),Sq1=1:4)
    
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
  
  attr(obj, "class") <- "Raven_matrix"
  return(obj)
}