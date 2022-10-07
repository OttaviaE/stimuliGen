##Rules methods 
rotation <- function(obj,n) {
  UseMethod("rotation")
}

resize <- function(obj,n) {
  UseMethod("resize")
}

diff_shapes <- function(obj,n) {
  UseMethod("diff_shapes")
}

AND <- function(obj) {
  UseMethod("AND")
}
##Rules
rotation.field<-function(obj,n) {
  obj$rotation<-obj$rotation+n*pi/6
  return(obj)
}

resize.field<-function(obj,n) {
  obj$size.x<-obj$size.x/n
  obj$size.y<-obj$size.y/n
  return(obj)
}

diff_shapes.field<-function(obj,n) {
  if(length(obj$visible)!=3)
  {
    stop("You must have at least three forms to change shapes!")
  }
  index<-c(1:3,1:2)
  obj$visible<-c(0,0,0)
  obj$visible[index[n]]<-1
  return(obj)
}

AND.Raven_matrix<-function(obj) {
  
  if(length(obj[[1]]$shape)!=5)
  {
    stop("You must have five forms to apply a logical AND !")
  }
  
  squares<-paste0("Sq",1:9)
  
  obj[[1]]$visible<-c(1,1,1,0,0)
  obj[[2]]$visible<-c(1,0,1,0,1)
  obj[[3]]$visible<-c(1,0,1,0,0)
  
  obj[[4]]$visible<-c(1,1,0,1,0)
  obj[[5]]$visible<-c(1,0,0,1,1)
  obj[[6]]$visible<-c(1,0,0,1,0)
  
  obj[[7]]$visible<-c(1,1,0,0,0)
  obj[[8]]$visible<-c(1,0,0,0,1)
  obj[[9]]$visible<-c(1,0,0,0,0)
  return(obj)
}