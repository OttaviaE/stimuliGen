source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")


rotation.field<-function(obj,n,rule="rot",...) {
  numbers<-unlist(strsplit(rule,split=""))
  num<-4
  for(i in 1:length(numbers))
  {
    if(any(numbers[i]==1:9)){
      num<-which(numbers[i]==1:9) 
    }
  }
  
  rot.x<-function(x,alpha)
  {
    return(cos(alpha)*x+sin(alpha)*x)
  }
  rot.y<-function(y,alpha)
  {
    return(cos(alpha)*y-sin(alpha)*y)
  }

  if(grepl("inv",rule)){
    obj$rotation<-Map('+', obj$rotation,(n-1)*-pi/num)
    obj$theta.1<-Map('+', obj$theta.1,(n-1)*-pi/num)
    obj$theta.2<-Map('+', obj$theta.2,(n-1)*-pi/num)
    obj$pos.x<-Map('rot.x', obj$pos.x,(n-1)*-pi/num)
    obj$pos.y<-Map('rot.y', obj$pos.y,(n-1)*-pi/num)
  }else{
    obj$rotation<-Map('+', obj$rotation,(n-1)*pi/num)
    obj$theta.1<-Map('+', obj$theta.1,(n-1)*pi/num)
    obj$theta.2<-Map('+', obj$theta.2,(n-1)*pi/num)
    obj$pos.x<-Map('rot.x', obj$pos.x,(n-1)*pi/num)
    obj$pos.y<-Map('rot.y', obj$pos.y,(n-1)*pi/num)
  }
  return(obj)
}

par(mfrow=c(1,5))
for(i in 1:5){
  draw(rotation(lily(),i))
}

par(mfrow=c(1,5))
for(i in 1:5){
  draw(size(bow.tie(),i))
}

par(mfrow=c(1,5))
for(i in 1:5){
  draw(size(cross(),i))
  
}
par(mfrow=c(1,5))
for(i in 1:5){
  draw(size(dice(),i))
}

par(mfrow=c(1,5))
for(i in 1:5){
  draw(size(circle(),i))
}
