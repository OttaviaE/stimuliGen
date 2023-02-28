##sequence ---
sequence <- function(obj, canvas,by.x,by.y,bg,mar,movement) {
  UseMethod("sequence")
}

sequence.field<-function(obj, canvas = TRUE,by.x=3,by.y=3,bg = "white",mar=c(1,1,1,1),movement=c("xy")) {
  if(canvas==TRUE)
  {
    Canvas(xlim = 17, pty="s")
  }
  draw(rectangle(s.x=-50,s.y=50,shd=bg,pos.x=+10,pos.y=-7), canvas = FALSE)
  
  if(movement=="x")
  {
    for(i in seq(-18,18,by=by.x))
    {
      obj$pos.x<-i
      draw(obj,canvas=FALSE)
    }
  }else if(movement=="y")
  {
    for(i in seq(-18,18,by=by.y))
    {
      obj$pos.y<-i
      draw(obj,canvas=FALSE)
    }
  }else{
    for(i in seq(-18,18,by=by.x))
    {
      for(j in seq(-18,18,by=by.y))
      {
      obj$pos.x<-i
      obj$pos.y<-j
      draw(obj,canvas=FALSE)
      }
    }
  }
} 

##sequence ---
rotation <- function(obj, canvas,by.x,by.y,bg,mar,ray,type) {
  UseMethod("rotation")
}

rotation.field<-function(obj, canvas = TRUE,
                         bg = "white",mar=c(1,1,1,1),
                         angle=pi/12,type="constant" ) {
  if(canvas==TRUE)
  {
    Canvas(xlim = 17)
  }
  draw(rectangle(s.x=-50,s.y=50,shd=bg,pos.x=+10,pos.y=-7), canvas = F)
  if(type=="constant")
  {
    
      for(j in seq(ray,-ray,by=-by.y))
      {
        obj$pos.x<-cos(angle)-sin(angle)
        obj$pos.y<-cos(angle)-sin(angle)
        draw(obj,canvas=FALSE)
      }

  }
} 



