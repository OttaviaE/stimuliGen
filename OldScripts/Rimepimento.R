
q=seq(0, square()$ size.x[[1]], by = 1)
ray=square()$ size.x[[1]]-q

#ray<- 

draw(diagline(p.x = q,
              p.y = q,
              s.x = list(sqrt(2*(ray^2))),
              s.y = list(sqrt(2*(ray^2)))))




draw(circle())
for(i in seq(1,length(da$x),by=4))
{
  
  polygon(c(x,-x),c(y,-y))
}
###

draw(circle())
coords<-prova(circle())
prova<-function(obj){
  j=1
  x<-DrawRegPolygon(x = obj$pos.x[[j]], y = obj$pos.y[[j]], rot = obj$rotation[[j]], 
                    radius.x = obj$size.x[[j]], radius.y = obj$size.y[[j]], nv = obj$nv[[j]],
                    lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]],plot = F)
  return(x)
}

found_points<-function(x1,x2,y1,y2,m1,q1){
  
  delta_y<- (y2 - y1)
  delta_x<- (x2 - x1)
  if(round(x2,3)==round(x1,3)){
    y<-m1*x1+q1
    x<-x1
  }else if(round(y1,3)==round(y2,3)){
    y<-y1
    x<-(y1-q1)/m1
  }else{
    m2 <-  delta_y / delta_x
    q2 <- y1-x1*m2
    x <- (q1 - q2) / (m2 - m1)
    y <- x*m2+q2
  }
  return(c(x,y))
}


filling<-function(q,m,coords)
{
  
  pt<-matrix(ncol=2,nrow = 2)
  n_solu<-1
  index<-c(1:length(coords$x),1)
  for(i in 2:length(index))
  {
    solution<-found_points(coords$x[index[i-1]],coords$x[index[i]],
                           coords$y[index[i-1]],coords$y[index[i]],
                           m,q)
    
    control_x<-min(coords$x[index[i-1]],coords$x[index[i]])<=solution[1] &
      max(coords$x[index[i-1]],coords$x[index[i]])>=solution[1]
    control_y<-min(coords$y[index[i-1]],coords$y[index[i]])<=solution[2] &
      max(coords$y[index[i-1]],coords$y[index[i]])>=solution[2]
    
    if(control_x && control_y && n_solu<=2)
    {
      
      pt[n_solu,]<-solution
      n_solu<-n_solu+1
    }
  }
  polygon(pt[,1],pt[,2])
  return(pt)
}

coords$x[coords$x<0]<-0
coords$y[coords$y<0]<-0
for(i in seq(-500 ,+500)){
  
  filling(i,coords=coords,m=1)
}

