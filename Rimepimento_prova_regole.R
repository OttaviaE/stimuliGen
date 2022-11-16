line<-function(obj,rule)
{
  #coefficente angolare di default
  m<-1
  coords<-DrawRegPolygon(x = obj$pos.x[[1]], y = obj$pos.y[[1]], rot = obj$rotation[[1]],
                         radius.x = obj$size.x[[1]], radius.y = obj$size.y[[1]],
                         nv = obj$nv[[1]],plot = F)
  # coords<-matrix(c(coords$x,coords$y),ncol = 2)
  
  if(grepl("inv",rule))
  {
    m=-1
  }else if(grepl("h",rule))
  {
    m=0
  }
  
  
  if(grepl("1",rule))
  {
    tr<-coords[1:2]
    tr$x[!coords$x>obj$pos.x[[1]]]<-obj$pos.x[[1]]
    tr$y[!coords$y>obj$pos.y[[1]]]<-obj$pos.y[[1]]
    for(i in seq(-20 ,20)){
      filling(i,coords=tr,m=m)
      
    }
  }
  if(grepl("2",rule))
  {
    tr<-coords[1:2]
    tr$x[!coords$x>obj$pos.x[[1]]]<-obj$pos.x[[1]]
    tr$y[!coords$y<obj$pos.y[[1]]]<-obj$pos.y[[1]]
    for(i in seq(-20 ,20)){
      filling(i,coords=tr,m=m)
      
    }
  }
  if(grepl("3",rule))
  {
    tr<-coords[1:2]
    tr$x[!coords$x<obj$pos.x[[1]]]<-obj$pos.x[[1]]
    tr$y[!coords$y<obj$pos.y[[1]]]<-obj$pos.y[[1]]
    for(i in seq(-20 ,20)){
      filling(i,coords=tr,m=m)
      
    }
  }
  if(grepl("4",rule))
  {
    tr<-coords[1:2]
    tr$x[!coords$x<obj$pos.x[[1]]]<-obj$pos.x[[1]]
    tr$y[!coords$y>obj$pos.y[[1]]]<-obj$pos.y[[1]]
    for(i in seq(-20 ,20)){
      filling(i,coords=tr,m=m)
      
    }
  }
  
  
}
###


found_points<-function(x1,x2,y1,y2,m1,q1){
  
  delta_y<- (y2 - y1)
  delta_x<- (x2 - x1)
  if(round(x2,3)==round(x1,3)){
    y<-m1*x1+q1
    x<-x1
  }else if(round(y1,3)==round(y2,3)){
    y<-y1
    x<-(y1-q1)
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




##TROPPI MODI DIVERSI DI APPLICARLO
#1) Regola
#2) oggetto diverso  con i suoi metodi
#3) Funzione che si applica alle matrici
#4) tipo di riempimento (provo questa opzione)

draw.field<- function(obj, main = NULL, canvas = TRUE,filling=FALSE) {
  library(DescTools)
  if (canvas == TRUE)
  {
    Canvas(xlim=16,mar=c(1,1,1,1), main = main)
  }
  for(j in 1:length(obj$shape))
  {
    if(obj$visible[[j]]==1)
    {
      if(obj$num[[j]][1]==1){
        if(grepl("line", obj$shade[[j]] ))
        {
          rule<-obj$shade[[j]]
          obj$shade[[j]]<-NA
          
        }
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

draw(square())
line(square(),"123")
