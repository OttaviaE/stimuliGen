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

  for(i in seq(-20 ,20)){
    filling(q=i,m=m,coords=coords) #,p.x=obj$pos.x[[1]],p.y=obj$pos.y[[1]], rule)
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
    x<-(y1-q1)/m1
  }else{
    m2 <-  delta_y / delta_x
    q2 <- y1-x1*m2
    x <- (q1 - q2) / (m2 - m1)
    y <- x*m1+q2
  }
  return(c(x,y))
}





##TROPPI MODI DIVERSI DI APPLICARLO
#1) Regola
#2) oggetto diverso  con i suoi metodi
#3) Funzione che si applica alle matrici
#4) tipo di riempimento (provo questa opzione)

draw.field<- function(obj, main = NULL, canvas = TRUE, hide = FALSE) {
  library(DescTools)
  if (hide == TRUE) {
    obj$Sq9 = hide(obj$Sq9)
  }
  if (canvas == TRUE)
  {
    Canvas(xlim=16,mar=c(1,1,1,1), main = main)
  }
  for(j in 1:length(obj$shape))
  {
    if(obj$visible[[j]]==1)
    {
      if(obj$num[[j]][1]==1){
        
      if(grepl("line",obj$shade[[j]][1]))
      {
        line(obj,obj$shade[[j]][1])
        obj$shade <- NA
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

oggetto<-pentagon()
oggetto$shade[[1]]<-"line1"
draw(oggetto)
line(pentagon(),"42inv")
line(pentagon(),"1.v")

