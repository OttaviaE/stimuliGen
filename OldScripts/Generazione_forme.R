shape<-function(form){
  if(form==1)#coordinates for printing the rectangle
  {
    x<-c(25,25,65,65)
    y<-c(30,80,80,30)
  }else if(form==2)#coordinates for printing a T
  {
    x<-c(20,60,60,52,52,38,38,20)
    y<-c(70,70,55,55,20,20,55,55)
  }else if(form==3)#coordinates for ellipse 
  {
    x<-c(seq(-6,6,by=.1),seq(6,-6,by=-.1))+40
    y<-c(-sqrt((9-(seq(-6,6,by=.1))^2/4)),sqrt((9-(seq(-6,6,by=.1))^2/4)))*3+60
  }
  return(data.frame(x,y))
}


par(mfrow=c(3,3))

cols<-c("white","grey","black","black","white","grey","grey","black")
for(i in 1:8){
  
  plot(c(10-10*n, 40+10*n), c(30-10*n, 80+10*n), type = "n", xlab = "", ylab = "", 
       axes = F)
  
  coords<-shape("elipse")
  polygon(coords$x,coords$y)
}

plot(c(-10, 10), c(-30, 30), type = "n", xlab = "", ylab = "", 
     axes = F)

plot(c(-100, 100), c(-100, 100), type = "n", xlab = "", ylab = "", 
     axes = F)


polygon(x,y)


                       