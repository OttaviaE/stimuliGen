
rm(list = ls())
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")
library(DescTools)
set.seed(999)


blu = "deepskyblue3"

giallo = "gold"

rosso = "firebrick"


a=5.2 #proporzione dei canvas
spessore= 9 # spessore linee 

draws<- function(obj, main = NULL, canvas = TRUE, bg = "white",mar=c(1,1,1,1),xlim=16,by=3.5) {
  library(DescTools)
  if (canvas == TRUE)
  {
    Canvas(xlim=xlim,mar=mar, main = main, bg = bg)
  }
  for(j in 1:length(obj$shape))
  {
    if(obj$visible[[j]]==1)
    {
      if(obj$num[[j]][1]==1){
        if(grepl("line",obj$shade[[j]][1]))
        {
          elements<-decof(obj)
          plotting_lines<-which(obj$visible==1 & grepl("line",unlist(obj$shade)))
          for(ll in 1:length(plotting_lines)){
            line(elements[[plotting_lines[[ll]]]],obj$shade[[j]][1],lwd=9,by=by) #Pejo tacon che sbrego
            
          }
          obj$shade[[j]][1] <- NA
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


############################################################
## YOUNG001
svg(paste0(getwd(), "/young001_11.svg"))
Canvas(xlim = 17,ylim = 17)
draw(rectangle(s.x=-50,s.y=50,shd=blu,pos.x=+10,pos.y=-7), canvas = F)
draw(diagline(pos.x=-50, s.x = 50, lwd = 3, shd = giallo), canvas = F)
for(i in seq(-50, 50, by = 3.5)) {
  draw(diagline(pos.x = i, s.x=40, lwd = 3),
       canvas = F)
}


draw(diagline.inv(pos.x=-50, s.x = 50, lwd = 3), canvas = F)
for(i in seq(-50, 50, by = 3.5)) {
  draw(diagline.inv(pos.x = i, s.x=40, lwd = 3),
       canvas = F)
}
# polygon(x=c(-18,-18),y=c(-50,50))
# polygon(x=c(-6,-6),y=c(-50,50))
# polygon(x=c(6,6),y=c(-50,50))
# polygon(x=c(18,18),y=c(-50,50))
# polygon(y=c(-18,-18),x=c(-50,50))
# polygon(y=c(-6,-6),x=c(-50,50))
# polygon(y=c(6,6),x=c(-50,50))
# polygon(y=c(18,18),x=c(-50,50))

clip(6,18,-18,-6)
draw(rectangle(s.x=20,s.y=20,shd="white",pos.x=12,pos.y=-12,lty=0),
     canvas = FALSE)
dev.off()





container = rectangle(s.x=10,s.y=10,shd=blu,lty=0)
svg(paste0(getwd(), "/young001_correct.svg"))

Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas = FALSE)
#draw(rectangle(s.x=-6,s.y=6,shd=blu,pos.x=0,pos.y=0), canvas = F)


draw(diagline(pos.x=-50, s.x = 50, lwd = spessore, shd = giallo), canvas = F)
for(i in seq(-50+12, 50-12, by = 3.5)) {
  draw(diagline(pos.x = i, s.x=40, lwd = spessore),
       canvas = F)
}

draw(diagline.inv(pos.x=-50+12, s.x = 50-12, lwd = spessore), canvas = F)
for(i in seq(-50, 50, by = 3.5)) {
  draw(diagline.inv(pos.x = i, s.x=40, lwd = spessore),
       canvas = F)
}

dev.off()

svg(paste0(getwd(), "/young001_ic.scale.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas = FALSE)

draws(rectangle(s.x=10,s.y=10,shd="line.12",lty = 0),
      canvas = FALSE)
draws(rectangle(s.x=10,s.y=10,shd="line.12.h",lty = 0),
      canvas = FALSE)
dev.off()

svg(paste0(getwd(), "/young001_diff1.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas = FALSE)

dev.off()

svg(paste0(getwd(), "/young001_diff2.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas = FALSE)

for (i in seq(-6, 7, by = 3)) {
  for (j in seq(-3, 3, by = 3)) {
    draw(dot(pos.x = i, pos.y = j, size.x = 0.5, size.y = 0.5), canvas = F)
  }
}

dev.off()

svg(paste0(getwd(), "/young001_ic.flip.svg"))

Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas = FALSE)

for(i in seq(-20, 20, by = 3.5)) {
  draw(vline(pos.x = i, s.x=7, lwd = spessore), canvas = F)
}

for(i in seq(-20, 20, by = 3.5)) {
  draw(hline(pos.y = i, s.x=7, lwd = spessore), canvas = F)
}

dev.off()

############################################################
## YOUNG002

svg(paste0(getwd(), "/young002_11.svg"))
Canvas(xlim = 17,ylim = 17)
draw(rectangle(s.x=-50,s.y=50,shd=giallo,pos.x=+10,pos.y=-7),canvas = FALSE)
draw(dot(pos.x = -30, pos.y = 15, size.x = 1, size.y = 1, shd = blu), canvas = F)

for (i in seq(-30, 30, by = 4)) {
  for (j in seq(15, -19, by = -4)) {
    draw(dot(pos.x = i, pos.y = j, size.x = 1, size.y = 1, shd = blu), canvas = F)
  }
}

clip(6,18,-18,-6)
draw(rectangle(s.x=20,s.y=20,shd="white",pos.x=12,pos.y=-12,lty=0),
     canvas = FALSE)
dev.off()


svg(paste0(getwd(), "/young002_correct.svg"))
container = rectangle(s.x=100,s.y=100,shd=giallo)

Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

for (i in seq(-30+12, 30-12, by = 4)) {
  for (j in seq(15-12, -19+12, by = -4)) {
    draw(dot(pos.x = i, pos.y = j, size.x = 1, size.y = 1, shd = blu), canvas = F)
  }
}
dev.off()

svg(paste0(getwd(), "/young002_ic.neg.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
draw(rectangle(s.x=10,s.y=10,shd=blu,pos.x=0,pos.y=0),
     canvas = FALSE)
for (i in seq(-30+12, 30-12, by = 4)) {
  for (j in seq(15-12, -19+12, by = -4)) {
    draw(dot(pos.x = i, pos.y = j, size.x = 1, size.y = 1, shd = giallo), canvas = F)
  }
}
dev.off()

svg(paste0(getwd(), "/young002_diff1.svg"))

Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
dev.off()

svg(paste0(getwd(), "/young002_diff2.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
for (i in seq(-30+12, 30-12, by = 4)) {
  for (j in seq(15-12, -19+12, by = -4)) {
    draw(square(pos.x = i, pos.y = j, s.x = 1, s.y = 1, shd = blu), canvas = F)
  }
}

dev.off()

svg(paste0(getwd(), "/young002_dist.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
draws(rectangle(s.x=10,s.y=10,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
draws(rectangle(s.x=10,s.y=10,shd="line.12"),by=3.5,
      canvas = FALSE)

dev.off()


############################################################
## YOUNG003
svg(paste0(getwd(), "/young003_11.svg"))
Canvas(xlim = 17,ylim = 17)
draw(rectangle(s.x=-50,s.y=50,shd=blu,pos.x=+10,pos.y=-7), canvas = F)
draw(vline(pos.x = -13, s.x = 50, lwd = 3), canvas = F)
draw(vline(pos.x = 13, s.x = 50, lwd = 3, lty = 1), canvas = F)
draw(vline(pos.x = 10, s.x = 50, lwd = 3), canvas = F)
draw(hline(pos.y = 9, s.x = 50, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = -12, s.x = 50, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = -15, s.x = 50, lwd = 3, lty = 1), canvas = F)

clip(6,18,-18,-6)
draw(rectangle(s.x=20,s.y=20,shd="white",pos.x=12,pos.y=-12,lty=0),
     canvas = FALSE)

dev.off()

svg(paste0(getwd(), "/young003_correct.svg"))
container = rectangle(s.x=10,s.y=10,shd=blu)

Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

draw(rectangle(s.x=-50,s.y=50,shd=blu,pos.x=+10,pos.y=-7), canvas = F)
draw(vline(pos.x = -13-12, s.x = 50, lwd = spessore), canvas = F)
draw(vline(pos.x = 13-12, s.x = 50, lwd = spessore, lty = 1), canvas = F)
draw(vline(pos.x = 10-12, s.x = 50, lwd = spessore), canvas = F)
draw(hline(pos.y = 9+12, s.x = 50, lwd = spessore, lty = 1), canvas = F)
draw(hline(pos.y = -12+12, s.x = 50, lwd = spessore, lty = 1), canvas = F)
draw(hline(pos.y = -15+12, s.x = 50, lwd = spessore, lty = 1), canvas = F)

dev.off()



svg(paste0(getwd(), "/young003_ic_alternativa3.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)


draw(vline(pos.x = -5, pos.y = -0, 
           s.x  = 10, lwd = spessore, lty = 1), canvas = F)
draw(vline(pos.x = -2, pos.y = 0, 
           s.x = 10, lwd = spessore), canvas = F)

draw(hline(pos.y = 2, 
           pos.x = 0,  s.x = 10, lwd = spessore, lty = 1), canvas = F)
draw(hline(pos.y = 5, pos.x = 0, s.x = 10, lwd = spessore, lty = 1), canvas = F)
dev.off()

svg(paste0(getwd(), "/young003_ic_alternativa.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)


draw(vline(pos.x = 3, pos.y = -0, 
           s.x  = 10, lwd = spessore, lty = 1), canvas = F)
draw(vline(pos.x = 2, pos.y = 0, 
           s.x = 10, lwd = spessore), canvas = F)

draw(hline(pos.y = -4, 
           pos.x = 0,  s.x = 10, lwd = spessore, lty = 1), canvas = F)
draw(hline(pos.y = -3, pos.x = 0, s.x = 10, lwd = spessore, lty = 1), canvas = F)
dev.off()

svg(paste0(getwd(), "/young003_ic_alternativa2.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

draw(rectangle(s.x=-50,s.y=50,shd=blu,pos.x=+10,pos.y=-7), canvas = F)
draw(vline(pos.x = -13-12, s.x = 50, lwd = spessore), canvas = F)

draw(vline(pos.x = 10-12, s.x = 50, lwd = spessore), canvas = F)
draw(hline(pos.y = 9+12, s.x = 50, lwd = spessore, lty = 1), canvas = F)
draw(hline(pos.y = -12+12, s.x = 50, lwd = spessore, lty = 1), canvas = F)
draw(hline(pos.y = -15+12, s.x = 50, lwd = spessore, lty = 1), canvas = F)
dev.off()


svg(paste0(getwd(), "/young003_ic1.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

draw(vline(pos.x = -5, pos.y = -0, 
           s.x  = 10, lwd = spessore, lty = 1), canvas = F)
draw(vline(pos.x = -2, pos.y = 0, 
           s.x = 10, lwd = spessore), canvas = F)

draw(hline(pos.y = 0, 
           pos.x = 0,  s.x = 10, lwd = spessore, lty = 1), canvas = F)
draw(hline(pos.y = 3, pos.x = 0, s.x = 10, lwd = spessore, lty = 0), canvas = F)
dev.off()

svg(paste0(getwd(), "/young003_diff.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
dev.off()

svg(paste0(getwd(), "/young003_wp.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

dev.off()

svg(paste0(getwd(), "/young003_wpalternativa1.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
draws(rectangle(s.x=10,s.y=10,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
draws(rectangle(s.x=10,s.y=10,shd="line.12"),by=3.5,
      canvas = FALSE)
dev.off()

svg(paste0(getwd(), "/young003_wpalternativa2.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
draw(rotation(vline(pos.x = -5, pos.y = -0, 
           s.x  = 10, lwd = spessore, lty = 1),2), canvas = F)
draw(rotation(vline(pos.x = -2, pos.y = 0, 
           s.x = 10, lwd = spessore),2), canvas = F)

draw(rotation(hline(pos.y = 0, 
           pos.x = 0,  s.x = 10, lwd = spessore, lty = 1),2), canvas = F)
dev.off()

############################################################
## YOUNG004
svg(paste0(getwd(), "/young004_11.svg"))
Canvas(xlim = 17,ylim = 17)

draw(rectangle(s.x=-50,s.y=50,shd=giallo,pos.x=+10,pos.y=-7), canvas = F)
draw(vline(pos.x=-50, s.x = 30, lwd = 1), canvas = F)
for(i in seq(-30, 30, by = 2)) {
  
  draw(vline(pos.x = i, s.x=40, lwd = 1+ abs(i)/2), 
       canvas = F,  bg = "white")
}

clip(6,18,-18,-6)
draw(rectangle(s.x=20,s.y=20,shd="white",pos.x=12,pos.y=-12,lty=0),
     canvas = FALSE)

dev.off()

svg(paste0(getwd(), "/young004_correct.svg"))
container = rectangle(s.x=10,s.y=10,shd=giallo)
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

for(i in seq(-30-12, 30-12, by = 2)) {
  
  draw(vline(pos.x = i, s.x=40, lwd = 1+ abs(i+12)*2), 
       canvas = F,  bg = "white")
}
dev.off()

svg(paste0(getwd(), "/young004_wp1.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

for(i in seq(14, 30, by = 1)) {
  
  draw(vline(pos.x = i-22, s.x=40, lwd = spessore+3), 
       canvas = F,  bg = "white")
}
dev.off()

svg(paste0(getwd(), "/young004_diff.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

dev.off()

svg(paste0(getwd(), "/young004_dist.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

draws(rectangle(s.x=10,s.y=10,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
dev.off()

svg(paste0(getwd(), "/young004_wp2.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12"),by=3.5,
      canvas = FALSE)
dev.off()

############################################################
## YOUNG005
svg(paste0(getwd(), "/young005_11.svg"))
Canvas(xlim = 17,ylim = 17)

draw(rectangle(s.x=-50,s.y=50,pos.x=+10,pos.y=-7,shd=blu),canvas=FALSE)

for(j in seq(0, 3, by = .2)) {
  draw(diagline(pos.x = 30,pos.y = 10, s.x=100, rot=(pi/8)*j, lwd = 3,lty=2), 
       canvas = F)
}

for(j in c(seq(-20, -5, by = 3),seq(7, 17, by = 1.5))) {
  draw(vline(pos.x = j, s.x=100, lwd = 3,lty = 1), 
       canvas = F)
}

for(j in seq(-16, -6, by = 3)) {
  draw(hline(pos.y = j, s.x=100, lwd = 3,lty = 1), 
       canvas = F)
}

clip(6,18,-18,-6)
draw(rectangle(s.x=20,s.y=20,shd="white",pos.x=12,pos.y=-12,lty=0),
     canvas = FALSE)

dev.off()

svg(paste0(getwd(), "/young005_correct.svg"))
container = rectangle(s.x=10,s.y=10,shd=blu)
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)

for(j in seq(0, 3, by = .2)) {
  draw(diagline(pos.x = 30,pos.y = 10, s.x=100, rot=(pi/8)*j, lwd = spessore,lty=2), 
       canvas = F)
}

for(j in c(seq(-20+12, -5+12, by = 3),seq(7-12, 17-12, by = 1.5))) {
  draw(vline(pos.x = j, s.x=100, lwd = spessore,lty = 1), 
       canvas = F)
}

for(j in seq(-16+12, -6+12, by = 3)) {
  draw(hline(pos.y = j, s.x=100, lwd = spessore,lty = 1), 
       canvas = F)
}

dev.off()


svg(paste0(getwd(), "/young005_wp1.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
for(j in seq(0, 3, by = .2)) {
  draw(diagline(pos.x =30- 15,pos.y = 10-(-10), s.x=100, rot=(pi/8)*j, lwd = 3,lty=2), 
       canvas = F)
}

for(j in c(seq(-20, -5, by = 3),seq(5, 15, by = 1.5))) {
  draw(vline(pos.x = j-15, s.x=100, lwd =spessore,lty = 1), 
       canvas = F)
}

dev.off()

svg(paste0(getwd(), "/young005_diff1.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
dev.off()

svg(paste0(getwd(), "/young005_wp2.svg"))
Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
draws(rectangle(s.x=10,s.y=10,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
dev.off()


svg(paste0(getwd(), "/young005_diff2.svg"))

Canvas(xlim = a)
clip(6,-6,6,-6)
draw(container,canvas=FALSE)
draws(rectangle(s.x=10,s.y=10,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
draws(rectangle(s.x=10,s.y=10,shd="line.12"),by=3.5,
      canvas = FALSE)
dev.off()

