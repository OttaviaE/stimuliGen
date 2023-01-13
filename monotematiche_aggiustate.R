
rm(list = ls())
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")
set.seed(999)

blu = "deepskyblue3"

giallo = "gold"

rosso = "firebrick"




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

container = rectangle(s.x=6,s.y=6,shd=blu,lty=0)
svg(paste0(getwd(), "/young001_correct.svg"))

draw(container,xlim = 6)
draw(rectangle(s.x=-50,s.y=50,shd=blu,pos.x=+10,pos.y=-7), canvas = F)
draw(diagline(pos.x=-50, s.x = 50, lwd = 3, shd = giallo), canvas = F)
for(i in seq(-50+12, 50-12, by = 3.5)) {
  draw(diagline(pos.x = i, s.x=40, lwd = 3),
       canvas = F)
}

draw(diagline.inv(pos.x=-50+12, s.x = 50-12, lwd = 3), canvas = F)
for(i in seq(-50, 50, by = 3.5)) {
  draw(diagline.inv(pos.x = i, s.x=40, lwd = 3),
       canvas = F)
}

dev.off()

svg(paste0(getwd(), "/young001_ic.scale.svg"))
draw(container,xlim = 6)
draws(rectangle(s.x=7,s.y=5,shd="line.12"),
      canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),
      canvas = FALSE)
dev.off()

svg(paste0(getwd(), "/young001_diff1.svg"))
draw(container,xlim = 6)
dev.off()

svg(paste0(getwd(), "/young001_diff2.svg"))
draw(container,xlim = 6)
for (i in seq(-6, 7, by = 3)) {
  for (j in seq(-3, 3, by = 3)) {
    draw(dot(pos.x = i, pos.y = j, size.x = 0.5, size.y = 0.5), canvas = F)
  }
}

dev.off()

svg(paste0(getwd(), "/young001_ic.flip.svg"))
draw(container,xlim = 6)
for(i in seq(-7, 7, by = 1)) {
  draw(vline(pos.x = i, s.x=5, lwd = 3), canvas = F)
}

for(i in seq(-5, 5, by = 1)) {
  draw(hline(pos.y = i, s.x=7, lwd = 3), canvas = F)
}

dev.off()
