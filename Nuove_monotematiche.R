source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")
source("monotematiche_rule.R")
blu = "deepskyblue3"

giallo = "gold"

rosso = "firebrick"

a=5.2 #proporzione dei canvas
spessore= 9 # spessore linee 

library(DescTools)

sequence(cof(circle(s.x=5,s.y=5),circle(s.x=3,s.y=3,shd = giallo),single = TRUE,name="d"),
         canvas = TRUE,by.x=10,by.y=10,bg=blu,movement=c("xy"))
clip(6,18,-18,-6)
draw(rectangle(s.x=20,s.y=20,shd="white",pos.x=12,pos.y=-12,lty=0),
     canvas = FALSE)


##corretta ---
Canvas(xlim =c(6,18),ylim=c(-18,-6), pty="s")
sequence(cof(circle(s.x=5,s.y=5),circle(s.x=3,s.y=3,shd = giallo),single = TRUE,name="d"),
         canvas = FALSE,by.x=10,by.y=10,bg=blu,movement=c("xy"))

## ic-neg---
Canvas(xlim =c(6,18),ylim=c(-18,-6), pty="s")
sequence(cof(circle(s.x=5,s.y=5),circle(s.x=3,s.y=3,shd = blu),single = TRUE,name="d"),
         canvas = FALSE,by.x=10,by.y=10,bg=giallo,movement=c("xy"))

## wp---
Canvas(xlim =c(6,18),ylim=c(-18,-6), pty="s")
sequence(cof(circle(s.x=1.5,s.y=1.5),circle(s.x=.9,s.y=.9,shd = giallo),single = TRUE,name="d"),
         canvas = FALSE,by.x=3,by.y=3,bg=blu,movement=c("xy"))

## diff---
Canvas(xlim =c(6,18),ylim=c(-18,-6), pty="s")
draw(rectangle(s.x=20,s.y=20,shd=blu,pos.x=12,pos.y=-12,lty=0),canvas = FALSE)
