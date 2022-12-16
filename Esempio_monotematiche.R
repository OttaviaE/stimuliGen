
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")

rectangle <- function(s.x=15,
                       s.y=20,
                       rot=pi / 4, pos.x = 0, pos.y = 0,
                       shd=NA, lwd = 3, lty = 1, 
                       vis = 1) {
  value <- list(
    shape = "rectangle",
    size.x = list(s.x),
    size.y = list(s.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lwd),
    lwd = list(lty),
    num = list(1),
    nv = list(101),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple', 'small','rotate'))
  )
  attr(value, "class") <- "field"
  value
}


# disegni matrici uniche 
Canvas()

# vertical canvas
draw(vline(pos.x=-30, s.x = 30, lwd = 5))
for(i in seq(-30, 30, by = 1)) {
  draw(vline(pos.x = i, s.x=40, lwd = 5), 
       canvas = F)
}
draw(rectangle(s.x=7,s.y=5,shd="white",pos.x=+8,pos.y=-7),canvas = FALSE)
