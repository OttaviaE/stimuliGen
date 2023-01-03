rm(list = ls())
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")

empty <- function() {
  value <- list(
    shape = "empty",
    size.x = list(5),
    size.y = list(5),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(pi),
    pos.x = list(0),
    pos.y = list(0),
    lty = list(0),
    lwd = list(1),
    num = list(1),
    nv = list(101),
    shade = list(NA),
    visible = 0,
    tag=list(c('simple'))
  )
  attr(value, "class") <- "field"
  value
}

bow.tie.inv <- function(pos.x = 0) {
  value <-cof(triangle(pos.x = pos.x+10, pos.y = pos.x, rot=pi/3, 
                       s.x = 10, s.y=10), 
              triangle(pos.x = pos.x-10, pos.y = pos.x, rot=-pi, 
                       s.x = 10, s.y=10))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "field"
  value
}

adult013a<-apply(Raven(cof(empty(),cross(),X()),"diff_shapes"))
adult013b<-apply(Raven(cof(pentagon(s.x=15, s.y=15),e.hexagon(s.x=14, s.y=14),
                           square(s.x=15, s.y=15)),vrule = "diff_shapes"))
adult013<-com(adult013a,adult013b)
draw(adult013)