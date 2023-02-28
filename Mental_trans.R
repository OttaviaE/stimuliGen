set.seed(999)
knitr::opts_chunk$set(echo=FALSE,  
                      eval=TRUE,  
                      warning = FALSE, 
                      message = FALSE, 
                      fig.align = "center")
knitr::knit_hooks$set(purl = knitr::hook_purl)

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
    theta.1= list(0),
    theta.2= list(0),
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

lilth<-lily()
s.lilth<-s.lily()
for(i in 1:length(lilth$shape)) {
  lilth$size.x[[i]] <-lilth$size.x[[i]]/2
  lilth$size.y[[i]] <-lilth$size.y[[i]]/2
  lilth$pos.y[[i]] <-lilth$pos.y[[i]]/2
  lilth$pos.x[[i]] <-lilth$pos.x[[i]]/2
  
}

s.lilth$size.x[[1]] <-s.lilth$size.x[[1]]/2
s.lilth$size.y[[1]] <-s.lilth$size.y[[1]]/2
s.lilth$pos.y[[1]] <-s.lilth$pos.y[[1]]/2
s.lilth$pos.x[[1]] <-s.lilth$pos.x[[1]]/2

papillon = bow.tie()
u.papillon = u.bow.tie()


for(i in 1:length(papillon$shape)) {
  papillon$size.x[[i]] <-papillon$size.x[[i]]/2
  papillon$size.y[[i]] <-papillon$size.y[[i]]/2
  papillon$pos.y[[i]] <-papillon$pos.y[[i]]/2
  papillon$pos.x[[i]] <-papillon$pos.x[[i]]/2
  
}

u.papillon$size.x[[1]] <-u.papillon$size.x[[1]]/2
u.papillon$size.y[[1]] <-u.papillon$size.y[[1]]/2
u.papillon$pos.y[[1]] <-u.papillon$pos.y[[1]]/2
u.papillon$pos.x[[1]] <-u.papillon$pos.x[[1]]/2


thepie = pie.4()
u.thepie = u.pie.4()


for(i in 1:length(thepie$shape)) {
  thepie$size.x[[i]] <-thepie$size.x[[i]]/2
  thepie$size.y[[i]] <-thepie$size.y[[i]]/2
  thepie$pos.y[[i]] <-thepie$pos.y[[i]]/2
  thepie$pos.x[[i]] <-thepie$pos.x[[i]]/2
  
}

u.thepie$size.x[[1]] <-u.thepie$size.x[[1]]/2
u.thepie$size.y[[1]] <-u.thepie$size.y[[1]]/2
u.thepie$pos.y[[1]] <-u.thepie$pos.y[[1]]/2
u.thepie$pos.x[[1]] <-u.thepie$pos.x[[1]]/2

biscuit = star()
u.biscuit = u.star()


for(i in 1:length(biscuit$shape)) {
  biscuit$size.x[[i]] <-biscuit$size.x[[i]]/2
  biscuit$size.y[[i]] <-biscuit$size.y[[i]]/2
  biscuit$pos.y[[i]] <-biscuit$pos.y[[i]]/2
  biscuit$pos.x[[i]] <-biscuit$pos.x[[i]]/2
  
}

u.biscuit$size.x[[1]] <-u.biscuit$size.x[[1]]/2
u.biscuit$size.y[[1]] <-u.biscuit$size.y[[1]]/2
u.biscuit$pos.y[[1]] <-u.biscuit$pos.y[[1]]/2
u.biscuit$pos.x[[1]] <-u.biscuit$pos.x[[1]]/2


square4bis <- function() {
  value <-cof(hline(pos.y=-11),vline(pos.x=11),
              hline(pos.y=11),vline(pos.x=-11))
  value$tag <- list("compose4")
  attr(value, "class") <- "field"
  value
}


smallbow.tie.inv <- function(pos.x = 0,pos.y=0,shd=NA) {
  value <-cof(triangle(pos.x = pos.x+5, pos.y = pos.y, rot=pi/3, 
                       s.x = 5, s.y=5,shd = shd), 
              triangle(pos.x = pos.x-5, pos.y = pos.y, rot=-pi, 
                       s.x = 5, s.y=5,shd = shd))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "field"
  value
}


u.smallbow.tie.inv <- function(pos.x = 0,pos.y=0,shd=NA) {
  value <-cof(triangle(pos.x = pos.x+5, pos.y = pos.y, rot=pi/3, 
                       s.x = 5, s.y=5,shd = shd), 
              triangle(pos.x = pos.x-5, pos.y = pos.y, rot=-pi, 
                       s.x = 5, s.y=5,shd = shd), 
              single = T, name = "u.smallbowtie.inv")
  value$tag <- list("compose2","fill", "rotate")
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



blu = "deepskyblue3"

giallo = "gold"

rosso = "firebrick"

#Visuo 1 ----

a1_visuo1c<-apply(Raven(cof(ellipse(s.x = 5,s.y = 4),
                            luck(s.x = 5,s.y = 4)),
                        c("trans.fill.inv")))

a1_visuo1a<-apply(Raven(cof(e.hexagon(s.x = 17, s.y = 17), 
                            
                            pentagon(s.x = 17, s.y = 17), 
                            square(s.x = 17, s.y = 17)),
                        c("diff_shapes"),
                        c("diff_shapes.inv")))

a1_visuo1b<-apply(Raven(u.pie.2.inv(size.x = 9), 
                        hrule = "rotation", 
                        vrule = "rotation.inv"))


a1_visuo1 = com(a1_visuo1a, a1_visuo1b, a1_visuo1c)
draw(a1_visuo1, hide = F)
