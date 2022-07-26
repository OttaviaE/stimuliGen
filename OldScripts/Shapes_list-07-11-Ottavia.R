#################################################################
########## POLIGONI -------
###############################################################


#' Default circle
#' @param vis Binary index that define if the shapes should be plot
#' @return Return the default circle object
#' @examples
#' circle()
#' @export
circle <- function(vis = 1) {
  value <- list(
    shape = "circle",
    size.x = list(10),
    size.y = list(10),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(0),
    pos.x = list(0),
    pos.y = list(0),
    lty = list(1),
    lwd = list(2),
    num = list(1),
    nv = list(100),
    shade = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}

#' Default ellipse
#'
#' @return Return the default ellipse object
#' @examples
#' ellipse()
#' @export
ellipse <- function(s.x=10,
                    s.y=7,
                    rot=0,
                    shd=NA,
                    vis = 1) {
  value <- list(
    shape = "ellipse",
    size.x = list(s.x),
    size.y = list(s.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(0),
    pos.y = list(0),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv = list(100),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}

#' Default triangle
#'
#' @return Return the default triangle object
#' @examples
#' triangle()
#' @export
triangle <- function(s.x=15,
                     s.y=15,
                     pos.x = 0, 
                     pos.y = 0, 
                     rot=pi / 6,
                     shd=NA,
                     vis = 1) {
  value <- list(
    shape = "triangle",
    size.x = list(s.x),
    size.y = list(s.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv = list(3),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}


#' Default square
#'
#' @return Return the default square object
#' @examples
#' square()
#' @export
square <- function(    s.x=15,
                       s.y=15,
                       rot=pi / 4,
                       shd=NA,
                       vis = 1) {
  value <- list(
    shape = "square",
    size.x = list(s.x),
    size.y = list(s.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(0),
    pos.y = list(0),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv = list(4),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}

#' Default pentagon
#'
#' @return Return the default pentagon object
#' @examples
#' pentagon()
#' @export
pentagon <- function(s.x=15,
                     s.y=15,
                     rot=pi / 2,
                     shd=NA,
                     vis = 1) {
  value <- list(
    shape = "pentagon",
    size.x = list(s.x),
    size.y = list(s.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(0),
    pos.y = list(0),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv = list(5),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}


#' Default empty hexagon
#'
#' @return Return the default empty hexagon object
#' @examples
#' e.hexagon()
#' @export
e.hexagon <- function(s.x=15,
                      s.y=15,
                      rot=0,
                      shd=SetAlpha("black", alpha = 0),
                      vis = 1) {
  value <- list(
    shape = "e.hexagon",
    size.x = list(s.x),
    size.y = list(s.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(0),
    pos.y = list(0),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv = list(6),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple', 'small', 'rotate'))
  )
  attr(value, "class") <- "field"
  value
}

#' Default hexagon
#'
#' @return Return the default hexagon object
#' @examples
#' hexagon()
#' @export
hexagon <- function(s.x=15,
                    s.y=15,
                    rot=0,
                    #shd=SetAlpha("black", alpha = 1),
                    shd="black",
                    vis = 1) {
  value <- list(
    shape = "hexagon",
    size.x = list(s.x),
    size.y = list(s.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(0),
    pos.y = list(0),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv = list(6),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple', 'small', 'fill'))
  )
  attr(value, "class") <- "field"
  value
}


#' Default rot.hexagon
#'
#' @return Return the default rot.hexagon object
#' @examples
#' rot.hexagon()
#' @export
rot.hexagon <- function(s.x=15,
                        s.y=15,
                        rot=3*pi/2,
                        #shd=SetAlpha("black", alpha = 1),
                        shd="black",
                        vis = 1) {
  value <- list(
    shape = "rot.hexagon",
    size.x = list(s.x),
    size.y = list(s.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(0),
    pos.y = list(0),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv = list(6),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}


#' Default star
#'
#' @return Return the star object
#' @examples
#' star()
#' @export
star <- function() {
  value <-cof(hexagon(),rot.hexagon())
  value$tag <- list("small", "compose2","fill")
  attr(value, "class") <- "field"
  value
}

#' Default star (unique)
#'
#' @return Return the star object (seen as a unique object)
#' @examples
#' u.star()
#' @export
u.star <- function() {
  value <-cof(hexagon(),rot.hexagon(), single = T, name = "u.star")
  value$tag <- list("small", "simple","fill")
  attr(value, "class") <- "field"
  value
}




#' bow tie
#'
#' @return Return the default cross object
#' @examples
#' bow.tie()
#' @export
bow.tie <- function() {
  value <-cof(triangle(pos.x = 0, pos.y = 10, rot=pi/6, 
                       s.x = 10, s.y=10), 
              triangle(pos.x = 0, pos.y = -10, rot=pi/2, 
                       s.x = 10, s.y=10))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "field"
  value
}

#################################################################
#### ARCHI DI CERCHIO -----
###############################################################


#' Vertical arc left up
#'
#' @return Return the vertical arc left up object
#' @examples
#' v.arc.left.up()
#' @export
v.arc.left.up <- function(vis = 1) {
  value <- list(
    shape = "v.arc.left.up",
    size.x  = list(square()$size.x[[1]]/2),
    size.y  = list(square()$size.y[[1]]/2),
    theta.1  = list(3*pi/4),
    theta.2  = list(5*pi/4),
    rotation = list(pi),
    pos.x  = list(5),
    pos.y  = list(5),
    lty  = list(1),
    lwd  = list(3),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}

#' Vertical arc right up
#'
#' @return Return the vertical arc right up object
#' @examples
#' v.arc.right.up()
#' @export
v.arc.right.up <- function(vis = 1) {
  value <- list(
    shape = "v.arc.right.up",
    size.x  = list(square()$size.x[[1]]/2),
    size.y  = list(square()$size.y[[1]]/2),
    theta.1  = list(7*pi/4),
    theta.2  = list(pi/4),
    rotation = list(pi),
    pos.x  = list(-5.5),
    pos.y  = list(5),
    lty  = list(1),
    lwd  = list(3),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}


#' Vertical arc left down
#'
#' @return Return the vertical arc left down object
#' @examples
#' v.arc.left.down()
#' @export
v.arc.left.down <- function(vis  = 1) {
  value <- list(
    shape  = "v.arc.left.down",
    size.x  = list(square()$size.x[[1]]/2),
    size.y  = list(square()$size.y[[1]]/2),
    theta.1  = list(3*pi/4),
    theta.2  = list(5*pi/4),
    rotation = list(pi),
    pos.x  = list(5.1),
    pos.y  = list(-5.5),
    lty  = list(1),
    lwd  = list(3),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}


#' Vertical arc right down
#'
#' @return Return the vertical arc right down object
#' @examples
#' v.arc.right.down()
#' @export
v.arc.right.down <- function(vis = 1) {
  value <- list(
    shape = "v.arc.right.down",
    size.x  = list(square()$size.x[[1]]/2),
    size.y  = list(square()$size.y[[1]]/2),
    theta.1  = list(7*pi/4),
    theta.2  = list(pi/4),
    rotation = list(pi),
    pos.x  = list(-5.5),
    pos.y  = list(-5.5),
    lty  = list(1),
    lwd  = list(3),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}


#' Horizontal arc left up
#'
#' @return Return the horizontal arc left up object
#' @examples
#' h.arc.left.up()
#' @export
h.arc.left.up <- function(vis = 1) {
  value <- list(
    shape  = "h.arc.left.up",
    size.x  = list(square()$size.x[[1]]/2),
    size.y  = list(square()$size.y[[1]]/2),
    theta.1  = list(pi/4),
    theta.2  = list(3*pi/4),
    rotation = list(pi),
    pos.x  = list(-5.5),
    pos.y  = list(-5.5),
    lty  = list(1),
    lwd  = list(3),
    num  = list(2),
    nv  = list(100), 
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}

#' Horizontal arc right up
#'
#' @return Return the horizontal arc right up object
#' @examples
#' h.arc.right.up()
#' @export
h.arc.right.up <- function(vis = 1) {
  value <- list(
    shape  = "h.arc.right.up",
    size.x  = list(square()$size.x[[1]]/2),
    size.y  = list(square()$size.y[[1]]/2),
    theta.1  = list(pi/4),
    theta.2  = list(3*pi/4),
    rotation = list(pi),
    pos.x  = list(5.1),
    pos.y  = list(-5.5),
    lty  = list(1),
    lwd  = list(3),
    num  = list(2),
    nv  = list(100), 
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}


#' Horizontal arc left down
#'
#' @return Return the horizontal arc left down object
#' @examples
#' h.arc.left.down()
#' @export
h.arc.left.down <- function(vis = 1) {
  value <- list(
    shape = "h.arc.left.down",
    size.x  = list(square()$size.x[[1]]/2),
    size.y  = list(square()$size.y[[1]]/2),
    theta.1  = list(5*pi/4),
    theta.2  = list(7*pi/4),
    rotation = list(pi),
    pos.x  = list(-5.5),
    pos.y  = list(5),
    lty  = list(1),
    lwd  = list(3),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}

#' Horizontal arc right down
#'
#' @return Return the horizontal arc right down object
#' @examples
#' h.arc.right.down()
#' @export
h.arc.right.down <- function(vis = 1) {
  value <- list(
    shape = "h.arc.right.down",
    size.x  = list(square()$size.x[[1]]/2),
    size.y  = list(square()$size.y[[1]]/2),
    theta.1  = list(5*pi/4),
    theta.2  = list(7*pi/4),
    rotation = list(pi),
    pos.x  = list(5),
    pos.y  = list(5),
    lty  = list(1),
    lwd  = list(3),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "field"
  value
}


#################################################################
#### COMBINAZIONI DI ARCHI MULTI FORMA
###############################################################

#' Default S vertical 
#'
#' @return Return the default S vertical object
#' @examples
#' s.vertical()
#' @export
s.vertical <- function() {
  value <-cof( v.arc.left.up(), v.arc.right.down())
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "field"
  value
}

#' Default S vertical inverted
#'
#' @return Return the default S vertical inverted object
#' @examples
#' s.vertical.inv()
#' @export
s.vertical.inv <- function() {
  value <-cof( v.arc.right.up(), v.arc.left.down())
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "field"
  value
}


#' Default S horizontal 
#'
#' @return Return the default S horizontal object
#' @examples
#' s.horizontal()
#' @export
s.horizontal <- function() {
  value <-cof( h.arc.left.up(), h.arc.right.down())
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "field"
  value
}

#' Default S horizontal inverted
#'
#' @return Return the default S horizontal inverted object
#' @examples
#' s.horizontal.inv()
#' @export
s.horizontal.inv <- function() {
  value <-cof( h.arc.left.down(), h.arc.right.up())
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "field"
  value
}

#################################################################
#### COMBINAZIONI DI ARCHI FORMA SINGOLA
###############################################################

#' Default S vertical 
#'
#' @return Return the default S vertical object
#' @examples
#' s.vertical()
#' @export
s_vertical <- function() {
  value <-cof( v.arc.left.up(), v.arc.right.down(),single=TRUE,name="s.vertical")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "field"
  value
}

#' Default S vertical inverted
#'
#' @return Return the default S vertical inverted object
#' @examples
#' s.vertical.inv()
#' @export
s_vertical.inv <- function() {
  value <-cof( v.arc.right.up(), v.arc.left.down(),single=TRUE,
               name="s.vertical.inv")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "field"
  value
}


#' Default S horizontal 
#'
#' @return Return the default S horizontal object
#' @examples
#' s.horizontal()
#' @export
s_horizontal <- function() {
  value <-cof(h.arc.left.up(), h.arc.right.down(),single=TRUE,
              name="s.horizontal")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "field"
  value
}

#' Default S horizontal inverted
#'
#' @return Return the default S horizontal inverted object
#' @examples
#' s.horizontal.inv()
#' @export
s_horizontal.inv <- function() {
  value <-cof( h.arc.left.down(), h.arc.right.up(),single=TRUE,
               name="s.horizontal.inv")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "field"
  value
}

#################################################################
#### COMBINAZIONI DI S MULTI FORMA
###############################################################


#' Default vertical eight
#'
#' @return Return the default vertical eight object
#' @examples
#' vertical.eight()
#' @export
vertical.eight <- function() {
  value <-cof(  s_vertical(), s_vertical.inv())
  value$tag <- list("compose2")
  attr(value, "class") <- "field"
  value
}

#' Default horizontal eight
#'
#' @return Return the default horizontal eight object
#' @examples
#' vertical.eight()
#' @export
horizontal.eight <- function() {
  value <-cof(s_horizontal(), s_horizontal.inv())
  value$tag <- list("compose2")
  attr(value, "class") <- "field"
  value
}


#' Default lily
#'
#' @return Return the default lily object
#' @examples
#' lily()
#' @export
lily <- function() {
  value <-cof( horizontal.eight(), vertical.eight())
  value$tag <- list("compose4")
  attr(value, "class") <- "field"
  value
}

#################################################################
#### COMBINAZIONI DI S FORMA singola
###############################################################


#' Default vertical eight
#'
#' @return Return the default vertical eight object
#' @examples
#' vertical.eight()
#' @export
vertical_eight <- function() {
  value <-cof(  s_vertical(), s_vertical.inv(),single=TRUE,name="v.eigth")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "field"
  value
}

#' Default horizontal eight
#'
#' @return Return the default horizontal eight object
#' @examples
#' vertical.eight()
#' @export
horizontal_eight <- function() {
  value <-cof(s_horizontal(), s_horizontal.inv(),single=TRUE,name="h.eigth")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "field"
  value
}


#' Default lily
#'
#' @return Return the default lily object
#' @examples
#' lily()
#' @export
s.lily <- function() {
  value <-cof( horizontal_eight(), vertical_eight(),single=TRUE,name="single_lily")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "field"
  value
}

#############################################################
## FORME PUNTINATE
###############################################################

#' Default small black circle
#'
#' @return Return the default dice object
#' @examples
#' dot()
#' @export
dot <- function(x=0,y=0,
                size.x = 2, 
                size.y =2, 
                vis = 1) {
  value <- list(
    shape = "dot",
    size.x = list(size.x),
    size.y = list(size.x),
    theta.1  = list(5*pi/4),
    theta.2  = list(7*pi/4),
    rotation = list(pi),
    pos.x = list(x),
    pos.y = list(y),
    lty = list(1),
    lwd = list(1),
    num = list(1),
    nv = list(100),
    shade = list("black"),
    visible = vis,
    tag = list("simple","fill")
  )
  attr(value, "class") <- "field"
  value
}

#' Default dice
#'
#' @return Return the default dice object
#' @examples
#' dice()
#' @export
dice <- function() {
  value <-cof(dot(13,13),dot(-13,13),dot(13,-13),dot(-13,-13),
              single = TRUE,name = "dice")
  value$tag <- list("simple")
  attr(value, "class") <- "field"
  value
}

#' Default dice
#'
#' @return Return the default dice object
#' @examples
#' dice()
#' @export
cross.dice <- function() {
  value <-cof(dot(13,0),dot(-13,0),dot(0,-13),dot(0,13),
              single = TRUE,name = "cross.dice")
  value$tag <- list("simple")
  attr(value, "class") <- "field"
  value
}

##############################################################
######## SEGMENTI
##############################################################

#' Default vertical line 
#'
#' @return Return the default vline object
#' @examples
#' vline()
#' @export
vline <- function(p.x=0 ,p.y=0,vis = 1) {
  value <- list(
    shape = "vline",
    size.x = list(sqrt(square()$ size.x[[1]]^2 /2)),
    size.y = list(sqrt(square()$ size.y[[1]]^2 /2)),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(pi + pi / 2),
    pos.x = list(p.x),
    pos.y = list(p.y),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv =  list(2),
    shade = list(NA),
    visible = vis,
    tag = list("simple","fill" ) 
  )
  attr(value, "class") <- "field"
  value
}

#' Default horizontal line 
#'
#' @return Return the default hline object
#' @examples
#' hline()
#' @export
hline <- function(p.x=0 ,p.y=0, vis = 1) {
  value <- list(
    shape = "hline",
    size.x = list(sqrt(square()$ size.x[[1]]^2 /2)),
    size.y = list(sqrt(square()$ size.y[[1]]^2 /2)),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(pi),
    pos.x = list(p.x),
    pos.y = list(p.y),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv =  list(2),
    shade = list(NA),
    visible = vis,
    tag = list("simple","fill" ) 
  )
  attr(value, "class") <- "field"
  value
}

#' Default cross 
#'
#' @return Return the default cross object
#' @examples
#' cross()
#' @export
cross <- function() {
  value <-cof(vline(),hline(),single = TRUE,name = "cross")
  value$tag <- list("simple","fill")
  value$visible<-1
  attr(value, "class") <- "field"
  value
}

#' Default square 
#'
#' @return Return the default cross object
#' @examples
#' cross()
#' @export
square4 <- function() {
  value <-cof(vline(p.x=-8,),vline(p.x=8),hline(p.y=-8,),hline(p.y=8))
  value$tag <- list("compose4")
  attr(value, "class") <- "field"
  value
}


#' Default inverse diagonal line
#'
#' @return Return the default diag line object
#' @examples
#' diagline.inv()
#' @export
diagline.inv <- function(p.x=0 ,p.y=0,vis = 1) {
  value <- list(
    shape = "diagline.inv",
    size.x = list(sqrt(square()$ size.x[[1]]^2 /2)),
    size.y = list(sqrt(square()$ size.y[[1]]^2 /2)),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(pi + pi / 4),
    pos.x = list(p.x),
    pos.y = list(p.y),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv =  list(2),
    shade = list(NA),
    visible = vis,
    tag = list("simple","fill" ) 
  )
  attr(value, "class") <- "field"
  value
}

#' Default main diagonal line
#'
#' @return Return the default diagonal line object
#' @examples
#' diagline()
#' @export
diagline <- function(p.x=0 ,p.y=0,vis = 1) {
  value <- list(
    shape = "diagline.inv",
    size.x = list(sqrt(square()$ size.x[[1]]^2 /2)),
    size.y = list(sqrt(square()$ size.y[[1]]^2 /2)),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(pi - pi / 4),
    pos.x = list(p.x),
    pos.y = list(p.y),
    lty = list(1),
    lwd = list(3),
    num = list(1),
    nv =  list(2),
    shade = list(NA),
    visible = vis,
    tag = list("simple","fill" ) 
  )
  attr(value, "class") <- "field"
  value
}

#' Default cross 
#'
#' @return Return the default cross object
#' @examples
#' X()
#' @export
X <- function() {
  value <-cof(diagline(),diagline.inv(),single = TRUE,name = "X")
  value$tag <- list("simple","fill")
  value$visible<-1
  attr(value, "class") <- "field"
  value
}