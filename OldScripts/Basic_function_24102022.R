## Definizione degli oggetti campo (field) e matrice (Raven)

field <- list(
  shape = NULL,
  size.x = list(),
  size.y = list(),
  rotation = list(),
  pos.x = list(),
  pos.y = list(),
  lty =list(),
  lwd = list(),
  num = list(),
  nv = list(),
  shade =list(),  
  visible = NULL, 
  tag = list()
)

Raven <- list(
  Sq1 = list(),
  Sq2 = list(),
  Sq3 = list(),
  
  Sq4 = list(),
  Sq5 = list(),
  Sq6 = list(),
  
  Sq7 = list(),
  Sq8 = list(),
  Sq9 = list(),
  hrule = list(),
  vrule = list()
)

class(field) <- "field"
class(Raven) <- "Raven_matrix"

#' Field function
#'
#' Function for the definition of a generic field
#' @param shapes
#' @param x
#' @param y
#' @param rot
#' @param vis
#' @return Return a field object
#' @examples
#'
#' @export
field <-
  function(shapes,
           sz.x,
           sz.y,
           rot,
           p.x,
           p.y,
           lty,
           lwd,
           num,
           nvert,
           shd,
           vis) {
    value <- list(
      shape = shapes,
      size.x = sz.x,
      size.y = sz.y,
      rotation = rot,
      pos.x = p.x,
      pos.y = p.y,
      lty = lty,
      lwd = lwd,
      num = num,
      nv = nvert,
      shade =shd,
      visible = vis
    )
    attr(value, "class") <- "field"
    value
  }

#' Matrix Raven function
#'
#' Function for the definition of a generic Raven matrix
#' @param st1 Fields in the top left square
#' @param hrule Rules applied with an horizontal logic
#' @param vrule Rules applied with an vertical logic
#' @return Return a Raven matrix object
#' @examples
#'
#' @export
Raven <- function(st1, hrule = "identity", vrule = "identity") {
  value <- list()
  squares <- paste0("Sq", 1:9)
  for (i in 1:length(squares))
  {
    value[[squares[i]]] <- st1 #Copy the same field in all the cells
  }
  value$hrule <- hrule
  value$vrule <- vrule
  attr(value, "class") <- "Raven_matrix"
  value
}

#' Default circle
#' @param vis Binary index that define if the shapes should be plot
#' @return Return the default circle object
#' @examples
#' circle()
#' @export
circle <- function(vis = 1) {
  value <- list(
    shape = "circle",
    size.x = 10,
    size.y = 10,
    rotation = 0,
    pos.x = 0,
    pos.y = 0,
    lty = 1,
    lwd = 2,
    num = 1,
    nv = 100,
    shade = NULL,
    visible = vis
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
triangle <- function(vis = 1) {
  value <- list(
    shape = "triangle",
    size.x = 15,
    size.y = 15,
    rotation = pi / 6,
    pos.x = 0,
    pos.y = 0,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 3,
    shade = NULL,
    visible = vis
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
square <- function(vis = 1) {
  value <- list(
    shape = "square",
    size.x = 15,
    size.y = 15,
    rotation = pi / 4,
    pos.x = 0,
    pos.y = 0,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 4,
    shade = NULL,
    visible = vis
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
pentagon <- function(vis = 1) {
  value <- list(
    shape = "pentagon",
    size.x = 15,
    size.y = 15,
    rotation = pi / 2,
    pos.x = 0,
    pos.y = 0,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 5,
    shade = NULL,
    visible = vis
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
hexagon <- function(vis = 1) {
  value <- list(
    shape = "hexagon",
    size.x = 15,
    size.y = 15,
    rotation = 0,
    pos.x = 0,
    pos.y = 0,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 6,
    shade = SetAlpha("black", alpha = 1),
    visible = vis
  )
  attr(value, "class") <- "field"
  value
}


#' Rotated hexagon
#'
#' @return Return the rotated hexagon object
#' @examples
#' rot.hexagon()
#' @export
rot.hexagon <- function(vis = 1) {
  value <- list(
    shape = "rot.hexagon",
    size.x = 15,
    size.y = 15,
    rotation = 3*pi/2,
    pos.x = 0,
    pos.y = 0,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 6,
    shade = SetAlpha("black", alpha = 1),
    visible = vis
  )
  attr(value, "class") <- "field"
  value
}

#' Default star 
#'
#' @return Return the default star object
#' @examples
#' star()
#' @export
star <- function() {
  value <-cof(hexagon(),rot.hexagon())
  attr(value, "class") <- "field"
  value
}

#' Vertical arc left up
#'
#' @return Return the vertical arc left up object
#' @examples
#' v.arc.left.up()
#' @export
v.arc.left.up <- function(vis = 1) {
  value <- list(
    shape = "v.arc.left.up",
    size.x = square()$size.x/2,
    size.y = square()$size.y/2,
    theta.1 = 3*pi/4,
    theta.2 = 5*pi/4,
    pos.x = 5,
    pos.y = 5,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 100, # non cambia nulla che sia indicato o meno
    shade = NULL,
    visible = vis
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
    size.x = square()$size.x/2,
    size.y = square()$size.y/2,
    theta.1 = 7*pi/4,
    theta.2 = pi/4,
    pos.x = -5.5,
    pos.y = 5,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 100, # non cambia nulla che sia indicato o meno
    shade = NULL,
    visible = vis
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
v.arc.left.down <- function(vis = 1) {
  value <- list(
    shape = "v.arc.left.down",
    size.x = square()$size.x/2,
    size.y = square()$size.y/2,
    theta.1 = 3*pi/4,
    theta.2 = 5*pi/4,
    pos.x = 5.1,
    pos.y = -5.5,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 100, # non cambia nulla che sia indicato o meno
    shade = NULL,
    visible = vis
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
    size.x = square()$size.x/2,
    size.y = square()$size.y/2,
    theta.1 = 7*pi/4,
    theta.2 = pi/4,
    pos.x = -5.5,
    pos.y = -5.5,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 100, # non cambia nulla che sia indicato o meno
    shade = NULL,
    visible = vis
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
    shape = "h.arc.left.up",
    size.x = square()$size.x/2,
    size.y = square()$size.y/2,
    theta.1 = pi/4,
    theta.2 = 3*pi/4,
    pos.x = -5.5,
    pos.y = -5.5,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 100, 
    shade = NULL,
    visible = vis
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
    shape = "h.arc.right.up",
    size.x = square()$size.x/2,
    size.y = square()$size.y/2,
    theta.1 = pi/4,
    theta.2 = 3*pi/4,
    pos.x = 5.1,
    pos.y = -5.5,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 100, 
    shade = NULL,
    visible = vis
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
    size.x = square()$size.x/2,
    size.y = square()$size.y/2,
    theta.1 = 5*pi/4,
    theta.2 = 7*pi/4,
    pos.x = -5.5,
    pos.y = 5,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 100, # non cambia nulla che sia indicato o meno
    shade = NULL,
    visible = vis
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
    size.x = square()$size.x/2,
    size.y = square()$size.y/2,
    theta.1 = 5*pi/4,
    theta.2 = 7*pi/4,
    pos.x = 5,
    pos.y = 5,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 100, # non cambia nulla che sia indicato o meno
    shade = NULL,
    visible = vis
  )
  attr(value, "class") <- "field"
  value
}


#' Default S vertical 
#'
#' @return Return the default S vertical object
#' @examples
#' s.vertical()
#' @export
s.vertical <- function() {
  value <-cof( v.arc.left.up(), v.arc.right.down())
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
  attr(value, "class") <- "field"
  value
}

#' Default vertical eight
#'
#' @return Return the default vertical eight object
#' @examples
#' vertical.eight()
#' @export
vertical.eight <- function() {
  value <-cof( s.vertical(), s.vertical.inv())
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
  attr(value, "class") <- "field"
  value
}

#' Default horizontal eight
#'
#' @return Return the default horizontal eight object
#' @examples
#' horizontal.eight()
#' @export
horizontal.eight <- function() {
  value <-cof( s.horizontal(), s.horizontal.inv())
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
  attr(value, "class") <- "field"
  value
}

lily.logic <- function(vis = 1) {
  value <- list(
    shape = "lily.logic",
    size.x = rep(square()$size.x/2, 2),
    size.y = rep(square()$size.x/2, 2),
    theta.1 = c(5*pi/4,  7*pi/4),
    theta.2 = c(7*pi/4, pi/4),
    pos.x = c(5,-5.5),
    pos.y =c(5, 5),
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 100, # non cambia nulla che sia indicato o meno
    shade = NULL,
    visible = vis
  )
  attr(value, "class") <- "field"
  value
}




#' Default vertical line 
#'
#' @return Return the default vline object
#' @examples
#' vline()
#' @export
vline <- function(vis = 1) {
  value <- list(
    shape = "vline",
    size.x = 10,
    size.y = 10,
    rotation = pi + pi / 2,
    pos.x = 0,
    pos.y = 0,
    lty = 1,
    lwd = 3,
    num = 1,
    nv =  2,
    shade = NULL,
    visible = vis
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
hline <- function(vis = 1) {
  value <- list(
    shape = "hline",
    size.x = 10,
    size.y = 10,
    rotation = pi ,
    pos.x = 0,
    pos.y = 0,
    lty = 1,
    lwd = 3,
    num = 1,
    nv =  2,
    shade = NULL,
    visible = vis
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
  value <-cof(vline(),hline())
  attr(value, "class") <- "field"
  value
}

#' Default dice NON FUNZIONA CON IL NUOVO CODICE LA NUMEROSITA è 
#' UN PROBLEMA
#'
#' @return Return the default dice object
#' @examples
#' dice()
#' @export
dice <- function(vis = 1) {
  value <- list(
    shape = "dice",
    size.x = 3,
    size.y = 3,
    rotation = pi,
    pos.x = 14,
    pos.y = 13,
    lty = 1,
    lwd = 3,
    num = 4,
    nv = 100,
    shade = NULL,
    visible = vis
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
ellipse <- function(vis = 1) {
  value <- list(
    shape = "ellipse",
    size.x = 10,
    size.y = 7,
    rotation = 0,
    pos.x = 0,
    pos.y = 0,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 100,
    shade = NULL,
    visible = vis
  )
  attr(value, "class") <- "field"
  value
}

#' Default small triangle
#'
#' @return Return the default small triangle object
#' @examples
#' small_triangle()
#' @export
small_triangle <- function(vis = 1) {
  value <- list(
    shape = "striangle",
    size.x = 3,
    size.y = 3,
    rotation = 0,
    pos.x = 18,
    pos.y = 13,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 3,
    shade = NULL,
    visible = vis
  )
  attr(value, "class") <- "field"
  value
}

#' Concatenationof field
#'
#' Function for the concatenation of difference fields
#' @return Return a field object
#' @examples
#'
#' @export
cof <- function(...) {
  UseMethod("cof")
}

cof.field <- function(...) {
  obj <- Map("c", ...)
  attr(obj, "class") <- "field"
  obj
}

#Per generalizzare la funzione per concatenzione di materici ho definito il metodo cof anche per
#i caratteri
cof.character <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "character"
  obj
}


#' Concatenationof matrix
#'
#' Function for the concatenation of difference matrices
#' @return Return a Matrix object
#' @examples
#'
#' @export
com <- function(...) {
  UseMethod("com")
}

com.Raven_matrix <- function(...) {
  obj <- Map("cof", ...)
  attr(obj, "class") <- "Raven_matrix"
  obj
}
