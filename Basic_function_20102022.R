## Definizione degli oggetti campo (field) e matrice (Raven)

field <- list(
  shape = NULL,
  size.x = NULL,
  size.y = NULL,
  rotation = NULL,
  pos.x = NULL,
  pos.y = NULL,
  lty = NULL,
  lwd = NULL,
  num = NULL,
  nv = NULL,
  shade =NULL,  
  visible = NULL
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

#' Default small black circle
#'
#' @return Return the default dice object
#' @examples
#' dot()
#' @export
dot <- function(x=0,y=0,vis = 1) {
  value <- list(
    shape = "dice",
    size.x = 3,
    size.y = 3,
    rotation = pi,
    pos.x = x,
    pos.y = y,
    lty = 1,
    lwd = 1,
    num = 1,
    nv = 100,
    shade = "black",
    visible = vis
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
  value <-cof(dot(13,13),dot(-13,13),dot(13,-13),dot(-13,-13))
  attr(value, "class") <- "field"
  value
}


#' Default elipse
#'
#' @return Return the default elipse object
#' @examples
#' elipse()
#' @export
elipse <- function(vis = 1) {
  value <- list(
    shape = "elipse",
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
