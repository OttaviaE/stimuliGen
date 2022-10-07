## Definizione degli oggetti campo (field) e matrice (Raven)

field <- list(
  shape = NULL,
  size.x = NULL,
  size.y = NULL,
  rotation = NULL,
  pos.x = NULL,
  pos.y = NULL,
  num = NULL,
  visible= NULL
  )

Raven<-list(
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
field <- function(shapes,sz.x,sz.y,p.x,p.y,rot,num,vis) {
  value <- list(
    shape = shapes,
    size.x = sz.x,
    size.y = sz.y,
    rotation = rot,
    pos.x = p.x, 
    pos.y = p.y,
    num = num,
    visible=vis
  )
  attr(value, "class") <- "field"
  value
}

#' Matrix Raven function
#'
#' Function for the definition of a generic Raven matrix
#' @param st1
#' @param hrule
#' @param vrule
#' @return Return a Raven matrix object 
#' @examples
#'
#' @export
Raven <- function(st1,hrule="identity",vrule="identity") {
  value<-list()
  squares<-paste0("Sq",1:9)
  for(i in 1:length(squares))
  {
    value[[squares[i]]]<-st1
  }
  value$hrule<-hrule
  value$vrule<-vrule
  attr(value, "class") <- "Raven_matrix"
  value
}

#' Default circle
#'
#' @return Return the default circle object 
#' @examples
#' circle()
#' @export
circle <- function() {
  value <- list(
    shape = "elipse",
    size.x = 10,
    size.y = 10,
    rotation = 0,
    pos.x = 0, 
    pos.y = 0,
    num = 1,
    visible=1
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
triangle <- function() {
  value <- list(
    shape = "triangle",
    size.x = 15,
    size.y = 15,
    rotation = pi/6,
    pos.x = 0, 
    pos.y = 0,
    num = 1,
    visible=1
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
square <- function() {
  value <- list(
    shape = "square",
    size.x = 15,
    size.y = 15,
    rotation = pi/4,
    pos.x = 0, 
    pos.y = 0,
    num = 1,
    visible=1
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
  value <- list(
    shape = "cross",
    size.x = 10,
    size.y = 10,
    rotation = pi,
    pos.x = 0, 
    pos.y = 0,
    num = 1,
    visible=1
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
  value <- list(
    shape = "dice",
    size.x = 3,
    size.y = 3,
    rotation = pi,
    pos.x = 14, 
    pos.y = 13,
    num = 4,
    visible=1
  )
  attr(value, "class") <- "field"
  value
}


#' Default elipse
#'
#' @return Return the default elipse object 
#' @examples
#' elipse()
#' @export
elipse <- function() {
  value <- list(
    shape = "elipse",
    size.x = 10,
    size.y = 7,
    rotation = 0,
    pos.x = 0, 
    pos.y = 0,
    num = 1,
    visible=1
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
small_triangle <- function() {
  value <- list(
    shape = "striangle",
    size.x = 3,
    size.y = 3,
    rotation = 0,
    pos.x = 14, 
    pos.y = 13,
    num = 1,
    visible=1
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

cof.field<-function(...) {
  obj<-Map("c",...)
  attr(obj, "class") <- "field"
  obj
}
