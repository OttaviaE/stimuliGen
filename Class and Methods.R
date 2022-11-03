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


#########################################################
###  BASIC METHODS FOR THE FIELDS
##########################################################

#' Concatenation of list or vector
#'
#' Function for the concatenation of difference fields
#' @return Return a field object
#' @examples
#'
#' @export
concatenation <- function(...) {
  UseMethod("concatenation")
}

concatenation.list <- function(...) {
  obj <- Map("c", ...)  
  return(obj)
}


concatenation.double <- function(...) {
  obj <- c(...)  
  attr(obj, "class") <- "double"
  obj
}

concatenation.character <- function(...) {
  obj <- c(...)  
  return(obj)
}

concatenation.integer <- function(...) {
  obj <- c(...)  
 return(obj)
}
  
#' Concatenation of field
#'
#' Function for the concatenation of difference fields
#' @return Return a field object
#' @examples
#'
#' @export
cof <- function(...,name, single) {
  UseMethod("cof")
}

cof.field <- function( ...,name=NULL, single=FALSE) {
  if(single==TRUE)
  {
    obj <- Map("concatenation", ...)
    obj$shape<-name
    obj$visible<-1
  }else{
    obj <- Map("c", ...)  
  }
  
  
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

#########################################################
###  BASIC METHODS FOR THE Raven
##########################################################


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

apply <- function(obj,rules) {
  UseMethod("apply")
}

draw <- function(obj) {
  UseMethod("draw")
}


apply.Raven_matrix <- function(obj,rules="HV") {
  # The rules are applied by row keeping fixed the row by means of the three vectors
  hrules <- obj$hrule
  row_1 <- paste0("Sq", 1:3)
  row_2 <- paste0("Sq", 4:6)
  row_3 <- paste0("Sq", 7:9)
  
  # The rules are applied by column keeping fixed the row by means of the three vectors
  vrules <- obj$vrule
  col_1 <- paste0("Sq", seq(1, 9, 3))
  col_2 <- paste0("Sq", seq(2, 9, 3))
  col_3 <- paste0("Sq", seq(3, 9, 3))
  
  #This table contains in the first row the label of the rules 
  #and in the second row the function associated
  function_list <- read.csv("function_list.prn", sep="")
  #applying the horizontal rules

  if(rules=="HV")
  {
    for (r in 1:length(hrules))
    {
      f<-get(function_list$function.[function_list$label==hrules[r]])
      for (i in 1:3)
      {
        obj[[row_1[i]]] <- f(obj[[row_1[i]]],i,hrules[r],seed=1)
        obj[[row_2[i]]] <- f(obj[[row_2[i]]],i,hrules[r],seed=5)
        obj[[row_3[i]]] <- f(obj[[row_3[i]]],i,hrules[r],seed=6)
      }
    }
    
    #applying the vertical rules
    for (r in 1:length(vrules))
    {
      f<-get(function_list$function.[function_list$label==vrules[r]])
      for (i in 1:3)
      {
        obj[[col_1[i]]] <- f(obj[[col_1[i]]],i,vrules[r],seed=1)
        obj[[col_2[i]]] <- f(obj[[col_2[i]]],i,vrules[r],seed=5)
        obj[[col_3[i]]] <- f(obj[[col_3[i]]],i,vrules[r],seed=6)
      }
    }
  }else{
    #applying the vertical rules
    for (r in 1:length(vrules))
    {
      f<-get(function_list$function.[function_list$label==vrules[r]])
      for (i in 1:3)
      {
        obj[[col_1[i]]] <- f(obj[[col_1[i]]],i,vrules[r],seed=1)
        obj[[col_2[i]]] <- f(obj[[col_2[i]]],i,vrules[r],seed=5)
        obj[[col_3[i]]] <- f(obj[[col_3[i]]],i,vrules[r],seed=6)
      }
    }
    
    for (r in 1:length(hrules))
    {
      f<-get(function_list$function.[function_list$label==hrules[r]])
      for (i in 1:3)
      {
        obj[[row_1[i]]] <- f(obj[[row_1[i]]],i,hrules[r],seed=1)
        obj[[row_2[i]]] <- f(obj[[row_2[i]]],i,hrules[r],seed=5)
        obj[[row_3[i]]] <- f(obj[[row_3[i]]],i,hrules[r],seed=6)
      }
    }
    
    
  }
  
  attr(obj, "class") <- "Raven_matrix"
  return(obj)
}


draw.Raven_matrix<- function(obj, main = NULL) { ###Definito Draw per i field si può semplificare questa
 
  library(DescTools)
  par(mfrow = c(3, 3), mar = c(0.5, 6, 0.5, 2) + .1, mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )
  
  squares <- paste0("Sq", 1:9)
  for (i in 1:length(squares))
  {
    #Initializing the variable for the plot
    shapes<-obj[[squares[i]]]$shape
    sz.x<-obj[[squares[i]]]$size.x
    sz.y<-obj[[squares[i]]]$size.y
    rot<-obj[[squares[i]]]$rotation
    visible <- obj[[squares[i]]]$visible
    pos.x<-obj[[squares[i]]]$pos.x
    pos.y<-obj[[squares[i]]]$pos.y
    lwd<-obj[[squares[i]]]$lwd
    lty<-obj[[squares[i]]]$lty
    numerosity<-obj[[squares[i]]]$num
    nvert<-obj[[squares[i]]]$nv
    shades<-obj[[squares[i]]]$shade
    theta1<-obj[[squares[i]]]$theta.1
    theta2<-obj[[squares[i]]]$theta.2
    
    #Fixing the plot area for each cells
    Canvas(xlim=16,mar=c(1,1,1,1), main = main)
    #Canvas(16,16)
    for(j in 1:length(shapes))
    {
      if(visible[[j]]==1)
      {
        if(numerosity[[j]][1]==1){
         DrawRegPolygon(x = pos.x[[j]], y = pos.y[[j]], rot = rot[[j]], 
                                    radius.x = sz.x[[j]], radius.y = sz.y[[j]], nv = nvert[[j]],
                        lty=lty[[j]],lwd=lwd[[j]],col = shades[[j]])
        }else{
          DrawCircle(x = pos.x[[j]], y = pos.y[[j]], 
                    r.out = sz.x[[j]],r.in= sz.y[[j]], theta.1=theta1[[j]],
                                                      theta.2=theta2[[j]], nv = nvert[[j]],
                    lty=lty[[j]],lwd=lwd[[j]],col = shades[[j]])

        }
      }
    }
    
  }
}
