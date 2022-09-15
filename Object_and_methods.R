field <- list(
  shape = NULL,
  size.x = NULL,
  size.y = NULL,
  rotation = NULL,
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


field <- function(shapes,x,y,rot,vis) {
  value <- list(
    shape = shapes,
    size.x = x,
    size.y = y,
    rotation = rot,
    visible=vis
  )
  # class can be set using class() or attr() function
  attr(value, "class") <- "field"
  value
}

Raven <- function(st1,hrule,vrule) {
  value<-list()
  squares<-paste0("Sq",1:9)
  for(i in 1:length(squares))
  {
    value[[squares[i]]]<-st1
  }
  value$hrule<-hrule
  value$vrule<-vrule
  # class can be set using class() or attr() function
  attr(value, "class") <- "Raven_matrix"
  value
}

##Rules
rotation <- function(obj,n) {
  UseMethod("rotation")
}
rotation.field<-function(obj,n) {
  obj$rotation<-obj$rotation+n*pi/6
  return(obj)
}

resize <- function(obj,n) {
  UseMethod("resize")
}
resize.field<-function(obj,n) {
  obj$size.x<-obj$size.x/n
  obj$size.y<-obj$size.y/n
  return(obj)
}

### Applying Rule
apply_rule <- function(obj) {
  UseMethod("apply_rule")
}
apply_rule.Raven_matrix <- function(obj) {
  #applying the horizontal rules
  hrules <- obj$hrule
  row_1 <- paste0("Sq", 1:3)
  row_2 <- paste0("Sq", 4:6)
  row_3 <- paste0("Sq", 7:9)
  for (r in 1:length(hrules))
  {
    if (hrules[r] == "rotation")
    {
      for (i in 1:3)
      {
        obj[[row_1[i]]] <- rotation(obj[[row_1[i]]],i)
        obj[[row_2[i]]] <- rotation(obj[[row_2[i]]],i)
        obj[[row_3[i]]] <- rotation(obj[[row_3[i]]],i)
      }
    }else if(hrules[r] == "size")
    {
      for (i in 1:3)
      {
      obj[[row_1[i]]] <- resize(obj[[row_1[i]]],i)
      obj[[row_2[i]]] <- resize(obj[[row_2[i]]],i)
      obj[[row_3[i]]] <- resize(obj[[row_3[i]]],i)
      }
    }
  }
  
  #applying the vertical rules
  vrules <- obj$vrule
  col_1 <- paste0("Sq", seq(1, 9, 3))
  col_2 <- paste0("Sq", seq(2, 9, 3))
  col_3 <- paste0("Sq", seq(3, 9, 3))
 
  for (r in 1:length(vrules))
  {
    if (vrules[r] == "rotation")
    {
      for (i in 1:3)
      {
        obj[[col_1[i]]] <- rotation(obj[[col_1[i]]],i)
        obj[[col_2[i]]] <- rotation(obj[[col_2[i]]],i)
        obj[[col_3[i]]] <- rotation(obj[[col_3[i]]],i)
      }
    }else if(vrules[r] == "size")
    {
      for (i in 1:3)
      {
      obj[[col_1[i]]] <- resize(obj[[col_1[i]]],i)
      obj[[col_2[i]]] <- resize(obj[[col_2[i]]],i)
      obj[[col_3[i]]] <- resize(obj[[col_3[i]]],i)
      }
    }
  }
  return(obj)
}

###DRAWING METHODS
draw <- function(obj) {
  UseMethod("draw")
}

draw.Raven_matrix<- function(obj) {
  library(DescTools)
 
  par(mfrow = c(3, 3), mar = c(0.5, 6, 0.5, 2) + 0.1)

  squares <- paste0("Sq", 1:9)
  for (i in 1:length(squares))
  {
    shapes<-obj[[squares[i]]]$shape
    x<-obj[[squares[i]]]$size.x
    y<-obj[[squares[i]]]$size.y
    rot<-obj[[squares[i]]]$rotation
    Canvas(15,15)
    for(j in 1:length(shapes))
    {
      if(shapes[j]=="triangle")
        {
        
        DrawRegPolygon(x = 0, y = 0, rot = rot[j], 
                        radius.x = x[j], nv = 3)
      }else if(shapes[j]=="elipse")
        {
        DrawEllipse(x = 0, y = 0, 
                    rot = rot[j], 
                    radius.x = x[j],radius.y = y[j])
      }else if(shapes[j]=="square")
      {
        DrawRegPolygon(x = 0, y = 0, rot = rot[j], 
                       radius.x = x[j], nv = 4)
      }
    }
    
  }
}
