### Applying Rule
apply <- function(obj) {
  UseMethod("apply")
}

logic_rules <- function(...) {
  UseMethod("logic_rules")
}

apply.Raven_matrix <- function(obj) {
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
  attr(obj, "class") <- "Raven_matrix"
  return(obj)
}

draw <- function(obj) {
  UseMethod("draw")
}

draw.Raven_matrix<- function(obj) {
  library(DescTools)
  par(mfrow = c(3, 3), mar = c(0.5, 6, 0.5, 2) + 0.1)
  
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
    
    #Fixing the plot area for each cells
    Canvas(15,15)
    for(j in 1:length(shapes))
    {
      if(visible[j]==1)
      {
        for(corn in 1:numerosity[j])
        {
          coords<-do.call("data.frame",DrawRegPolygon(x = pos.x[j], y = pos.y[j], rot = rot[j], 
                                                      radius.x = sz.x[j], nv = nvert[j],plot = FALSE))
           if(shades[j]=='none'){
             shd<-NULL
           }else{
             shd<-shades[j]
             }
          polygon(coords[, c("x","y")],
                  lty=lty[j],lwd=lwd[j],col = shd)
        }
      }
    }
    
  }
}

