
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
    }else if(hrules[r] == "diff_shapes")
    {
      for (i in 1:3)
      {
        obj[[row_1[i]]] <- diff_shapes(obj[[row_1[i]]],i)
        obj[[row_2[i]]] <- diff_shapes(obj[[row_2[i]]],i)
        obj[[row_3[i]]] <- diff_shapes(obj[[row_3[i]]],i)
      }
    }else if(hrules[r] == "lwd" | hrules[r] == "lty")
    {
      for (i in 1:3)
      {
      obj[[row_1[i]]] <- margin(obj[[row_1[i]]],i,hrules[r])
      obj[[row_2[i]]] <- margin(obj[[row_2[i]]],i,hrules[r])
      obj[[row_3[i]]] <- margin(obj[[row_3[i]]],i,hrules[r])
      }
    }else if(hrules[r] == "AND" | hrules[r] == "OR" |hrules[r] == "XOR"  )
    {
    for (i in 1:3)
    {
      obj[[row_1[i]]] <- logic(obj[[row_1[i]]],i,1,hrules[r])
      obj[[row_2[i]]] <- logic(obj[[row_2[i]]],i,2,hrules[r])
      obj[[row_3[i]]] <- logic(obj[[row_3[i]]],i,3,hrules[r])
    }
    }else if(grepl( "mov",hrules[r]))
    {
      for (i in 1:3)
      {
        obj[[row_1[i]]] <- movement(obj[[row_1[i]]],i,hrules[r])
        obj[[row_2[i]]] <- movement(obj[[row_2[i]]],i,hrules[r])
        obj[[row_3[i]]] <- movement(obj[[row_3[i]]],i,hrules[r])
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
    }else if(vrules[r] == "diff_shapes" )
    {
      for (i in 1:3)
      {
        obj[[col_1[i]]] <- diff_shapes(obj[[col_1[i]]],i)
        obj[[col_2[i]]] <- diff_shapes(obj[[col_2[i]]],i)
        obj[[col_3[i]]] <- diff_shapes(obj[[col_3[i]]],i)
      }
    }
    else if(vrules[r] == "lwd" | vrules[r] == "lty")
    {
      for (i in 1:3)
      {
      obj[[col_1[i]]] <- margin(obj[[col_1[i]]],i,vrules[r])
      obj[[col_2[i]]] <- margin(obj[[col_2[i]]],i,vrules[r])
      obj[[col_3[i]]] <- margin(obj[[col_3[i]]],i,vrules[r])
      }
    }
    else if(vrules[r] == "AND" | vrules[r] == "OR" |vrules[r] == "XOR"  )
    {
      for (i in 1:3)
      {
        obj[[col_1[i]]] <- logic(obj[[col_1[i]]],i,1,vrules[r])
        obj[[col_2[i]]] <- logic(obj[[col_2[i]]],i,2,vrules[r])
        obj[[col_3[i]]] <- logic(obj[[col_3[i]]],i,3,vrules[r])
      }
    }else if(grepl( "mov",vrules[r]))
    {
      for (i in 1:3)
      {
        obj[[col_1[i]]] <- movement(obj[[col_1[i]]],i,hrules[r])
        obj[[col_2[i]]] <- movement(obj[[col_2[i]]],i,hrules[r])
        obj[[col_3[i]]] <- movement(obj[[col_3[i]]],i,hrules[r])
      }
    }
  }
  attr(obj, "class") <- "Raven_matrix"
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
    sz.x<-obj[[squares[i]]]$size.x
    sz.y<-obj[[squares[i]]]$size.y
    rot<-obj[[squares[i]]]$rotation
    visible <- obj[[squares[i]]]$visible
    pos.x<-obj[[squares[i]]]$pos.x
    pos.y<-obj[[squares[i]]]$pos.y
    lwd<-obj[[squares[i]]]$lwd
    lty<-obj[[squares[i]]]$lty
    numerosity<-obj[[squares[i]]]$num
    Canvas(15,15)
    for(j in 1:length(shapes))
    {
      if(visible[j]==1)
      {
        if(shapes[j]=="triangle")
        {
            coords<-do.call("data.frame",DrawRegPolygon(x = pos.x[j], y = pos.y[j], rot = rot[j], 
                                                        radius.x = sz.x[j], nv = 3,
                                                        lty=lty[j],lwd=lwd[j],plot = FALSE))
            polygon(coords[, c("x","y")],
                    lty=lty[j],lwd=lwd[j])
        }else if(shapes[j]=="elipse")
        {
          coords<-do.call("data.frame",DrawEllipse(pos.x[j], y = pos.y[j], 
                      rot = rot[j], 
                      radius.x = sz.x[j],radius.y = sz.y[j],
                      lty=lty[j],lwd=lwd[j],plot=FALSE))
          polygon(coords[, c("x","y")],
                  lty=lty[j],lwd=lwd[j])
        }else if(shapes[j]=="square")
        {
          coords<-do.call("data.frame",DrawRegPolygon( pos.x[j], y = pos.y[j], rot = rot[j], 
                         radius.x = sz.x[j], nv = 4,lty=lty[j],lwd=lwd[j],plot=FALSE))
          polygon(coords[, c("x","y")],
                  lty=lty[j],lwd=lwd[j])
        }else if(shapes[j]=="cross")
        {
          coords<-do.call("data.frame",DrawRegPolygon( pos.x[j], y = pos.y[j],radius.x = sz.x[j],
                         rot = pi/2+rot[j],  nv = 2, lty=lty[j],lwd=lwd[j], plot=FALSE))
          polygon(coords[, c("x","y")],
                  lty=lty[j],lwd=lwd[j])
          
          coords<-do.call("data.frame",DrawRegPolygon(pos.x[j], y = pos.y[j],radius.x = sz.x[j],
                         rot = rot[j],  nv = 2,lty=lty[j],lwd=lwd[j],plot=FALSE)) 
          polygon(coords[, c("x","y")],
                  lty=lty[j],lwd=lwd[j])
        }else if(shapes[j]=="dice")
        {

          position<-matrix(c(1,1,-1,1,1,-1,-1,-1),byrow = TRUE,ncol=2)
          for(corn in 1:numerosity[j])
          {
            coords<-do.call("data.frame",
                            DrawEllipse(pos.x[j]*position[corn,1], y = pos.y[j]*position[corn,2], rot = rot[j], 
                                        radius.x = sz.x[j],radius.y = sz.y[j],
                                        lty=lty[j],lwd=lwd[j],plot = FALSE))
            polygon(coords[, c("x","y")],col="black")
          }
        }else if(shapes[j]=="striangle")
        {
          
          for(corn in 1:numerosity[j])
          {
            coords<-do.call("data.frame",DrawRegPolygon(x = pos.x[j], y = pos.y[j], rot = rot[j], 
                                                        radius.x = sz.x[j], nv = 3,plot = FALSE))
            polygon(coords[, c("x","y")],
                    lty=lty[j],lwd=lwd[j])
          }
      }
      }
    }
    
  }
}
