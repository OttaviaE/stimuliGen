#27/09 nuovo codice per l'ellisse basato sulla generazione gerarchica di regole

# ellisse ---- 
library(DescTools)
drawCorrect = function(object) {
  if (object[["shape"]] == "ellipse") {
    par(mfrow=c(1,1), mar = c(0.5,6,0.5,2)+0.1)
        Canvas(20, 20)
        DrawEllipse(radius.x = object[["size.x.elli"]][3, 3], 
                    radius.y = object[["size.y.elli"]][3, 3], 
                    lwd = 2, 
                    rot = object[["rotation"]][3, 3], 
                    col = SetAlpha("black",object[["shade"]][3, 3]), 
                    plot = T, lty = object[["line"]][3, 3])
  } else if (object[["shape"]] == "triangle") {
   
    par(mfrow=c(1,1), mar = c(0.5,6,0.5,2)+0.1)
        Canvas(20,20)
        DrawRegPolygon(rot = object[["rotation"]][3, 3],
                       col = SetAlpha("black",object[["shade"]][3, 3]),
                       nv=3, lwd = 2, 
                       lty = object[["line"]][3, 3], 
                       radius.x = object[["size.x"]][3, 3], 
                       radius.y = object[["size.y"]][3, 3])
  } else if (object[["shape"]] == "pentagon") {
    par(mfrow=c(1,1), mar = c(0.5,6,0.5,2)+0.1)
        temp = NULL
        Canvas(20,20)
        DrawRegPolygon(rot = object[["rotation"]][3, 3],
                       col = SetAlpha("black",object[["shade"]][3, 3]),
                       nv=5, lwd = 2, 
                       lty = object[["line"]][3, 3], 
                       radius.x = object[["size.x"]][3, 3], 
                       radius.y = object[["size.y"]][3, 3])
  } else if (object[["shape"]] == "all") {
    vert = Permn(c(3,5,100))[c(seq(1,5,by=2)), ]
    par(mfrow=c(1,1), mar = c(0.5,6,0.5,2)+0.1)
        Canvas(20,20)
        DrawRegPolygon(rot = object[["rotation"]][3, 3],
                       col = SetAlpha("black",object[["shade"]][3, 3]),
                       nv=vert[3, 3], lwd = 2, 
                       lty = object[["line"]][3, 3], 
                       radius.x = object[["size.x"]][3, 3], 
                       radius.y = object[["size.y"]][3, 3])
  }
}

drawStim = function(object) {
  if (object[["shape"]] == "ellipse") {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:3) {
      for (j in 1:3) {
        Canvas(20, 20)
        DrawEllipse(radius.x = object[["size.x.elli"]][i, j], 
                    radius.y = object[["size.y.elli"]][i, j], 
                    lwd = 2, 
                    rot = object[["rotation"]][i, j], 
                    col = SetAlpha("black",object[["shade"]][i, j]), 
                    plot = T, lty = object[["line"]][i, j])
      }
    }
  } else if (object[["shape"]] == "triangle") {
    
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:3) {
      for (j in 1:3) {
        temp = NULL
        Canvas(20,20)
        DrawRegPolygon(rot = object[["rotation"]][i, j],
                       col = SetAlpha("black",object[["shade"]][i, j]),
                       nv=3, lwd = 2, 
                       lty = object[["line"]][i, j], 
                       radius.x = object[["size.x"]][i, j], 
                       radius.y = object[["size.y"]][i, j])
      }
    }
    
  } else if (object[["shape"]] == "pentagon") {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:3) {
      for (j in 1:3) {
        temp = NULL
        Canvas(20,20)
        DrawRegPolygon(rot = object[["rotation"]][i, j],
                       col = SetAlpha("black",object[["shade"]][i, j]),
                       nv=5, lwd = 2, 
                       lty = object[["line"]][i, j], 
                       radius.x = object[["size.x"]][i, j], 
                       radius.y = object[["size.y"]][i, j])
      }
    }
  } else if (object[["shape"]] == "all") {
    vert = Permn(c(3,5,100))[c(seq(1,5,by=2)), ]
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:3) {
      for (j in 1:3) {
        temp = NULL
        Canvas(20,20)
        DrawRegPolygon(rot = object[["rotation"]][i, j],
                       col = SetAlpha("black",object[["shade"]][i, j]),
                       nv=vert[i,j], lwd = 2, 
                       lty = object[["line"]][i, j], 
                       radius.x = object[["size.x"]][i, j], 
                       radius.y = object[["size.y"]][i, j])
      }
    }
    
  }
}
