stimGen = function(shape = c("ellipse", "circle", "rectangle", "line"), 
                   rotation = NULL, 
                   color = NULL, 
                   lcol = NULL, 
                   lwd = NULL, 
                   alpha = NULL) {
  if (is.null(alpha) == FALSE & is.null(color) == TRUE) {
    stop("You must specify a color!")
  }
  if (shape == "ellipse") {
    if (is.null(rotation) == TRUE & is.null(color) == TRUE & is.null(alpha) == TRUE) {
      par(mfrow=c(3,3))
      for (i in 1:8) {
        Canvas(15, 15)
        DrawEllipse(x = 0, y = 0, 
                    radius.x = 15, 
                    radius.y = 10)
      }
    } else if (is.null(color) == FALSE & is.null(alpha) == TRUE & is.null(rotation) == TRUE) {
      par(mfrow=c(3,3))
      for (i in 1:8) {
        Canvas(15, 15)
        DrawEllipse(x = 0, y = 0, 
                    radius.x = 15, 
                    radius.y = 10, 
                    col = color)
      }
    } else if (is.null(color) == FALSE &  is.null(alpha) == FALSE & is.null(rotation) == TRUE) {
      par(mfrow=c(3,3))
      for (i in 1:length(alpha)) {
        for (j in 1:length(alpha[[i]])) {
          Canvas(15, 15)
          DrawEllipse(x = 0, y = 0, 
                      radius.x = 15, 
                      radius.y = 10, 
                      col=SetAlpha(color, alpha[[i]][j]))
        }
        
      }
    } else if (is.null(color) == FALSE &  is.null(alpha) == TRUE & is.null(rotation) == FALSE) {
      par(mfrow=c(3,3))
      for (i in 1:length(rotation)) {
        for (j in 1:length(rotation[[i]])) {
          Canvas(15, 15)
          DrawEllipse(x = 0, y = 0, 
                      radius.x = 15, 
                      radius.y = 10, 
                      col= color, 
                      rot = rotation[[i]][j])
        }
        
      }
    } else if (is.null(color) == FALSE &  is.null(alpha) == FALSE & is.null(rotation) == FALSE) {
      par(mfrow=c(3,3))
      for (i in 1:length(rotation)) {
        for (j in 1:length(rotation[[i]])) {
          Canvas(15, 15)
          DrawEllipse(x = 0, y = 0, 
                      radius.x = 15, 
                      radius.y = 10, 
                      col= SetAlpha(color, alpha[[i]][j]), 
                      rot = rotation[[i]][j])
        }
      }
      
    }
  } else if (shape == "circle") {
    if (is.null(rotation) == FALSE) {
      stop("Do you really want to ROTATE a CIRCLE?")
    }
    
    if (is.null(color) == TRUE & is.null(alpha) == TRUE) {
      par(mfrow=c(3,3))
      for (i in 1:8) {
        Canvas(15, 15)
        DrawEllipse(x = 0, y = 0, 
                    radius.x = 10, 
                    radius.y = 10)
      }
    } else if (is.null(color) == FALSE & is.null(alpha) == TRUE) {
      par(mfrow=c(3,3))
      for (i in 1:8) {
        Canvas(15, 15)
        DrawEllipse(x = 0, y = 0, 
                    radius.x = 10, 
                    radius.y = 10, 
                    col = color)
      }
    } else if (is.null(color) == FALSE &  is.null(alpha) == FALSE) {
      par(mfrow=c(3,3))
      for (i in 1:length(alpha)) {
        for (j in 1:length(alpha[[i]])) {
          Canvas(15, 15)
          DrawEllipse(x = 0, y = 0, 
                      radius.x = 10, 
                      radius.y = 10, 
                      col=SetAlpha(color, alpha[[i]][j]))
        }
        
      }
    } 
  } 
}
