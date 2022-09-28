# triangolo ---- 

stimTri = function(object, rotation = NULL, 
                   color = NULL, 
                   line = NULL) {
  rot_rule = object[["rotation"]]
  shade_rule = object[["shade"]]
  line_rule = object[["line"]]
  if (is.null(rotation) & is.null(color) & is.null(line)) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas()
        DrawRegPolygon(rot = rot_rule[[i]][[j]],
                       col = SetAlpha("black",shade_rule[[i]][[j]]),
                       nv=3, lwd = 2, lty = line_rule[[i]][[j]])
      }
    }
  } else if (!is.null(rotation) & is.null(color) & is.null(line)) {   # rotation only ----
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas()
        DrawRegPolygon(rot = rot_rule[[i]][[j]], nv=3, lwd = 2)
      }
    }
  }  # color only  ---- 
  else if (is.null(rotation) & !is.null(color) & is.null(line)) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas()
        DrawRegPolygon(col = SetAlpha("black",shade_rule[[i]][[j]]),
                       nv=3, lwd = 2)
      }
    }
  } # line only ------
  else if(is.null(rotation) & is.null(color) & !is.null(line)){
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas()
        DrawRegPolygon(lty = line_rule[[i]][[j]],
                       nv=3, lwd = 2)
      }
    }
  }
  # rotation and color ---- 
  else if (!is.null(rotation) & !is.null(color) & is.null(line)) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas()
        DrawRegPolygon(col = SetAlpha("black",shade_rule[[i]][[j]]), 
                       rot = rot_rule[[i]][[j]],
                       nv=3, lwd = 2)
      }
    }
  } else if (is.null(rotation) & !is.null(color) & !is.null(line)) { # line and color ----- 
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas()
        DrawRegPolygon(col = SetAlpha("black",shade_rule[[i]][[j]]),
                       nv=3, lwd = 2, lty = line_rule[[i]][[j]])
      }
    }
  } else if (!is.null(rotation) & is.null(color) & !is.null(line)) { # rotation and line ----
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas()
        DrawRegPolygon(rot = rot_rule[[i]][[j]],
                       nv=3, lwd = 2, lty = line_rule[[i]][[j]])
      }
    }
  } # rotation color and line ---- 
  else if (!is.null(rotation) & !is.null(color) & !is.null(line)) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas()
        DrawRegPolygon(rot = rot_rule[[i]][[j]],
                       col = SetAlpha("black",shade_rule[[i]][[j]]),
                       nv=3, lwd = 2, lty = line_rule[[i]][[j]])
      }
    }
  } 
}
