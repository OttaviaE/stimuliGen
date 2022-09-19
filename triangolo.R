# triangolo ---- 

stimTri = function(rotation = NULL, 
                   color = NULL, 
                   line = NULL) {
  rot_rule = rotation[["rotation"]]
  shade_rule = color[["shade"]]
  line_rule = line[["line"]]
  
  if (!is.null(rotation) & is.null(color) & is.null(line)) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas()
        DrawRegPolygon(rot = rot_rule[[i]][[j]], nv=3, lwd = 2)
      }
    }
  } else if (!is.null(rotation) & !is.null(color) & is.null(line)) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas()
        DrawRegPolygon(rot = rot_rule[[i]][[j]],
                       col = SetAlpha("black",shade_rule[[i]][[j]]),
                       nv=3, lwd = 2)
      }
    }
  } else if (!is.null(rotation) & !is.null(color) & !is.null(line)) {
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
