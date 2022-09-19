# ellisse ---- 

stimElli = function(rotation = NULL, 
                   color = NULL, 
                   line = NULL) {
  rot_rule = rotation[["rotation"]]
  shade_rule = color[["shade"]]
  if(!is.null(rotation) & is.null(color) & is.null(line)) {
     par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
  for (i in 1:length(rot_rule)) {
    for (j in 1:length(rot_rule[[i]])) {
      temp = NULL
      Canvas(15, 15)
      DrawEllipse(x = 0,
                  radius.x = 10, 
                  radius.y = 15, 
                  lwd = 2, 
                  rot = rot_rule[[i]][[j]], plot = T)
    }
  }
  } else if (!is.null(rotation) & !is.null(color) & is.null(line)) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule)) {
      for (j in 1:length(rot_rule[[i]])) {
        temp = NULL
        Canvas(15, 15)
        DrawEllipse(x = 0,
                    radius.x = 10, 
                    radius.y = 15, 
                    lwd = 2, 
                    rot = rot_rule[[i]][[j]], 
                    col = SetAlpha("black",shade_rule[[i]][[j]]), 
                    plot = T)
      }
      }
    }
  }
