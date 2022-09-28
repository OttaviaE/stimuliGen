#27/09 nuovo codice per l'ellisse basato sulla generazione gerarchica di regole

# ellisse ---- 
library(DescTools)
stimElli = function(object, multi = F) {
  if (multi == F) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(object)) {
      for (j in 1:length(object)) {
        Canvas(15, 15)
        DrawEllipse(x = 0,
                    radius.x = 10, 
                    radius.y = 15, 
                    lwd = 2, 
                    rot = object[["rotation"]][i, j], 
                    col = SetAlpha("black",object[["shade"]][i, j]), 
                    plot = T, lty = object[["line"]][i, j])
      }
    }
  } else if (multi == T) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:nrow(object)) {
      for (j in 1:ncol(object)) {
        temp = NULL
        Canvas()
        DrawEllipse(lwd = 2, 
                    rot = (1:object[i, j])*pi/3)
      }}
  }

}

stimElli(getDone(highRule(multi = T), multi = "increasing"), multi = T)
stimElli(getDone(highRule(rotation = T, shade = T, line = T), 
        rotation = "htv", shade = "wtb", line = "sdad"))
ob = getDone(highRule(multi = T), multi = "increasing")
stimElli(ob)
ob[["rotation"]][3, 1]

p = ob[["rotation"]]
