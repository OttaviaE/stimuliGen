# i distractors sono stati creati a mano, non c'è la funzione (ancora)
library(DescTools)
Canvas(15, 15)
DrawRegPolygon(radius.x = 10, radius.y = 10,rot = pi/2, nv = 3)
DrawRegPolygon(radius.x = 15, radius.y = 15, rot = pi/2, nv = 5)
DrawRegPolygon(radius.x = 10, radius.y = 10, rot = pi/2, nv = 5)

DrawEllipse(radius.x = 20, radius.y = 10)

ob = getDone(highRule(shape = T, shade = T, size  = T),  
             shade  = "wtb", shape = "all", size = "increasing")

drawStim(ob)
drawCorrect(ob)

# repetition error ----
# rleft
Canvas(20, 20)

DrawRegPolygon(radius.x = ob$size.x[3,2], 
               radius.y = ob$size.y[3,2], 
               nv=3, 
               rot = ob[["rotation"]][1, 1], 
            lty = ob[["line"]][1, 1], 
               col = SetAlpha("black",ob[["shade"]][3, 2]))


# r top 
Canvas(20, 20)
DrawRegPolygon(radius.x = ob$size.x[2,3], 
               radius.y = ob$size.y[2,3], 
               nv=3, 
               rot = ob[["rotation"]][1, 1], 
               lty = ob[["line"]][1, 1], 
               col = SetAlpha("black",ob[["shade"]][2,3]))
# r diag 
Canvas(20, 20)
DrawRegPolygon(radius.x = ob$size.x[2,2], 
               radius.y = ob$size.y[2,2], 
               nv=100, 
               rot = ob[["rotation"]][1, 1], 
               lty = ob[["line"]][1, 1], 
               col = SetAlpha("black",ob[["shade"]][2,2]))



# D union
Canvas(20, 20)
DrawRegPolygon(radius.x = c(ob$size.x[1,1], ob$size.x[2,2], ob$size.x[3,3]), 
               radius.y = c(ob$size.y[1,1], ob$size.y[2,2], ob$size.y[3,3]), 
               nv=c(100, 5,3), 
               rot = pi/2, 
               lty = ob[["line"]][1, 1], lwd = 2, 
               col = SetAlpha("black",ob[["shade"]][1,1]))
# D diff
Canvas(20, 20)
DrawRegPolygon(radius.x = ob$size.x[2,2],
               radius.y = ob$size.y[2,2],
               nv=6,
               rot = pi,
               lty = ob[["line"]][1, 1],
               col = SetAlpha("black",ob[["shade"]][3,3]), lwd=3)

# 

# d plus
Canvas(20, 20)
DrawRegPolygon(radius.x = ob$size.x[3,3], 
               radius.y = ob$size.y[3,3], 
               nv=3, 
               rot = pi/2, 
               lty = ob[["line"]][1, 1], 
               col = SetAlpha("black",ob[["shade"]][1,1]), lwd=25)

# WP Copy ----
Canvas(20, 20)
DrawRegPolygon(radius.x = ob$size.x[1,1], 
               radius.y = ob$size.y[1,1], 
               nv=3, 
               rot = pi/2, 
               lty = ob[["line"]][1, 1], 
               col = SetAlpha("black",ob[["shade"]][1,1]), lwd = 2)
# WP Flip ----

Canvas(20, 20)
DrawRegPolygon(radius.x = ob$size.x[2,2], 
               radius.y = ob$size.y[2,2], 
               nv=3, 
               rot = pi, 
               lty = ob[["line"]][1, 1], 
               col = SetAlpha("black",ob[["shade"]][2,2]), lwd = 2)

# WP MAatrix ----
Canvas(20, 20)
DrawRegPolygon(radius.x = c(ob$size.x[1,1], ob$size.x[2,2], ob$size.x[3,3]), 
               radius.y = c(ob$size.y[1,1], ob$size.y[2,2], ob$size.y[3,3]), 
               nv=c(100, 5,3), 
               rot = 3*pi, 
               lty = ob[["line"]][1, 1], lwd = 2, 
               col = SetAlpha("black",ob[["shade"]][1,1]))

# IC neg ----
Canvas(20, 20)
DrawRegPolygon(radius.x = ob$size.x[3,3], 
               radius.y = ob$size.y[3,3], 
               nv=5, 
               rot = pi/2, 
               lty = ob[["line"]][1, 1], 
               col = SetAlpha("black",ob[["shade"]][1,1]), lwd = 2)

# IC fill 
Canvas(20, 20)
for (i in 1:15) {
  DrawRegPolygon(radius.x = ob$size.x[3,3]-i, 
                 radius.y = ob$size.y[3,3]-i, 
                 nv=5, 
                 rot = pi/2, 
                 lty = ob[["line"]][1, 1], 
                 col = SetAlpha("black",ob[["shade"]][2,2]), lwd=3)
}


# Ic  rot 
Canvas(20, 20)
DrawRegPolygon(radius.x = ob$size.x[3,3], 
               radius.y = ob$size.y[3,3], 
               nv=5, 
               rot = pi/3, 
               lty =1,  
               col = SetAlpha("black",ob[["shade"]][3,3]), lwd = 2)

# Ic scale 
Canvas(20, 20)
DrawRegPolygon(radius.x = ob$size.x[1,1], 
               radius.y = ob$size.y[1,1], 
               nv=5, 
               rot = pi/2, 
               lty =1,  
               col = SetAlpha("black",ob[["shade"]][3,3]), lwd = 2)






plot(c(0,1),c(0,1), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")


DrawRegPolygon(x=xy$x, y=xy$y + 0.5, radius.x=seq(0.5, 0.1, -0.1),
               nv=4, rot=seq(0, pi/2, length.out=5), col=rainbow(5) )

DrawRegPolygon(x = 0,
               radius.x = 20, 
               radius.y = 20, 
               nv=100, border = NA, 
               rot = object[["rotation"]][1, 1], 
               plot = T, lty = object[["line"]][1, 1], col = "white")

object = ob
Canvas(20, 20)



pts <- DrawRegPolygon(radius.x=c(0.7, 0.5), nv = c(100, 6), plot=FALSE )
Canvas(15, 15)
DrawRegPolygon(radius.x = 10, radius.y = 15, nv = 100)
# combine the 2 shapes and plot the new structure
polygon(x = unlist(lapply(pts, "[", "x")),
        y=unlist(lapply(pts, "[", "y")), col="green", border=FALSE)
