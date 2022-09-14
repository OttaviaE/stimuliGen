library(DescTools)

alpha = c(.10, .50, .70)


rot = c(0, 45, 90)



par(mfrow=c(3,3))
for (i in 1:length(alpha)) {
    for (j in 1:length(alpha[[i]])) {
      Canvas(15, 15)
      DrawEllipse(x = 0, y = 0, 
                  radius.x = 15, 
                  radius.y = 10, rot = rot[[i]][j],
                  col=SetAlpha("black", alpha[[i]][j]))
    }
    
  }
  

alpha = list(first = c(.10, .50, .90), 
             second = c(.90, .50, .10), 
             third = c(.50, .10))

rot = list(first =  c(0, 45, 90), 
           second = c(90, 45, 0), 
           third = c(45, 0))

stimGen("ellipse", color = "blue", rotation = rot)
stimGen("ellipse", color = "red", rotation = rot, alpha = alpha)
stimGen("circle", color = "yellow")
stimGen("circle", color = "blue", alpha = alpha)


par(mfrow=c(1,1))
Canvas(15, 15)
# ellisse
DrawEllipse(x = 0, y = 0, 
            radius.x = 15, 
            radius.y = 10, 
            rot = rot[[1]][3])
# cerchio 
Canvas(15, 15)
DrawEllipse(x = 0, y = 0, 
            radius.x = 10, 
            radius.y = 10, 
            rot = rot[[1]][3])
# rettangolo 
Canvas(15,15)
DrawEllipse(x = 5, y = 5, 
            radius.x = 10, 
            radius.y = 10, col = "black")


# riga orizzontale
Canvas(15, 15)
DrawEllipse(x = 0, y = 10, 
            radius.x = 15, 
            radius.y = 0)
# riga verticale 
Canvas(15, 15)
DrawEllipse(x = 0, y = 10, 
            radius.x = 15, 
            radius.y = 0)


