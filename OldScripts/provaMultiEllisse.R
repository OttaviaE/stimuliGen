# 27/09 
# tante ellissi!

par(mfrow=c(1,2))

rotations = c(1:3)*pi/3

rot_rule = list(a = c(1,2,3), 
                b = c(2,1,3), 
                d = c(3,2,1))



par(mfrow=c(1,3))
Canvas()
DrawEllipse(rot = 1:rot_rule$b*pi/3)
Canvas()
DrawEllipse(rot = 1:rot_rule$a*pi/3)
Canvas()
DrawEllipse(rot = 1:rot_rule$d*pi/3)



Canvas()
DrawEllipse(rot = c(1,2)*pi/3)

Canvas()
DrawEllipse(rot = c(1:3)*pi/3)
par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
for (i in 1:length(rot_rule)) {
  for (j in 1:length(rot_rule[[i]])) {
    temp = NULL
    Canvas()
    DrawEllipse(lwd = 2, 
                rot = (1:rot_rule[[i]][[j]])*pi/3)
  }}

DrawEllipse(rot = c(1:3) * pi/3, 
            col=SetAlpha(c("blue","red","green"), 0.5) )
shade_rule = getDone(highRule(shade = T), shade = "wtg")$shade
