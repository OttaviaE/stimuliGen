# disegni matrici uniche 
Canvas()

# vertical canvas
draw(vline(pos.x=-30, s.x = 30, lwd = 5))
for(i in seq(-30, 30, by = 1)) {
  draw(vline(pos.x = i, s.x=40, lwd = 5), 
       canvas = F)
}

# horizontal canvas

#draw(hline(pos.y=-17, s.x = 30))
for(i in seq(-17, 17, by = 1)) {
  draw(hline(pos.y = i, s.x=30, lwd = 5), 
       canvas = F)
}


# vertical inner canvas

draw(vline(pos.x=-50, s.x = 30, lwd = 1), bg = "white")
for(i in seq(-30, 30, by = 1)) {
  
    draw(vline(pos.x = i, s.x=40, lwd = 1+ abs(i)/2), 
         canvas = F,  bg = "white")
}

# vertical outer canvas
draw(vline(pos.x=-50, s.x = 30, lwd = 1), bg = "white")
p = seq(-30, 30, by = 2)
q = c(abs(p[(length(p)/2):1]), abs(p[1:(length(p)/2)]))
for(i in 1:length(p)) {
  draw(vline(pos.x = p[i], s.x=40, lwd = q[i]), 
       canvas = F,  bg = "white")
}

draw(vline(pos.x=-50, s.x = 30, lwd = 1), bg = "white")

p = seq(-10, 10, by = 2)
q = c(abs(p[(length(p)/2):1]), abs(p[1:(length(p)/2)]))
for(i in 1:length(p)) {
  draw(vline(pos.x = p[i], s.x=40, lwd = q[i]), 
       canvas = F,  bg = "white")
}

Canvas(mar = c(15,17,15,15), bg = "white")
text(0, 0, "Hello world!", cex=2)


draw(square())

# vertical increasing canvas
draw(vline(pos.x=-50, s.x = 30, lwd = 1), bg = "white")
for(i in seq(-30, 30, by = 4)) {
  draw(vline(pos.x = i, s.x=40, lwd = 31 + i), 
       canvas = F,  bg = "white")
}


# vertical decreasing canvas
draw(vline(pos.x=-50, s.x = 30, lwd = 1), bg = "white")
for(i in seq(-30, 30, by = 4)) {
  draw(vline(pos.x = i, s.x= 40, lwd = 31 - i), 
       canvas = F,  bg = "white")
}

# stessa cosa ma in orizzontale (viene una merda)------

# horizontal inner canvas

draw(hline(pos.y=-17, s.x = 100, lwd = 1), bg = "white")
for(i in seq(-30, 30, by = 1)) {
  draw(hline(pos.y = i, s.x=100, lwd = 1+ abs(i)/2), 
       canvas = F,  bg = "white")
}

# vertical outer canvas
draw(hline(pos.y=-50, s.x = 100, lwd = 1), bg = "white")
p = seq(-30, 30, by = 4)
q = c(abs(p[(length(p)/2):1]), abs(p[1:(length(p)/2)]))
for(i in 1:length(p)) {
  draw(hline(pos.y  = p[i], s.x=100, lwd = q[i]), 
       canvas = F,  bg = "white")
}




# vertical increasing canvas
draw(hline(pos.y=-50, s.x = 100, lwd = 1), bg = "white")
for(i in seq(-15, 15, by = 2)) {
  draw(hline(pos.y = i, s.x=100, lwd = 31 + i/2), 
       canvas = F,  bg = "white")
}


# vertical decreasing canvas
draw(vline(pos.x=-50, s.x = 30, lwd = 1), bg = "white")
for(i in seq(-30, 30, by = 4)) {
  draw(vline(pos.x = i, s.x= 40, lwd = 31 - i), 
       canvas = F,  bg = "white")
}

# in orizzonatel provo a fare le righe che diventano via via più lunghe 
draw(hline(pos.y=-50, s.x = 100, lwd = 1), bg = "white")
p = seq(-15, 15, by = 2)
for(i in 1:length(p)) {
  draw(hline(pos.y = -p[i], s.x=i*2, lwd = 3), 
       canvas = F,  bg = "white")
}

# matrici con le righette ---- 

Canvas()
draw(diagline(pos.x = -25, pos.y = 20,  rotation = pi/2))
draw(diagline(pos.x = -15, pos.y = 15,  rotation = pi), canvas = F)


draw(vline(pos.x = -20, pos.y = 20, s.x = 5, s.y = 5))
draw(hline(pos.x = -15, pos.y = 15, s.y  = 5, s.x = 5), canvas = F)
draw()
