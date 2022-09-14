# 9 rettangoli -----
# stessa grandezza, rimepimento per riga, domanda in fondo a dx ----
# prepara il plot
par(mfrow=c(3,3))
# up sx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, angle = 50)

# up center 
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="grey", border = "black")

# up rx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="black", border = "black")

# middle sx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="black", border = "black")

# middle center
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, angle = 50)

# middle rx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="grey", border = "black")

# bottom sx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="grey", border = "black")

# bottom center
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="black", border = "black")

# bottom rx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
text(x = 45, y = 55, "?", cex =10)


# stessa grandezza, rimepimento per colonna, domanda in fondo a dx
# prepara il plot
par(mfcol=c(3,3)) # basta cambiare questo
# up sx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, angle = 50)

# up center 
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="grey", border = "black")

# up rx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="black", border = "black")

# middle sx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="black", border = "black")

# middle center
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, angle = 50)

# middle rx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="grey", border = "black")

# bottom sx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="grey", border = "black")

# bottom center
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
rect(25, 30, 65, 80, col ="black", border = "black")

# bottom rx
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
text(x = 45, y = 55, "?", cex =10)

# altre figure ---- 
# cerchio ----
# è orrendo 
symbols(x=20, y =30, circles = 2,
        fg="blue", bg="blue", 
        xlab = "", ylab = "", bty="n", axes=F)
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
# dagli esempi del pacchetto DescTools
Canvas(xlim = c(-5,5), xpd=TRUE)
cols <- Pal("Helsana")[1:4]
DrawCircle
# questo è interessante perché puoi fare completare il cerchio (letteralmente)
geom <- rbind(c(-pi, 0, .25, .5), c(0, pi, 1, 2),
              c(-pi/2, pi/2, 2, 2.5), c(pi/2, 3 * pi/2, 3, 4),
              c(pi - pi/8, pi + pi/8, 1.5, 2.5))

DrawCircle (r.in = geom[,3], r.out = geom[,4],
            theta.1 = geom[,1], theta.2 = geom[,2],
            col = SetAlpha(cols, 0.6),
            border = cols, lwd=1)

# ellisse ----
library(DescTools)
plot(c(10, 80), c(30, 80), type = "n", xlab = "", ylab = "", 
     axes = F)
DrawEllipse(x = 15, y = 50, radius.x = 5, radius.y = 10)
DrawEllipse(x = 35, y = 50, radius.x = 10, radius.y = 5, col = "red")
DrawEllipse(x = 55, y = 50, radius.x = 10, radius.y = 5, rot = 45, lwd=3)
DrawEllipse(x = 75, y = 50, radius.x = 10, radius.y = 5, rot = 90,
            col=SetAlpha("red", 0.5), lty =3)
Canvas(15, 15)
DrawEllipse(x = 0, y = 0, radius.x = 10, radius.y = 10)
par(new=T)
DrawEllipse(x = 0, y = 0, radius.x = 15, radius.y = 10)


Canvas(xlim=c(-5,5))
DrawCircle (r.out=4:1, col=c("white", "steelblue2", "white", "red"), lwd=3, nv=300)

x <- seq(-3, 3, length.out=10)
y <- rep(0, length.out=length(x))
Canvas(xlim=c(-5,5), bg="black")
sapply( (0:11) * pi/6, function(theta) {
  xy <- Rotate(x, y=y, theta=theta)
  DrawCircle (x=xy$x, y=xy$y, r.in=2.4, border=SetAlpha("white", 0.2))
} )

plot(c(0,1),c(0,1), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
DrawRegPolygon(x = 0.5, y = 0.5, rot = (1:4)*pi/6, radius.x = 0.5, nv = 3,
               col = SetAlpha("yellow",0.5))

Canvas()
DrawRegPolygon(radius.x=c(0.7, 0.5), 
               radius.y = c(0.5, 0.5), 
               nv = c(100, 4), plot=T, 
               col = c("white", "blue"), rot = c(0, 180))

Canvas()
DrawRegPolygon(radius.x=c(0.7, 0.2, 0.2, 0.2), 
               radius.y = c(0.7, 0.5, 0.2, 0.5), 
               nv = c(100, 100, 100, 100), plot=T, 
               col = c("white", "blue", "red", "green"))



# Calculate circle and hexagon, but do not plot
pts <- DrawRegPolygon(radius.x=c(0.7, 0.5), nv = c(100, 4), plot=F )
# combine the 2 shapes and plot the new structure
polygon(x = unlist(lapply(pts, "[", "x")),
        y=unlist(lapply(pts, "[", "y")), col="green", border=T)


# 14/09/2022 ----- 
# cambiare grandezza 
# supponendo di tenere fisso il numero di stimoli 
# ellisse su canvas 3 x 3
# regola: crescente prima riga
# gigante minuscolo piccolo seconda riga
# decrescente terza riga
# questa regola la identiofico con rule 1 e la assegno a una lista 
# ATTENZIONE: per il momento tengo la y fissa
size_rule1x =  list(first = c(4, 8, 12), 
                   second = c(12, 4, 8), 
                   third = c(12, 8))
size_rule1y = list(first = c(5, 10, 15), 
                   second = c(15, 5, 10), 
                   third = c(15, 10))

par(mfrow=c(1,1))
par(mar=c(2, 2, 2, 2))
Canvas(15, 15)
DrawEllipse(x = 0, y = 0, 
            radius.x = 25, 
            radius.y = 30)
par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
for (i in 1:length(size_rule1x)) {
  for (j in 1:length(size_rule1x[[i]])) {
    Canvas(15, 15)
    DrawEllipse(x = 0, y = 0, 
                radius.x = size_rule1x[[i]][[j]], 
                radius.y = size_rule1y[[i]][[j]])
  }
  
}

# seconda regola di grandezza: l'inverso di quella sopra
size_rule2x =  list(first = c(12, 8, 4), 
                    second = c(8, 4, 12), 
                    third = c(4, 8))
size_rule2y = list(first = c(15, 10, 5), 
                   second = c(10, 5, 15), 
                   third = c(15, 10))
par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
for (i in 1:length(size_rule1x)) {
  for (j in 1:length(size_rule1x[[i]])) {
    Canvas(15, 15)
    DrawEllipse(x = 0, y = 0, 
                radius.x = size_rule2x[[i]][[j]], 
                radius.y = size_rule2y[[i]][[j]])
  }
  
}

# regola della rotazione 
# clockwise

# anticlockwise




