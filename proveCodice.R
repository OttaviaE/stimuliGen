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




