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

