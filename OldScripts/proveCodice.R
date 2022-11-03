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

Canvas(15,15)
DrawRegPolygon(radius.x = 10, 
               radius.y = 10, 
               nv=4, rot=pi)
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
rot_rule1 = list(first = c(1:3) * pi/3, 
                 second = c(2,3, 1) * pi/3, 
                 third = c(3, 1)* pi/3)

par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
for (i in 1:length(size_rule1x)) {
  for (j in 1:length(size_rule1x[[i]])) {
    Canvas(15, 15)
    DrawEllipse(x = 0, y = 0, 
                rot = rot_rule1[[i]][[j]], 
                radius.x = 10, radius.y = 15)
  }
}

# regola della riga 

lty_rule1 = list(first = c(1:3), 
                 second = c(2,1,3), 
                 third = c(3,1)) 
  
par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
for (i in 1:length(size_rule1x)) {
  for (j in 1:length(size_rule1x[[i]])) {
    Canvas(15, 15)
    DrawEllipse(x = 0, y = 0, 
                rot = rot_rule1[[i]][[j]], 
                radius.x = 10, radius.y = 15, lwd = 2, 
                lty = lty_rule1[[i]][[j]])
  }
}

# regola del colore (per ora solo black)

col_rule1 = list(first = c(0.10, 0.50, 0.90), 
                second = c(0.50, 0.90, 0.10), 
                third = c(0.90, 0.10))
rot_rule1 = list(first = c(1:3) * pi/3, 
                 second = c(2,3, 1) * pi/3, 
                 third = c(3, 2)* pi/3)
size_rule1x =  list(first = c(4, 8, 12), 
                    second = c(12, 4, 8), 
                    third = c(12, 8))
size_rule1y = list(first = c(5, 10, 15), 
                   second = c(15, 5, 10), 
                   third = c(15, 10))

lty_rule1 = list(first = c(1:3), 
                 second = c(2,1,3), 
                 third = c(3,1)) 

par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
Canvas()
for (i in 1:length(size_rule1x)) {
  for (j in 1:length(size_rule1x[[i]])) {
    Canvas(15, 15)
    DrawEllipse(x = 0, 
                radius.x = 10, 
                radius.y = 15, 
                lwd = 2, 
                lty = lty_rule1[[i]][[j]], 
                col=SetAlpha(c(rep("black", 3)), 
                             col_rule1[[i]][[j]]), 
                rot = rot_rule1[[i]][[j]])
  }
}

# codice per generare la risposta corretta 
rule1_last = lty_rule1[[length(lty_rule1)]] 
rule1_last = rule1_last[order(rule1_last)]
rule1_first = lty_rule1[[1]] 
rule1_first = rule1_first[order(rule1_first)]
component1 = rule1_first[!rule1_first %in% rule1_last]

rule2_last = col_rule1[[length(col_rule1)]] 
rule2_last = rule2_last[order(rule2_last)]
rule2_first = col_rule1[[1]] 
rule2_first = rule2_first[order(rule2_first)]
component2 = rule2_first[!rule2_first %in% rule2_last]

rule3_last = rot_rule1[[length(rot_rule1)]] 
rule3_last = rule3_last[order(rule3_last)]
rule3_first = rot_rule1[[1]] 
rule3_first = rule3_first[order(rule3_first)]
component3 = rule3_first[!rule3_first %in% rule3_last]


# disegno risposta corretta
par(mfrow=c(1,1))
Canvas(15,15)
DrawEllipse(x = 0, y = 0, radius.x = 10, radius.y = 15, lwd = 2,
            lty = component1, 
            col = SetAlpha("black", component2), 
            rot = component3, plot = T)

stimElli = function(rotation = NULL, 
                    color = NULL, 
                    line = NULL) {
  if (is.null(rotation) == T & is.null(color) == T & is.null(line) == T) {
    stop("Please specify at least one argument")
  }
  if (is.null(rotation) == F & is.null(color) == T & is.null(line) == T) {
    rot_rule1 = list(first = c(1:3) * pi/3, 
                     second = c(2,3, 1) * pi/3, 
                     third = c(3, 1)* pi/3)
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule1)) {
      for (j in 1:length(rot_rule1[[i]])) {
        temp = NULL
        Canvas(15, 15)
        DrawEllipse(x = 0, y = 0, 
                           rot = rot_rule1[[i]][[j]], 
                           radius.x = 10, radius.y = 15, plot = T)
      }
    }
  } else if (is.null(rotation) == F & is.null(color) == F & is.null(line) == T) {
    col_rule1 = list(first = c(0.10, 0.50, 0.00), 
                     second = c(0.50, 0.00, 0.10), 
                     third = c(0.00, 0.10))
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(size_rule1x)) {
      for (j in 1:length(size_rule1x[[i]])) {
        Canvas(15, 15)
        DrawEllipse(x = 0, 
                    radius.x = 10, 
                    radius.y = 15, 
                    lwd = 2, 
                    col=SetAlpha(c(rep("black", 3)), 
                                 col_rule1[[i]][[j]]), 
                    rot = rot_rule1[[i]][[j]])
      }
    }
  } else if (is.null(rotation) == F & is.null(color) == F & is.null(line) == F) {
    lty_rule1 = list(first = c(1:3), 
                     second = c(2,1,3), 
                     third = c(3,1))
    
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(size_rule1x)) {
      for (j in 1:length(size_rule1x[[i]])) {
        Canvas(15, 15)
        DrawEllipse(x = 0, 
                    radius.x = 10, 
                    radius.y = 15, 
                    lwd = 2, 
                    lty = lty_rule1[[i]][[j]], 
                    col=SetAlpha(c(rep("black", 3)), 
                                 col_rule1[[i]][[j]]), 
                    rot = rot_rule1[[i]][[j]])
      }
    }
    
  }
}

stimElli(color = T, rotation = T, line = T)
elliCorrect = function(x, 
                       rotation = NULL, 
                       color = NULL, 
                       line = NULL) {
  if (is.null(rotation) == T & is.null(color) == T & is.null(line) == T) {
    stop("I need arguments")
  }
  rot_rule1 = list(first = c(1:3) * pi/3, 
                   second = c(2,3, 1) * pi/3, 
                   third = c(3, 1)* pi/3)
  col_rule1 = list(first = c(0.10, 0.50, 0.00), 
                   second = c(0.50, 0.00, 0.10), 
                   third = c(0.00, 0.10))
  lty_rule1 = list(first = c(1:3), 
                   second = c(2,1,3), 
                   third = c(3,1))
  if ((is.null(rotation) == F & is.null(color) == T & is.null(line) == T)) {
    rule1_last = rot_rule1[[length(rot_rule1)]] 
    rule1_last = rule1_last[order(rule1_last)]
    rule1_first = rot_rule1[[1]] 
    rule1_first = rule1_first[order(rule1_first)]
    component1 = rule1_first[!rule1_first %in% rule1_last]
    par(mfrow=c(1,1))
    Canvas(15,15)
    DrawEllipse(x = 0, y = 0, radius.x = 10, radius.y = 15, lwd = 2,
                rot = component1, plot = T)
  } else if (is.null(rotation) == F & is.null(color) == F & is.null(line) == T) {
    rule1_last = rot_rule1[[length(rot_rule1)]] 
    rule1_last = rule1_last[order(rule1_last)]
    rule1_first = rot_rule1[[1]] 
    rule1_first = rule1_first[order(rule1_first)]
    component1 = rule1_first[!rule1_first %in% rule1_last]
    
    rule2_last = col_rule1[[length(col_rule1)]] 
    rule2_last = rule2_last[order(rule2_last)]
    rule2_first = col_rule1[[1]] 
    rule2_first = rule2_first[order(rule2_first)]
    component2 = rule2_first[!rule2_first %in% rule2_last]
    
    par(mfrow=c(1,1))
    Canvas(15,15)
    DrawEllipse(x = 0, y = 0, radius.x = 10, radius.y = 15, lwd = 2,
                rot = component1, col = SetAlpha("black", 
                                                 component2) ,  plot = T)
 
  } else if (is.null(rotation) == F & is.null(color) == F & is.null(line) == F) {
    rule1_last = rot_rule1[[length(rot_rule1)]] 
    rule1_last = rule1_last[order(rule1_last)]
    rule1_first = rot_rule1[[1]] 
    rule1_first = rule1_first[order(rule1_first)]
    component1 = rule1_first[!rule1_first %in% rule1_last]
    
    rule2_last = col_rule1[[length(col_rule1)]] 
    rule2_last = rule2_last[order(rule2_last)]
    rule2_first = col_rule1[[1]] 
    rule2_first = rule2_first[order(rule2_first)]
    component2 = rule2_first[!rule2_first %in% rule2_last]
    
    rule3_last = lty_rule1[[length(lty_rule1)]] 
    rule3_last = rule3_last[order(rule3_last)]
    rule3_first = lty_rule1[[1]] 
    rule3_first = rule3_first[order(rule3_first)]
    component3 = rule3_first[!rule3_first %in% rule3_last]
    
    par(mfrow=c(1,1))
    Canvas(15,15)
    DrawEllipse(x = 0, y = 0, radius.x = 10, radius.y = 15, lwd = 2,
                rot = component1, col = SetAlpha("black", 
                                                       component2) ,
                
                lty = component3,  
                plot = T)
  }
}

stimElli(rotation = T, color = T, line=T)
elliCorrect(stimElli(rotation = T, color = T, line = T), 
            rotation = T, color = T, line = T)

rule1_last = rot_rule1[[length(rot_rule1)]] 
rule1_last = rule1_last[order(rule1_last)]
rule1_first = rot_rule1[[1]] 
rule1_first = rule1_first[order(rule1_first)]
component1 = rule1_first[!rule1_first %in% rule1_last]

rule2_last = col_rule1[[length(col_rule1)]] 
rule2_last = rule2_last[order(rule2_last)]
rule2_first = col_rule1[[1]] 
rule2_first = rule2_first[order(rule2_first)]
component2 = rule2_first[!rule2_first %in% rule2_last]

par(mfrow=c(1,1))
Canvas(15,15)
DrawEllipse(x = 0, y = 0, radius.x = 10, radius.y = 15, lwd = 2,
            rot = component1, col = SetAlpha("black", 
                                             component2) ,  plot = T)



rot_rule1 = list(first = c(1:3) * pi/3, 
                 second = c(2,3, 1) * pi/3, 
                 third = c(3, 1)* pi/3)
graph = NULL
temp = NULL
par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
for (i in 1:length(rot_rule1)) {
  for (j in 1:length(rot_rule1[[i]])) {
      Canvas(15, 15)
      temp[[j]] = DrawEllipse(x = 0, y = 0, 
                         rot = rot_rule1[[i]][[j]], 
                         radius.x = 10, radius.y = 15, plot = F)
  }
}

rm(a)
a = stimElli(rotation = T)
par(mfrow=c(3,1))

for(i in 1:length(a)) {
  Canvas(15, 15)
  polygon(a[[i]])
  
}


DrawRegPolygon(radius.x = 10, 
               radius.y = 10, 
               nv=4, rot=pi)

par(mfrow=c(1,1), mar = c(0.5,6,0.5,2)+0.1)
Canvas()
DrawEllipse(rot = c(1:3) * pi/3, col=SetAlpha(c(rep("black", 3)), 
                                              col_rule1[[3]]) )
