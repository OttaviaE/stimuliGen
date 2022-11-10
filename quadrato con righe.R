# nuove forme 20/10/2022
# CODICE PER RIEMPIRE IL QUADRATO -----
library(DescTools) 
# riempo la metà superiore con righe diag 45 gradi
# modifictao draw per farlo
draw(square())
clip(-unlist(square()$size.x)/sqrt(2), unlist(square()$size.x)/sqrt(2), 
     unlist(square()$size.x)/sqrt(2),
     -unlist(square()$size.x)/sqrt(2))
# usando diagline non viene preché in primis riprende il canvas vuoto
# in secundis è gigantesca
draw(diagline.inv(p.x = c(seq(-20, 20, by = 1)),
                  p.y = unlist(square()$size.x)/2), # variando y, cambia la quantità di riempimento
     canvas = F)

# metà inferiore
draw(square())
clip(-unlist(square()$size.x)/sqrt(2), unlist(square()$size.x)/sqrt(2), 
     unlist(square()$size.x)/sqrt(2),
     -unlist(square()$size.x)/sqrt(2))
draw(diagline.inv(p.x = c(seq(-20, 20, by = 1)),
                  p.y = -unlist(square()$size.x)/2), canvas = F)

# solo in alto/solo in basso
draw(square())
clip(-unlist(square()$size.x)/sqrt(2), unlist(square()$size.x)/sqrt(2), 
     unlist(square()$size.x)/sqrt(2),
     -unlist(square()$size.x)/sqrt(2))
draw(diagline.inv(p.x = c(seq(-20, 20, by = 1)),
                  p.y = c(unlist(square()$size.x), 
                          -unlist(square()$size.x))), 
     canvas = F)

# tutto
draw(square())
clip(-unlist(square()$size.x)/sqrt(2), unlist(square()$size.x)/sqrt(2), 
     unlist(square()$size.x)/sqrt(2),
     -unlist(square()$size.x)/sqrt(2))
draw(diagline.inv(p.x = c(seq(-20, 20, by = 1)),
                  p.y = c(unlist(square()$size.x)/2, 
                          -unlist(square()$size.x)/2)), 
     canvas = F)

# facendo il vettore all'interno della funzione vengono righe più karghe

### NON ANDARE OLTRE, ANCORA WORK IN PROGRESS -----


draw(square())
clip(-unlist(square()$size.x)/sqrt(2), unlist(square()$size.x)/sqrt(2), 
     unlist(square()$size.x)/sqrt(2),
     -unlist(square()$size.x)/sqrt(2)) # definisce l'area entro cui disegnare il riempimento
# nota bene che è esattamente il lato del quadrato
# disegna il rimepimento (linea 45 gradi)
DrawRegPolygon(x = c(seq(-20, 20, by = 1)), 
               y = 0, # variando y, cambia la quantità di riempimento
               nv = 2, rot=pi/4, 
               radius.x = 15, 
               radius.y = 15) 
# con y = 0 riempie tutto il quadrato 
draw(square())
clip(-unlist(square()$size.x)/sqrt(2), unlist(square()$size.x)/sqrt(2), 
     unlist(square()$size.x)/sqrt(2),
     -unlist(square()$size.x)/sqrt(2))
DrawRegPolygon(x = c(seq(-20, 20, by = 1)), 
               y = -10, # variando y, cambia la quantità di riempimento
               nv = 2, rot=pi/4, 
               radius.x = 15, 
               radius.y = 15) 
# -10 riempie la metà inferiore



DrawRegPolygon(x = c(seq(-20, 20, by = 1)), 
               y = +10, # variando y, cambia la quantità di riempimento
               nv = 2, rot=pi/4, 
               radius.x = 15, 
               radius.y = 15) 
# 10 riempie la metà inferiore
# si può variare la quantita di quadrato riempito variando la y 





Canvas()
polygon(q) # disegna il quadrato
clip(-0.70, .70, .70, -.70) # definisce l'area entro cui disegnare il riempimento
# nota bene che è esattamente il lato del quadrato

# disegna il rimepimento (linea 135 gradi)
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=3*pi/4, radius.x = 15, radius.y = 15) 

Canvas()
polygon(q) # disegna il quadrato
clip(-0.70, .70, .70, -.70) # definisce l'area entro cui disegnare il riempimento
# nota bene che è esattamente il lato del quadrato

# disegna il rimepimento (90 gradi)
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=pi/2, radius.x = 15, radius.y = 15)



# disegna un cerchio
p = DrawRegPolygon(x = 0, y = 0, radius.x = .70, radius.y = .70,
                   nv = 100, plot = F, rot = pi/4, 
                   lwd = 3, col = "white", lty = 2)

p = DrawRegPolygon(x = 0, y = 0, radius.x = .70, radius.y = .70,
                   nv = 100, plot = T, rot = pi/4, 
                   lwd = 3,  lty = 2, col = "white")

# usr <- par("usr")
# do.call("clip", as.list(usr)) # non serve perché il quadrato è dentro al cerchio
polygon(p, col = "white")

Canvas()
polygon(q)
# rimpimento a sinistra
clip(-0.70, .70, .70, -.70)
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=pi -pi/4, radius.x = 15, radius.y = 15)

# figure complesse --- 
t = DrawRegPolygon(x = 0, y = 0, radius.x = .70, radius.y = 0.7826238,
                   nv = 3,  rot = pi/2, plot = F, col = "white")
r = DrawRegPolygon(x = 0, y = 0, radius.x = .70, radius.y = .70,
                   nv = 4,  rot = pi/2, plot = F, col = "white", lty = 2)

par(mfrow=c(1,3), mar = c(0.5,6,0.5,2)+0.1)
Canvas()
polygon(q, lwd = 2)
clip(-0.70, .70, .70, -.70) # definisce l'area entro cui disegnare il riempimento
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=pi/4, radius.x = 15, radius.y = 15) 
polygon(p, col = "white", lwd =2)

Canvas()
polygon(q, lwd = 2)
clip(-0.70, .70, .70, -.70)
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=pi - pi/4, radius.x = 15, radius.y = 15) 
polygon(t, col = "white", lwd=2)

Canvas()
polygon(q, lwd = 2)
clip(-0.70, .70, .70, -.70)
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=pi - pi/4, radius.x = 15, radius.y = 15) 
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=pi/4, radius.x = 15, radius.y = 15) 

polygon(r, col = "white", lwd=2)

# figure complesse separate 
par(mfrow=c(1,1), mar = c(0.5,6,0.5,2)+0.1)
Canvas()
polygon(q, lwd = 2)
clip(-0.70, .70, .70, -.70) # definisce l'area entro cui disegnare il riempimento
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=pi/4, radius.x = 15, radius.y = 15) 
polygon(p, col = "white", lwd =2)

Canvas()
polygon(q, lwd = 2)
clip(-0.70, .70, .70, -.70)
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=pi - pi/4, radius.x = 15, radius.y = 15) 
polygon(t, col = "white", lwd=2)

Canvas()
polygon(q, lwd = 2)
clip(-0.70, .70, .70, -.70)
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=pi - pi/4, radius.x = 15, radius.y = 15) 
DrawRegPolygon(x = c(seq(-12, 12, by = .10)), y = 1, 
               nv = 2, rot=pi/4, radius.x = 15, radius.y = 15) 

polygon(r, col = "white", lwd=2)




# quadrato con sfondo della stessa grandezza di square -----
Canvas(15, 15, bg = "white")
Canvas(15, 15)
DrawRegPolygon(radius.x = square()$size.x, 
               radius.y = square()$size.y, 
               lwd = square()$lwd, 
               lty = square()$lty, nv = square()$nv, rot = square()$rotation)
# in questo modo si fa una clip dell'area di grafico che è uguale al lato del quadrato
# inscritto nel ceerchio di raggio 15
clip(-square()$size.x/sqrt(2), square()$size.x/sqrt(2), 
     square()$size.x/sqrt(2),
     -square()$size.x/sqrt(2))

DrawRegPolygon(x = seq(-20, 20, by = 1), y = 0, 
               nv = 2, rot=pi - pi/4, radius.x = square()$size.x, 
               radius.y = square()$size.y) 



# LE FUNZIONI PER IL PACCHETTO da sistemare -------
#' Square with left lines
#'
#' @return Return the square with left lines object
#' @examples
#' diag.left()
#' @export
diag.left <- function(vis = 1, move.diag = 1) {
  value <- list(
    shape = "diag.left",
    size.x = square()$size.x,
    size.y = square()$size.y,
    rotation = pi -pi/4,
    pos.x = (- square()$size.x-5) + move.diag,
    pos.y = 0,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 2,
    visible = vis
  )
  attr(value, "class") <- "field"
  value
}

#' Default square with left filling 
#'
#' @return Return the default square with left filling
#' @examples
#' square.left()
#' @export
square.left <- function() {
   value = diag.left()
  for (i in 1:length(seq(-20, 20, by = 1))) {
    value = cof(value, diag.left(move.diag = i))
  }
  attr(value, "class") <- "field"
  value
}

Canvas(15,15)
DrawRegPolygon(radius.x = square()$size.x, 
               radius.y = square()$size.y, 
               lwd = square()$lwd, 
               lty = square()$lty, nv = square()$nv, rot = square()$rotation)
square.left()
clip(-square()$size.x/sqrt(2), square()$size.x/sqrt(2), 
     square()$size.x/sqrt(2),
     -square()$size.x/sqrt(2))
# funziona 
DrawRegPolygon(radius.x = square.left()$size.x, 
               radius.y = square.left()$size.y, 
               x = square.left()$pos.x, 
               y = square.left()$pos.y,
               lwd = square.left()$lwd, 
               lty = square.left()$lty, nv = square.left()$nv, 
               rot = square.left()$rotation)

# non nella matrice
M<-Raven(st1=square.left(),hrule=c("identity"),vrule=c("identity"))
clip(-square()$size.x/sqrt(2), square()$size.x/sqrt(2), 
     square()$size.x/sqrt(2),
     -square()$size.x/sqrt(2))
draw(M)

# Non guadare perché non ha senso -----

#' Square with right lines
#'
#' @return Return the square with left right object
#' @examples
#' square.right()
#' @export
square.right <- function(vis = 1) {
  value <- list(
    shape = "square.right",
    size.x = 15,
    size.y = 15,
    rotation = pi -pi/4,
    pos.x = c(seq(-12, 12, by = .10)),
    pos.y = 1,
    lty = 1,
    lwd = 3,
    num = 1,
    nv = 5,
    visible = vis
  )
  attr(value, "class") <- "field"
  value
}
