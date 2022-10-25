library(DescTools)
Canvas(20, 20, bg = "white")
radius.x = 2
radius.y = 6
start.x = 1
start.y = 1

# Angolo retto tocca alla perfezione
# ELICA --------
helix = NULL
helix[[1]] = DrawRegPolygon(x= start.x, y = start.y, 
                             radius.x = radius.x, 
                             radius.y = radius.y,
                             nv = 100, plot = F)

helix[[2]] = DrawRegPolygon(y = helix[[1]]$y[75], 
                             x = radius.y + start.x + start.x/2,
                             radius.x = radius.x, 
                             radius.y = radius.y,
                             nv = 100, 
               rot = ((2*pi)/360)*90, plot = FALSE)
helix[[3]] = DrawRegPolygon(y = helix[[1]]$y[75], 
                             x = - radius.y + start.x/2,  
                             radius.x = radius.x, 
                             radius.y = radius.y,
                             nv = 100, 
               rot = ((2*pi)/360)*90, plot = FALSE)
helix[[4]] = DrawRegPolygon(y = 2*helix[[1]]$y[75] - start.y, x =start.x,  
                             radius.x = radius.x, radius.y = radius.y,
                             nv = 100, 
                             plot = FALSE)

Canvas(20, 20)
for (i in 1:length(helix)) {
  polygon(helix[[i]], lwd = 2)
}

# Fiorellino ------ 
Canvas(20, 20)
flower = NULL
flower[[1]] = DrawRegPolygon(x= start.x, y = start.y, 
                            radius.x = radius.x, 
                            radius.y = radius.y,
                            nv = 100, plot = F)

flower[[2]] = DrawRegPolygon(y = flower[[1]]$y[75]- radius.y/2, 
                            x = radius.y +start.x/2,
                            radius.x = radius.x, 
                            radius.y = radius.y,
                            nv = 100, 
                            rot = ((2*pi)/360)*60, plot = F)
flower[[3]] = DrawRegPolygon(y = flower[[1]]$y[75] - radius.y/2, 
                            x = - radius.y +start.x+start.x/2,  
                            radius.x = radius.x, 
                            radius.y = radius.y,
                            nv = 100, 
                            rot = ((2*pi)/360)*120, plot = F)

Canvas(20, 20)
for (i in 1:length(flower)) {
  polygon(flower[[i]], lwd = 4)
}


# opzione due per il fiorellino/elica -----
Canvas(3, 3)
# mi sono disegnata i cerchi per aiutarmi a capire dove mettere gli archi, 
# ma si può fare solo l'arco (per questo è il codice dei cerchi è commentato)
# primo cerchio
DrawCircle(x = 0, y =0, r.out = 1)
# cerchio sx
DrawCircle(x = -1.4, y =0, r.out = 1)
# cerchio sx in alto
DrawCircle(x = -1.4, y =1.4, r.out = 1)
# cerchio alto
DrawCircle(x = 0, y =1.4, r.out = 1)
DrawRegPolygon(x = 0, y =1.4, radius.x = 1, radius.y = 1, nv = 100, 
               col = NA)
Canvas(15,15)
DrawCircle(x = 0, y =0, r.out = 8)
DrawCircle(x = -11.2, y =0, r.out = 8)
DrawCircle(x = 0, y = -11.2, r.out = 8)
DrawCircle(x = -11.2, y =-11.2, r.out = 8)
# disegno gli archi con queste coordinate che funzionano per motivi a me oscuri
# arco sx v 
# DrawArc(x = 0, y = 0, rx = 8,
#         theta.1 = 3*pi/4, 
#         theta.2 = 5*pi/4, col="red", nv = 100, lwd = 3)
# # arco dx v
# DrawArc(x = -11.2, y = 0, rx = 8,
#         theta.1 = 7*pi/4, 
#         theta.2 = pi/4, col="red", nv = 100, lwd = 3)
# arco sx left down v
# DrawArc(x = 0, y = -11.2, rx = 8,
#         theta.1 = 3*pi/4, 
#         theta.2 = 5*pi/4,col="red", nv = 100, lwd = 3)
# arco dx right down v
# DrawArc(x = -11.2, y = -11.2, rx = 8,
#         theta.1 = 7*pi/4, 
#         theta.2 = pi/4,col="red", nv = 100, lwd = 3)
# h left up 
# DrawArc(x = -11.2, y = -11.2, rx = 8,
#         theta.1 = pi/4, 
#         theta.2 = 3*pi/4,col="red", nv = 100, lwd = 3)
# h right up
# DrawArc(x = 0, y = -11.2, rx = 8,
#         theta.1 = pi/4, 
#         theta.2 = 3*pi/4,col="red", nv = 100, lwd = 3)
# h arco dx down
# DrawArc(x = -11.2, y = 0, rx = 8,
#         theta.1 = 5*pi/4, 
#         theta.2 = 7*pi/4, col="red", nv = 100, lwd = 3)
# h arco sx down 
# DrawArc(x = 0, y = 0, rx = 8,
#         theta.1 = 5*pi/4, 
#         theta.2 = 7*pi/4, col="red", nv = 100, lwd = 3)
# traduco in DrawCircle 
Canvas(15, 15)
#arco sx v up: v.arc.left.up
DrawCircle(x = 5, y = 5, r.out = square()$size.x/2, 
border="red", nv = 100, lwd = 3)
DrawCircle(x = 5, y = 5, r.out = square()$size.x/2, 
           r.in = square()$size.y/2,
        theta.1 = 3*pi/4,
        theta.2 = 5*pi/4, col="red", nv = 100, lwd = 3)
# arco dx v up: v.arc.right.up
DrawCircle(x = -5.5, y = 5,r.out = square()$size.x/2, 
border="red", nv = 100, lwd = 3)
DrawCircle(x = -5.5, y = 5,r.out = square()$size.x/2, 
           r.in = square()$size.y/2,
        theta.1 = 7*pi/4,
        theta.2 = pi/4, col="red", nv = 100, lwd = 3)
#arco sx left down v: v.arc.left.down
DrawCircle(x = 5.1, y = -5.5, r.out = square()$size.x/2,
           border="red", nv = 100, lwd = 3)
DrawCircle(x = 5.1, y = -5.5, r.out = square()$size.x/2, r.in = square()$size.x/2,,
        theta.1 = 3*pi/4,
        theta.2 = 5*pi/4,col="red", nv = 100, lwd = 3)
#arco dx right down v: v.arc.right.down
DrawCircle(x = -5.5, y = -5.5, r.out = square()$size.x/2, 
           border="red", nv = 100, lwd = 3)
DrawCircle(x = -5.5, y = -5.5,  r.out =  square()$size.x/2, r.in =  square()$size.x/2,
        theta.1 = 7*pi/4,
        theta.2 = pi/4,col="red", nv = 100, lwd = 3)
#h left up: h.arc.left.up
DrawCircle(x = -5.5, y = -5.5, r.out =  square()$size.x/2, r.in =  square()$size.x/2,
        theta.1 = pi/4,
        theta.2 = 3*pi/4,col="red", nv = 100, lwd = 3)
#h right up: h.arc.right.up
DrawCircle(x = 5.1, y = -5.5, r.out =  square()$size.x/2, r.in =  square()$size.x/2,
        theta.1 = pi/4,
        theta.2 = 3*pi/4,col="red", nv = 100, lwd = 3)
#h arco sx down: h.arc.left.down
DrawCircle(x = -5.5, y = 5, r.out =  square()$size.x/2, r.in =  square()$size.x/2,
        theta.1 = 5*pi/4,
        theta.2 = 7*pi/4, col="red", nv = 100, lwd = 3)
#h arco dx down: h.arc.right.down
DrawCircle(x = 5, y = 5, r.out =  square()$size.x/2, r.in =  square()$size.x/2,
        theta.1 = 5*pi/4,
        theta.2 = 7*pi/4, col="red", nv = 100, lwd = 3)

# h arc right up
arc1 = DrawArc(x = 0, y = 0, rx = 1, 
               theta.1 = pi/4, theta.2 = 3*pi/4, col="red", lwd = 3, 
               plot = T)
polygon(arc1$x, arc1$y, col = "red")

# h arc left up 
DrawArc(x = -1.4, y = 0, rx = 1,
        theta.1 = pi/4, theta.2 = 3*pi/4, col="red", nv = 100, lwd = 3)
# h arc left down
DrawArc(x = -1.4, y = 1.4, rx = 1, 
        theta.1 = 5*pi/4, theta.2 =  7*pi/4, col="red", nv = 100, lwd = 3)
# h arc right down
DrawArc(x = 0, y = 1.4, rx = 1, 
        theta.1 = 5*pi/4, theta.2 =  7*pi/4, col="red", nv = 100, lwd = 3)
#v arc left up
DrawArc(0, 1.4, rx = 1,
        theta.1 = 3*pi/4, theta.2 = 5*pi/4, nv = 100, 
        col = "red", lwd = 3)
# v arc right up
DrawArc(-1.4, 1.4, rx = 1,
        theta.1 = 7*pi/4, theta.2 = pi/4, nv = 100, 
        col = "red", lwd = 3)
# v arc down left
DrawArc(0, 0, rx = 1,
        theta.1 = 3*pi/4, theta.2 = 5*pi/4, nv = 100, 
        col = "red", lwd = 3)
# v arc down r
DrawArc(-1.4, 0, rx = 1,
        theta.1 = 7*pi/4, theta.2 = pi/4, nv = 100, 
        col = "red", lwd = 3)
arca = DrawArc(-1.4, 0, rx = 1,
               theta.1 = 7*pi/4, theta.2 = pi/4, nv = 100, 
               col = "red", lwd = 3, plot = F)
Canvas(3,3)
polygon(arca[[1]]$x, arca[[1]]$y) # per ragioni che non so, mi disegna proprio 
# la sezione del cerchio. 

# stella ----
# di fatto, due triangoli
Canvas(3,3)
DrawRegPolygon(radius.x = 3, nv = 6, 
               radius.y = 3,  col = NA, rot = 0)
DrawRegPolygon(x=0, y = 0, radius.x = 3, nv = 6, 
               radius.y = 3, rot = 3*pi/2, col = NA)

# la figata di DrawArc è che puoi stabilire il numero di vertici -----
Canvas(3,3)
DrawArc(x = 0, y = 0, rx = 1, 
               theta.1 = pi/4, theta.2 = 3*pi/4, col="red", lwd = 3, 
        nv = 3, 
               plot = T)

DrawArc(x = -1.4, y = 0, rx = 1,
        theta.1 = pi/4, theta.2 = 3*pi/4, col="red", nv = 3, lwd = 3)
DrawArc(x = -1.4, y = 1.4, rx = 1, 
        theta.1 = 5*pi/4, theta.2 =  7*pi/4, col="red", nv = 3, lwd = 3)
DrawArc(x = 0, y = 1.4, rx = 1, 
        theta.1 = 5*pi/4, theta.2 =  7*pi/4, col="red", nv = 3, lwd = 3)
DrawArc(0, 1.4, rx = 1,
        theta.1 = 3*pi/4, theta.2 = 5*pi/4, nv = 3, 
        col = "red", lwd = 3)
DrawArc(-1.4, 1.4, rx = 1,
        theta.1 = 7*pi/4, theta.2 = pi/4, nv = 3, 
        col = "red", lwd = 3)
DrawArc(0, 0, rx = 1,
        theta.1 = 3*pi/4, theta.2 = 5*pi/4, nv = 3, 
        col = "red", lwd = 3)
DrawArc(-1.4, 0, rx = 1,
        theta.1 = 7*pi/4, theta.2 = pi/4, nv = 3, 
        col = "red", lwd = 3)


# la buona notizia è che ho scoperto come si colora una sezione del cerchio ----
Canvas(xpd = T)
DrawCircle(x = -1.4, y = 0, r.out  = 1,
           theta.1 = pi/4, theta.2 = 3*pi/4, col="red")
Canvas(xpd = T)
# si può fare l'archetto anche con DrawCircle!!! mi viene da piangere
arc = DrawCircle(x = -1.4, y = 0, r.out  = 1,r.in=1,
                 theta.1 = pi/4, theta.2 = 3*pi/4, border="red", plot = F)
polygon(arc[[1]]$x, arc[[1]]$y)

# fiorellino bello fatto con DrawCircle & polygon
Canvas(3, xpd = T)

lily = NULL
lily[[1]] = DrawCircle(x = -1.4, y = 0, r.out  = 1,r.in=1,
                       theta.1 = pi/4, theta.2 = 3*pi/4, border="red",
                       lwd = 3, plot = F)
lily[[2]] = DrawCircle(x = -1.4, y = 1.4, r.out  = 1,r.in=1,
                       theta.1 = 5*pi/4, theta.2 =  7*pi/4,border="red",
                       lwd = 3, plot = F)
lily[[3]] = DrawCircle(x = 0, y = 1.4,r.out  = 1,r.in=1,
                       theta.1 = 5*pi/4, theta.2 =  7*pi/4, 
                       border="red",lwd = 3, plot = F)

lily[[4]] = DrawCircle(0, 1.4, r.out  = 1,r.in=1,
                       theta.1 = 3*pi/4, theta.2 = 5*pi/4, 
                       border="red", lwd = 3, plot = F)
lily[[5]] = DrawCircle(-1.4, 1.4, r.out  = 1,r.in=1, theta.1 = 7*pi/4, theta.2 = pi/4, nv = 100, 
                       border="red", lwd = 3, plot = F)

lily[[6]] = DrawCircle(0, 0, r.out  = 1,r.in=1,
                       theta.1 = 3*pi/4, theta.2 = 5*pi/4,
                       border="red", lwd = 3, plot = F)
lily[[7]] = DrawCircle(-1.4, 0,r.out  = 1,r.in=1,
                       theta.1 = 7*pi/4, theta.2 = pi/4, 
                       border="red", lwd = 3, plot = F)
lily[[8]] = DrawCircle(x = 0, y = 0,r.out  = 1,r.in=1,
                       theta.1 = pi/4, theta.2 = 3*pi/4, border="red", lwd = 3, 
                       plot = F)
Canvas(3,3)
for (i in 1:length(lily)) {
  polygon(lily[[i]][[1]]$x, lily[[i]][[1]]$y, lwd = 3)
}




# archetti ------
Canvas(2, 2)
DrawCircle(x = 0, y = 0, r.out = 1.4)
DrawArc(x = 0, y = 0, rx = 1, 
        theta.1 = 5*pi/6, theta.2 = 5*pi/4, col="red", nv = 100)
DrawCircle(x = -1.4, y = 1.4, r.out = 1.4)
DrawArc(x = -1.4, y = 1.4, rx = 1, 
        theta.1 =  5*pi/3, theta.2 = pi/12 , col="red", nv = 100)


DrawArc(x = pi/2, y = 0, rx = 1, theta.1 = pi/4, theta.2 = 3*pi/4, col="red")

Canvas(2, 2)
# DrawCircle(x = 0, y =0, r.out = 1)
DrawArc(x = 0, y = 0, rx = 1, 
        theta.1 = pi/4, theta.2 = 3*pi/4, col="red", nv = 100, lwd = 3)
# DrawCircle(x = -1.4, y =0, r.out = 1)
# DrawArc(x = -1.4, y = 0, rx = 1, 
#         theta.1 = pi/4, theta.2 = 3*pi/4, col="red", nv = 100, lwd = 3)
# DrawCircle(x = -1.4, y =1.4, r.out = 1)
DrawArc(x = -1.4, y = 1.4, rx = 1, 
        theta.1 = 5*pi/4, theta.2 =  7*pi/4, col="red", nv = 100, lwd = 3)
# mi fa impazzire che le sue stanghette non si toccano, se ci mettessimo un 
# pallino? non appena trovo le maledette coordinate



polygon(lily[[1]][[1]]$x, lily[[1]][[1]]$y)


polygon(x = unlist(lapply(p, "[", "x")),
        y=unlist(lapply(p, "[", "y")), col="green", border=FALSE)







Canvas(20, 20)
DrawRegPolygon(radius.x = square()$size.x, 
               radius.y = square()$size.y,
               x = -0.1,
               y = -0.1,
               lwd = square()$lwd, 
               lty = square()$lty, nv = square()$nv, 
               rot = square()$rotation, 
               col = SetAlpha("black", 1))
Canvas(20, 20)
DrawCircle(x = c(-6.53,6.53, 4.67), 
           y = c(-6.53,4.67, -4.67), 
           r.out = 7.5, 
           r.in = 7.5, 
           theta.1 = lily()$theta.1, 
           theta.2 = lily()$theta.2, 
           border =  "red", lwd = 3)


Canvas(15, 15)
#arco sx v up: v.arc.left.up
DrawCircle(x = 5, y = 5, r.out = square()$size.x/2, 
           border="white", nv = 100, lwd = 3)
DrawCircle(x = 5, y = 5, r.out = square()$size.x/2, 
           r.in = square()$size.y/2,
           theta.1 = 3*pi/4,
           theta.2 = 5*pi/4, border="red", nv = 100, lwd = 3)
# arco dx v up: v.arc.right.up
# DrawCircle(x = -5.5, y = 5,r.out = square()$size.x/2, 
#            border="red", nv = 100, lwd = 3)
DrawCircle(x = -5.5, y = 5,r.out = square()$size.x/2, 
           r.in = square()$size.y/2,
           theta.1 = 7*pi/4,
           theta.2 = pi/4, border="white", nv = 100, lwd = 3)
#arco sx left down v: v.arc.left.down
# DrawCircle(x = 5.1, y = -5.5, r.out = square()$size.x/2,
#            border="red", nv = 100, lwd = 3)
DrawCircle(x = 5.1, y = -5.5, r.out = square()$size.x/2, r.in = square()$size.x/2,
           theta.1 = 3*pi/4,
           theta.2 = 5*pi/4,border="white", nv = 100, lwd = 3)
#arco dx right down v: v.arc.right.down
# DrawCircle(x = -5.5, y = -5.5, r.out = square()$size.x/2, 
#            border="red", nv = 100, lwd = 3)
DrawCircle(x = -5.5, y = -5.5,  r.out =  square()$size.x/2, r.in =  square()$size.x/2,
           theta.1 = 7*pi/4,
           theta.2 = pi/4,border="white", nv = 100, lwd = 3)
#h left up: h.arc.left.up
DrawCircle(x = -5.5, y = -5.5, r.out =  square()$size.x/2, r.in =  square()$size.x/2,
           theta.1 = pi/4,
           theta.2 = 3*pi/4,border="white", nv = 100, lwd = 3)
#h right up: h.arc.right.up
DrawCircle(x = 5.1, y = -5.5, r.out =  square()$size.x/2, r.in =  square()$size.x/2,
           theta.1 = pi/4,
           theta.2 = 3*pi/4,border="white", nv = 100, lwd = 3)
#h arco sx down: h.arc.left.down
DrawCircle(x = -5.5, y = 5, r.out =  square()$size.x/2, r.in =  square()$size.x/2,
           theta.1 = 5*pi/4,
           theta.2 = 7*pi/4, border="white", nv = 100, lwd = 3)
#h arco dx down: h.arc.right.down
DrawCircle(x = 5, y = 5, r.out =  square()$size.x/2, r.in =  square()$size.x/2,
           theta.1 = 5*pi/4,
           theta.2 = 7*pi/4, border="white", nv = 100, lwd = 3)

# provo con il quadrato pieno 

Canvas(20, 20)
q = DrawRegPolygon(radius.x = square()$size.x, radius.y = square()$size.y,
                   nv = square()$nv, 
                   plot = T, rot = pi/4) # crea le coordinate del quadrato
clip(-square()$size.x/sqrt(2), square()$size.x/sqrt(2), 
     square()$size.x/sqrt(2),
     -square()$size.x/sqrt(2)) # definisce l'area entro cui disegnare il riempimento
DrawRegPolygon(x = c(seq(-25, 25, by = 1)), y = 1, 
               nv = 2, rot=pi/4, radius.x = 15, radius.y = 15)
Canvas()
polygon(q) # disegna il quadrato






