#23/09/2022
# relazione a tre vie tra gli stimoli
# teroicamente basta una permutazione delle regole già esistenti 
# come non detto, l'unica random è la prima riga, le altre due devono seguire 
# quello che fa la prima 

h = 0; d= pi/3; v = pi/2
sw = 0; sg = 0.10; sb = 0.6

# which_line 
s = 1; da = 5; do = 3

htv = c(h = 0, d= pi/3, v = pi/2)
vth = c(v = pi/2, d = pi/3, h = 0)

wtb = c(sw = 0, sg = 0.10, sb = 0.6)
btw = c(sb = 0.6, sg=0.10, sw = 0)

sdad = c(s = 1, da =5, do = 3)
dasd = c(da = 5, s = 1, do = 3)


start = list(rot = htv, 
             shade = btw, 
             line = sdad)


rest = list(rot = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
              shade =Permn(start[["shade"]])[c(seq(1,5,by=2)), ], 
              line = Permn(start[["line"]])[c(seq(1,5,by=2)), ])

par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
for (i in 1:length(rest)) {
  for (j in 1:length(rest[[i]])) {
    temp = NULL
    Canvas(15, 15)
    DrawEllipse(x = 0,
                radius.x = 10, 
                radius.y = 15, 
                lwd = 2, 
                rot = rest[["rot"]][[j]], 
                col = SetAlpha("black",rest[["shade"]][[j]]), 
                plot = T, lty = rest[["line"]][[j]])
  }
} 

par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
for (i in 1:length(rest)) {
  for (j in 1:length(rest[[i]])) {
    temp = NULL
    Canvas()
    DrawRegPolygon(rot = rest[["rot"]][[j]],
                   col = SetAlpha("black",rest[["shade"]][[j]]),
                   nv=3, lwd = 2, lty = rest[["line"]][[j]])
  }
}
