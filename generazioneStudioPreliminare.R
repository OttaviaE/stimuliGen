rm(list = ls())
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")

# distrattori (funzioni) -----

# funcione risposta corretta matrice 
correct = function(m) {
  correct = m$Sq9 
  return(correct)
}

repetition = function(m,which = c("all", "top", "diag", "left")) {
  m.correct = correct(m)
  distr.repetition = list(  r.top = m$Sq6,
                            r.diag = m$Sq5,
                            r.left = m$Sq8)
  if (which == "all") {
    if (any(unlist(distr.repetition$r.top) != unlist(m.correct), na.rm = T) == F) {
      sample.index = c(2:3)
    } else if (any(unlist(distr.repetition$r.left) != unlist(m.correct), na.rm = T) == F) {
      sample.index = c(1:2)
    } else if (any(unlist(distr.repetition$r.diag) != unlist(m.correct), na.rm = T) == F) {
      sample.index = c(1,3)
    } else {
      sample.index = c(1:3)
    }
    sample.index = sample(sample.index)[1]
    # distr.repetition = distr.repetition[[sample.index]]
    
  } else if (which == "top") {
    distr.repetition = distr.repetition$r.top
  } else if (which == "left") {
    distr.repetition = distr.repetition$r.left
  } else if (which == "diag") {
    distr.repetition = distr.repetition$r.diag
  }
  return(distr.repetition)
}

wp = function(m, which = c("all", "copy", "matrix")) {
  sample.index = sample(c(1:4, 7))[1] # non è random
  distr.wp.copy = m[[sample.index]]
  distr.wp.matrix = m$Sq1
  for (i in 2:8) {
    distr.wp.matrix = cof(distr.wp.matrix, m[[i]])
  }
  if (which == "all") {
    distr.wp = list(wp.copy = distr.wp.copy, 
                    wp.matrix = distr.wp.matrix)
  } else if (which == "copy") {
    distr.wp = distr.wp.copy
  } else if (which == "matrix") {
    distr.wp = distr.wp.matrix
  }
  
}

d.union = function(m, n = 1) {
  n = n
  d.union = m$Sq1
  for (i in 2:8) {
    d.union = cof(d.union, m$Sq1)
  }
  shapes = shapes_list("Shapes_list-10-11-Ottavia.R")
  shapes = shapes[-grep("arc", shapes$name), ]
  exclusion = shapes[!shapes$name %in% unlist(d.union$shape), ]
  random<-sample(1:length(exclusion$name),n)
  for(i in 1:length(random)) {
    f<-get(exclusion$name[random[i]])
    if(i==1){
      obj<-f()
    }else{
      obj<-cof(obj,f())
    }
  }
  d.union = cof(d.union, obj)
  return(d.union)
}

ic.flip = function(m, rot = pi/4) {
    resp.c = correct(m)
    p = resp.c$visible # prende quali sono gli elementi vsibili
    index.ic = which(p == 1) # devo fare il codice per quando ci sono più elementi
    ic.flip = resp.c 
    ic.flip[["rotation"]][[index.ic]] = ic.flip[["rotation"]][[index.ic]] + rot
  
  return(ic.flip)
}


# generazione di matrici per lo studio dei distrattori ---- 
# gruppo A 
a5_sh_rot_size = apply(Raven(
  st1 = cof(ellipse(), triangle(), luck()), 
  vrule= c("diff_shapes",  "size"), 
  hrule = c( "rotation","diff_shapes")
))
# QUADRATO NO
m = apply(Raven(
  st1 = cof(triangle(),ellipse(rot = pi/2),  e.hexagon(rot=pi/3)), 
  hrule= c("diff_shapes",   "size"), 
  vrule = c("diff_shapes",  "rotation")
))
draw(m)
m1 = m$Sq1

draw(a5_sh_rot_size, hide = F)

dist_a5 = list(correct = correct(a5_sh_rot_size))
rep_a5 = repetition(a5_sh_rot_size, 
                    which = "all")

dist_a5[["rep"]] = rep_a5[[2]]
draw(dist_a5$rep)

wp_a5 = wp(a5_sh_rot_size, 
           which = "all")


dist_a5[[3]] = wp_a5$wp.copy
dist_a5[[4]] = d.union(a5_sh_rot_size, 
                       n =1)
draw(dist_a5[[4]])


dist_a5[[5]] = ic.flip(a5_sh_rot_size)
dist_a5[[6]] = wp_a5$wp.matrix

dist_a5 = sample(dist_a5)

par(mfcol = c(2,3), mai = c(1, 5, 1, 5))
for (i in 1:length(dist_a5)) {
  draw(dist_a5[[i]])
}


# non ho ancora scritto il codice per incomplete correlate.
# facci
  
# distrattori ---


b5_sh_rot_size = apply(Raven(
  st1 = cof(square(), triangle(), e.hexagon()), 
  vrule= c("diff_shapes",  "size"), 
  hrule = c("diff_shapes", "rotation")
))

draw(b5_sh_rot_size, hide = T)

# il biscotto non ruota, questa non va bene
a5a_sh_rot_size = apply(Raven(
  st1 = cof(ellipse(shd="black"), 
            u.star(), square(shd = "black")), 
  vrule= c("diff_shapes", "rotation", "size"), 
  hrule = c("diff_shapes", "rotation")
))
draw(a5a_sh_rot_size, hide = F)

b5a_sh_rot_size = apply(Raven(
  st1 = cof(pentagon(shd="black"), 
            u.star(), square(shd = "black")), 
  vrule= c("diff_shapes", "rotation"), 
  hrule = c("diff_shapes", "rotation", "size")
))
draw(b5a_sh_rot_size, hide = F)

# prova trasformazion mentale ---- 
# intanto, non si riesce a modificare la linea sul lily singolo 
# e questo non va bene. va specificato l'ordine con cui voglio le righe
# nello specifico che voglio la prima, la seconda ae la terza devono 
# essere uguali
p = apply(Raven(st1 = cof(circle(), 
                          slice(), 
                          circle()),
          hrule = c("diff_shapes", "lty"), 
          vrule = "identity"))
draw(p)
