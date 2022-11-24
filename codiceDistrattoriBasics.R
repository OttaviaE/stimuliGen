rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods.R")
source("Class and Methods extension.R")
source("Rules_27102022.R")

m1 = apply(Raven(st1 = cof(dot(), 
                           s.lily(), 
                           square(s.x = 5, s.y = 5, 
                                  shd = "black", rot = pi/2)), 
                 hrule = "diff_shapes"))
draw(m1)           
m2 = apply(Raven(st1=pentagon(),
                 hrule=c("identity"),
                 vrule=c("identity")))
draw(m2)
mix = (com(m2, m1))

m3 = apply(Raven(st1=cof(pentagon(), e.hexagon(), triangle()),
                 hrule=c("diff_shapes"),
                 vrule=c("lty")))
draw(m3)
unlist(m3[[1:9]])

# funcione risposta corretta matrice 
correct = function(m) {
  correct = m$Sq9 
  return(correct)
}

resp.correct = correct(mix)
draw(mix, hide = T)
draw(resp.correct)

# repetion ----
# selezione casuale del distrattore. volendo si può anche fare che si sceglie 
# quale distrattore prendere 
# questo codice controlla anche che nessuna cella sia uguale alla risposta corretta
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

d.r = repetition(mix, which = "all")
draw(d.r)
draw(repetition(M1, which = "top"))

draw(repetition(M2, which = "top"), canvas = F)

m3 = com(M1, M2)
draw(repetition(m3, which = "top"))
d3 = repetition(m3, which = "all")
draw(d3$r.diag)
# wp ----
# partiamo dal copy che è il più facile 
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

d.wp = wp(mix, which = "all")
draw(d.wp$wp.copy)
draw(d.wp$wp.matrix)

# difference -----
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
draw(d.union(m3,  n = 3))



# gli archetti si possono togliere 


draw(obj)
d.union = cof(d.union, obj)
