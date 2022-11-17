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
# funcione risposta corretta matrice 
correct = function(m) {
  correct = m$Sq9 
  return(correct)
}

resp.correct = correct(mix)
draw(mix, hide = T)


# repetion ----
# selezione casuale del distrattore. volendo si può anche fare che si sceglie 
# quale distrattore prendere 
# questo codice controlla anche che nessuna cella sia uguale alla risposta corretta
repetition = function(m) {
  m.correct = correct(m)
  distr.repetition = list(  r.top = m$Sq6,
                            r.diag = m$Sq5,
                            r.left = m$Sq8)
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
  distr.repetition = distr.repetition[[sample.index]]
  return(distr.repetition)
}

d.r = repetition(mix)
draw(d.r)





draw(repetition(mix, which = "diag"))
distr.repetition
