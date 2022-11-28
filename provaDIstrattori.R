# funzioni distrattori ---
rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")

# risposta corretta ---
correct = function(m) {
  correct = m$Sq9 
  return(correct)
}

# # funzioni per i distrattori singoli
# wp = function(m) {
#   # wrong principle copy
#   sample.index = sample(c(1:4, 7)) # non è random
#   s = sample.index[1]
#   distr.wp.copy = m[[s]]
#   distr.wp.matrix = m$Sq1
#   for (i in 2:8) {
#     distr.wp.matrix = cof(distr.wp.matrix, 
#                           m[[i]])
#   }
#   distr.wp = list(wp.copy = distr.wp.copy, 
#                   wp.matrix = distr.wp.matrix)
#   return(distr.wp)
#   
# }


repetition = function(m) {
  m.correct = correct(m)
  distr.repetition = list(  r.top = m$Sq6,
                            r.diag = m$Sq5,
                            r.left = m$Sq8)
  if (any(unlist(distr.repetition$r.top) != unlist(m.correct),
          na.rm = T) == F) {
    warning("R-Top is equal to the correct response")
    flag = "r.top"
  } else if (any(unlist(distr.repetition$r.left) != unlist(m.correct),
                 na.rm = T) == F) {
    warning("R-left is equal to the correct response")
    flag = "r.left"
  } else if (any(unlist(distr.repetition$r.diag) != unlist(m.correct),
                 na.rm = T) == F) {
    warning("R-diag is equal to the correct response")
    # distr.repetition = distr.repetition[[sample.index]]
    flag = "r.diag"
  }
  return(distr.repetition)
}


wp = function(m, which = "all") {
  m.correct = correct(m)
  check.rep = repetition(m)
  sample.index = sample(c(1:4, 7)) # non è random
  s = sample.index[1]
  distr.wp.copy = m[[s]]
  distr.wp.matrix = m$Sq1
  for (i in 2:8) {
    distr.wp.matrix = cof(distr.wp.matrix,
                          m[[i]])
  }
  if (any(unlist(distr.wp.copy) != unlist(m.correct),
          na.rm = T) == F) {
    warning("WP-Copy is equal to the correct response!")
  }
  distr.wp = list(wp.copy = distr.wp.copy,
                  wp.matrix = distr.wp.matrix)

}


# difference -----
d.union = function(m, n = 1, 
                   shapes.out = NULL, 
                   shapes.in = NULL) {
  n = n
  d.union = m$Sq1
  for (i in 2:8) {
    d.union = cof(d.union, m[[i]])
  }
  shapes.l = shapes_list("Shapes_list-10-11-Ottavia.R")
  shapes.l = shapes.l[-grep("arc", shapes.l$name), ]
  if (is.null(shapes.out) == F) {
    exclusion = shapes.l[!shapes.l$name %in% c(unlist(d.union$shape), 
                                               shapes.out), ]  
  } else {
    exclusion = shapes.l[!shapes.l$name %in% unlist(d.union$shape), ]  
  } 
  if (is.null(shapes.in) == F) {
    selection = exclusion[exclusion$name %in% shapes.in, ]
    for (i in 1:nrow(selection)) {
      f = get(selection$name[i])
      if (i==1) {
        obj = f()
      } else {
        obj = cof(obj, f())
      }
    }
  } else {
    random<-sample(1:length(exclusion$name),n)
    for(i in 1:length(random)) {
      f<-get(exclusion$name[random[i]])
      if(i==1){
        obj<-f()
      }else{
        obj<-cof(obj,f())
      }
    }
  }
  
  d.union = cof(d.union, obj)
  return(d.union)
}



ic = function(m, 
              n.rule = 1) {
  m.correct = correct(m)
  
  ic.scale = size(m.correct, 3)
  ic.flip = rotation(correct(m), 3) # il numero sulla rotazione dipende dalla figura
  if (n.rule != 1) {
    ic.inc = hide(m.correct, 2)
  } else {
    ic.inc = m.correct
    ic.inc$attention = "I need more stuff to make IC-INC"
  }
  ic.dist = list(ic.scale = ic.scale, 
                 ic.flip = ic.flip, 
                 ic.inc = ic.inc)
  return(ic.dist)
}


# mette insieme i distrattori e comincia a fare un check rudimentale 

responses = function(m, n.rule = 1, 
                     shapes.out = NULL, 
                     shapes.in = NULL, 
                     n.shapes = 1) {
  m.correct = correct(m)
  resp = list(correct = m.correct, 
              r.top = m$Sq6,
              r.diag = m$Sq5,
              r.left = m$Sq8, 
              wp.copy = wp(m)$wp.copy, 
              wp.matrix = wp(m)$wp.matrix, 
              d.union = d.union(m, shapes.out = shapes.out, 
                                shapes.in = shapes.in, 
                                n = n.shapes), 
              ic.scale = ic(m, n.rule = n.rule)$ic.scale, 
              ic.flip = ic(m, n.rule = n.rule)$ic.flip, 
              ic.inc = ic(m, n.rule = n.rule)$ic.inc)
  
  if (any(unlist(resp$r.top) != unlist(m.correct), 
          na.rm = T) == F) {
    resp$r.top$attention = "R-top equal correct"
  } else if (any(unlist(resp$r.left) != unlist(m.correct), 
                 na.rm = T) == F) {
    resp$r.left$attention = "R-left equal correct"
  } else if (any(unlist(resp$r.diag) != unlist(m.correct), 
                 na.rm = T) == F) {
    resp$r.diag$attention = "R-diag equal correct"
  }
  
  
  if (any(unlist(resp$wp.copy) != unlist(m.correct), 
          na.rm = T) == F) {
    resp$wp.copy$attention = "WP-Copy equal correct"
  }
  
  return(resp)
} 



# funzione per disegnare i distrattori under cunstruction ---- 
# ma proprio non guardarla che non sta bene 
draw.distr = function(obj.list, 
                      n.dist = 4, 
                      n.rule = 1, 
                      main = FALSE) {
# come prima cosa toglie i distrattori con problemi 
  p = obj.list
  for (i in 1:length(p)) {
    if (any(names(p[[i]]) == "attention") == T) {
      p[[i]] = NULL
    } 
  }

  n.p = names(p)
  
  if (length(obj.list) == 5) {
    par(mfrow = c(1, 5))
  } else {
    par(mfrow=c(2, 4))
  }
  
  for (i in 1:length(obj.list)) {
    draw(obj.list[[i]])
  }
}
