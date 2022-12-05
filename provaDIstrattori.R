# funzioni distrattori ---
rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")



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









ic = function(m, 
              n.rule = 1) {
  m.correct = correct(m)
  
  ic.scale = size(m.correct, 3)
  ic.flip = rotation(correct(m), 3) # il numero sulla rotazione dipende dalla figura
  if (n.rule != 1) {
    ic.inc = hide(m.correct, 3) # questo sarebbe da riscrivere sulla base di chi è effettivamente visibile in quella cella
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
                     choose.matrix = 1, choose.copy = 1, 
                     choose.start = 1) {
  m.correct = correct(m)
  resp = list(correct = m.correct, 
              r.top = m$Sq6,
              r.diag = m$Sq5,
              r.left = m$Sq8, 
              wp.copy = wp(m, choose.copy = choose.copy)$wp.copy, 
              wp.matrix = wp(m, choose.matrix = choose.matrix)$wp.matrix, 
              d.union = d.union(m, choose.start = choose.start), 
              ic.scale = ic(m, n.rule = n.rule)$ic.scale, 
              ic.flip = ic(m, n.rule = n.rule)$ic.flip, 
              ic.inc = ic(m, n.rule = n.rule)$ic.inc)
  
  # if (any(unlist(resp$r.top) != unlist(m.correct), 
  #         na.rm = T) == F) {
  #   resp$r.top$attention = "R-top equal correct"
  # } if (any(unlist(resp$r.left) != unlist(m.correct), 
  #                na.rm = T) == F) {
  #   resp$r.left$attention = "R-left equal correct"
  # } else if (any(unlist(resp$r.diag) != unlist(m.correct), 
  #                na.rm = T) == F) {
  #   resp$r.diag$attention = "R-diag equal correct"
  # }
  # 
  # 
  # if (any(unlist(resp$wp.copy) != unlist(m.correct), 
  #         na.rm = T) == F) {
  #   resp$wp.copy$attention = "WP-Copy equal correct"
  # }
  # 
  return(resp)
} 

draw.dist = function(dist.list, n.resp = 8,
                     main = NULL) {
  if (n.resp ==8) {
    par(mfrow = c(2, 4)) 
    
  } else if (n.resp == 10) {
    par(mfrow = c(2, 5))
  } else if (n.resp == 5) {
    par(mfrow =c(1, 5))
  }
 
  if (is.null(main) == F) {
    for (i in 1:length(dist.list)) {
      draw(dist.list[[i]], main = names(dist.list)[i])
    }
  } else {
    for (i in 1:length(dist.list)) {
      draw(dist.list[[i]])
    }
  }
}



# funzione per disegnare i distrattori under cunstruction ---- 
# ma proprio non guardarla che non sta bene 
# draw.distr = function(obj.list, 
#                       n.dist = 4, 
#                       n.rule = 1, 
#                       main = FALSE) {
# # come prima cosa toglie i distrattori con problemi 
#   p = obj.list
#   for (i in 1:length(p)) {
#     if (any(names(p[[i]]) == "attention") == T) {
#       p[[i]] = NULL
#     } 
#   }
# 
#   n.p = names(p)
#   
#   if (length(obj.list) == 5) {
#     par(mfrow = c(1, 5))
#   } else {
#     par(mfrow=c(2, 4))
#   }
#   
#   for (i in 1:length(obj.list)) {
#     draw(obj.list[[i]])
#   }
# }
