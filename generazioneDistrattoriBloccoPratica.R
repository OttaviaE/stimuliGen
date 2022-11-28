# prove distrattori ---- 

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("provaDIstrattori.R")
source("bloccoPratica.R")


# change margins --- 
change.marg = function(vec) {
  if (length(vec) == 5) {
    par(mfrow = c(1, 5))
  } else {
    par(mfrow = c(2, 4))
  }
}

# questi cosi al momento mi servono 
select.diag = c("correct", "r.diag", "wp.copy", "d.union", 
                "ic.scale")
select.top = c("correct", "r.top", "wp.matrix", "d.union", 
               "ic.flip")
select.left = c("correct", "r.top", "wp.matrix", "d.union", 
                "ic.flip")
select.inc = c("correct", "r.top", "wp.matrix", "d.union", 
               "ic.inc")
# disegna i distrattori ---- 

draw.dist = function(resp.list, 
                     vec.sel, 
                     main = F) {
  vec.sel = sample(vec.sel) 
  
  if ( main == T) {
    for (i in 1:length(vec.sel)) {
      
      draw(resp.list[[vec.sel[i]]], 
           main = vec.sel[[i]])
    }
  } else {
    for (i in 1:length(vec.sel)) {
      
      draw(resp.list[[vec.sel[i]]])
    }
  }
}
# comincia a disegnare daje ----

dist_m1 = responses(rot_h)

change.marg(select.diag)
draw.dist(dist_m1, select.diag)

# in questo caso, rep top and rep diag sono uguali
# no qui non vnno quelle fatte in automatico perché il biscotto è nero 
# siccome questo è fastidioso, è il prescelto per avere solo la rispsta corretta 
# ma io non so fare i rettangoli dio pivero
draw(s_v)


# altra matrice
draw(lwd_hv, hide = T)

dist_m3 = responses(lwd_hv)

change.marg(select.diag)
draw.dist(dist_m3, select.diag)


# la quarta matrice dei distrattori --- 
draw(mix)
dist_m4 = responses(mix)

change.marg(select.diag)
draw.dist(dist_m4, select.diag)
