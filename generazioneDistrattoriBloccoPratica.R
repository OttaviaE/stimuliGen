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

dist_m1 = responses(m_pratica1)


svg(paste0(getwd(), "/StudioPreliminare/distrattori/", 
           "dist_pratica1.svg"), width=14, height=8.5)
change.marg(select.diag)
draw.dist(dist_m1, select.diag)

dev.off()



# in questo caso, rep top and rep diag sono uguali
# no qui non vnno quelle fatte in automatico perché il biscotto è nero 
# siccome questo è fastidioso, è il prescelto per avere solo la rispsta corretta 
# ma io non so fare i rettangoli dio pivero
# QUI VANNO I DISTRATTORI CON LE CELLE BIANCHE E BASTA------
draw(m_pratica2, hide = TRUE) 


# altra matrice
draw(m_pratica3, hide = T)

dist_m3 = responses(m_pratica3)


svg(paste0(getwd(), "/StudioPreliminare/distrattori/", 
           "dist_pratica3.svg"), width=14, height=8.5)
change.marg(select.diag)
draw.dist(dist_m3, select.diag)
dev.off()

# la quarta matrice dei distrattori --- 
draw(m_pratica4)
dist_m4 = responses(m_pratica4)


svg(paste0(getwd(), "/StudioPreliminare/distrattori/", 
           "dist_pratica4.svg"), width=14, height=8.5)

change.marg(select.diag)
draw.dist(dist_m4, select.diag)
dev.off()

