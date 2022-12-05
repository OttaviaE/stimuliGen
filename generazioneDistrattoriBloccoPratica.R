# prove distrattori ---- 

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("provaDIstrattori.R")
source("bloccoPratica.R")

p = ic(m_pratica1)

# comincia a disegnare daje ----

dist_m1 = responses(m_pratica1)

draw.dist(dist_m1, main = T, n.resp = 10)

# svg(paste0(getwd(), "/StudioPreliminare/distrattori/", 
#            "dist_pratica1.svg"), width=14, height=8.5)
# change.marg(select.diag)
# draw.dist(dist_m1, select.diag)
# 
# dev.off()



# in questo caso, rep top and rep diag sono uguali
# no qui non vnno quelle fatte in automatico perché il biscotto è nero 
# siccome questo è fastidioso, è il prescelto per avere solo la rispsta corretta 
# ma io non so fare i rettangoli dio pivero
# QUI VANNO I DISTRATTORI CON LE CELLE BIANCHE E BASTA------
draw(m_pratica2, hide = TRUE) 

dist_m2 = responses(m_pratica2, 
                    choose.start = 7, 
                    choose.matrix = 4)
draw.dist(dist_m2, n.resp = 10, 
          main = T)

# altra matrice
draw(m_pratica3, hide = T)


dist_m3 = responses(m_pratica3, 
                    choose.start = 4)
draw.dist(dist_m3, main = T, n.resp = 10)

# svg(paste0(getwd(), "/StudioPreliminare/distrattori/", 
#            "dist_pratica3.svg"), width=14, height=8.5)
# change.marg(select.diag)
# draw.dist(dist_m3, select.diag)
# dev.off()

# la quarta matrice dei distrattori --- 
draw(m_pratica4, hide = T)
dist_m4 = responses(m_pratica4)
draw.dist(dist_m4, n.resp = 10, main = T)
p = ic(m_pratica4)
# svg(paste0(getwd(), "/StudioPreliminare/distrattori/", 
#            "dist_pratica4.svg"), width=14, height=8.5)
# 
# change.marg(select.diag)
# draw.dist(dist_m4, select.diag)
# dev.off()

