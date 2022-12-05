
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("CodiceDistrattoriVero.R")



m_pratica1 <-apply(Raven(st1=cof(e.hexagon()),
                    hrule=c("fill"),vrule=c("identity")))

draw(m_pratica1, hide = TRUE)
m_pratica1_dist = responses(m_pratica1, choose.copy = 4)

draw.dist(m_pratica1_dist, n.resp = 10, main = T)
svg(paste0(getwd(), "/StudioPreliminare/", 
           "m_pratica1_dist.svg"), width=14, height=8.5)
draw.dist(m_pratica1_dist, n.resp = 10, main = T)
dev.off()
svg(paste0(getwd(), "/StudioPreliminare/", 
           "m_pratica1.svg"), width=14, height=8.5)

draw(m_pratica1, hide = TRUE)
dev.off()


m_pratica2 = apply(Raven(st1=pie.4(),
                  hrule=c("identity"),
                  vrule=c("size")))
draw(m_pratica2, hide = TRUE)

# non riesco a farlo funzionare solo con un elemento ci penso domani
# in realtà non funziona con gli oggetti union
m_pratica2_dist = responses(m_pratica2, 
                       choose.copy = 4)

draw.dist(m_pratica2_dist, n.resp = 10)
svg(paste0(getwd(), "/StudioPreliminare/", 
           "m_pratica2.svg"), width=14, height=8.5)

draw(m_pratica2, hide = TRUE)
dev.off()


m_pratica3 = apply(Raven(st1=pie.4(),
                     hrule=c("lty"),
                     vrule=c("identity")))
m_pratica3 = responses(m_pratica3)

draw.dist(m_pratica3)

svg(paste0(getwd(), "/StudioPreliminare/", 
           "m_pratica3.svg"), width=14, height=8.5)

draw(m_pratica3, hide = TRUE)
dev.off()


m_pratica4a = apply(Raven(st1 = cof(dot(), 
                            s.lily(), 
                            square(s.x = 5, s.y = 5, 
                                   shd = "black", rot = pi/2)), 
                 hrule = "diff_shapes"))
draw(m_pratica4a)           
m_pratica4b = apply(Raven(st1=pentagon(),
                 hrule=c("identity"),
                 vrule=c("identity")))
draw(m_pratica4b)
m_pratica4 = (com(m_pratica4a, m_pratica4b))

svg(paste0(getwd(), "/StudioPreliminare/", 
           "m_pratica4.svg"), width=14, height=8.5)

draw(m_pratica4, hide = TRUE)
dev.off()

draw(m_pratica4, hide = T)


# distrattori ---- 




