# ditstarttori matrici non logiche studio preliminare ----- 
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("provaDIstrattori.R")
source("bloccoPratica.R")
source("matriciNonLogiche-Preliminare.R")




# change margins --- 
change.marg = function(vec) {
  if (length(vec) == 5) {
    par(mfrow = c(1, 5), mar = c(0.5, 6, 0.5, 2) + .1, 
        mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )
  } else {
    par(mfrow = c(2, 4), mar = c(0.5, 6, 0.5, 2) + .1, 
        mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )
  }
}


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


# gruppo a (7 distrattori) ----- 
dist7 = c("correct", "r.top", "d.union", 
          "ic.scale", "ic.flip", "wp.matrix", 
          "wp.copy", "r.left")
dista = c("correct", "r.top", "d.union", 
          "ic.scale", "ic.inc", "wp.matrix", 
          "wp.copy", "r.left")
dist7a = c("correct", "r.top", "d.union", 
          "ic.scale", "ic.inc", "wp.matrix", 
          "wp.copy", "r.diag")
# a2 ----
draw(a2)
dist_a2 = responses(a2, 
                    shapes.out = c("ellipse"), 
                    shapes.in = c("lily"), 
                    all = c(2, 5, 7))
draw(d1)
svg(paste0(getwd(), "/StudioPreliminare/distrattori/gruppoA/", 
           "dist_a2.svg"), width=14, height=8.5)
change.marg(dist7)
draw.dist(dist_a2, sample(dist7))
dev.off()

# a1_2 ---- 
draw(a1_2)

dist_a1_2 = responses(a1_2, 
                      shapes.out = c("star", "u.star", 
                                     "hexagon"), 
                      shapes.in = c("bow.tie", "ellipse"), 
                      all = c(3, 4,6))
change.marg(dist7)
svg(paste0(getwd(), "/StudioPreliminare/distrattori/gruppoA/", 
           "dist_a1_2.svg"), width=14, height=8.5)
draw.dist(dist_a1_2, dist7)
dev.off()

# a3 ---- 

draw(a_3)

# non riesco più a salvarlo in svg 

dist_a3 = responses(a_3, n.rule = 3, 
                    shapes.out = c("star", "u.star", 
                                   "hexagon"), 
                    shapes.in = c("luck", "ellipse"), 
                    all = c(1, 2))
change.marg(dist7)
svg(paste0(getwd(), "/StudioPreliminare/distrattori/gruppoA/", 
           "dist_a3.svg"), width=14, height=8.5)
draw.dist(dist_a3, dist7a)
dev.off()

# a1_3 -----
draw(a1_3)

dist_a1_3 = responses(a1_3, 
                      n.rule = 3, 
                      shapes.out = c("star", "u.star", 
                                     "hexagon"), 
                      shapes.in = c("bow.tie", "pie.4"), 
                      all = c(3, 5, 7))
change.marg(dist7)
draw.dist(dist_a1_3, dist7a, main = T)

# gruppo B (5 dist) -----

# questi cosi al momento mi servono 
select.diag = c("correct", "r.diag", "wp.copy", "d.union", 
                "ic.scale")
select.top = c("correct", "r.top", "wp.matrix", "d.union", 
               "ic.flip")
select.left = c("correct", "r.top", "wp.matrix", "d.union", 
                "ic.flip")
select.inc = c("correct", "r.top", "wp.matrix", "d.union", 
               "ic.inc")

select.m = c("correct", "r.left", "wp.copy", "d.union", 
               "ic.flip")



# b2-----
draw(b2)

dist_b2 = responses(b2, 
                    n.rule = 1, 
                    shapes.out = c("star", "u.star", 
                                   "hexagon"), 
                    shapes.in = c("lily", "bow.tie"))
change.marg(select.diag)
draw.dist(dist_b2, select.diag, main = T)


# b1_2 ----- 
draw(b1_2)
dist_b1_2 = responses(b1_2, 
                    n.rule = 1, 
                    shapes.out = c("star", "u.star", 
                                   "hexagon"), 
                    shapes.in = c("luck", "pie.4"), 
                    all = c(2,4))
change.marg(select.diag)
draw.dist(dist_b2, select.m, main = T)


# b3 ---- 
draw(b_3)
dist_b3 = responses(b_3, 
                      n.rule = 3, 
                      shapes.out = c("star", "u.star", 
                                     "hexagon", "ellipse"), 
                      shapes.in = c("luck","dice"), 
                      all = c(1,5,7))
change.marg(select.diag)
draw.dist(dist_b3, select.inc, main = T)


# b1_3 ----- 

select.m1 = c("correct", "r.left", "wp.copy", "d.union", 
             "ic.flip")

draw(b1_3)
dist_b3 = responses(b1_3, 
                    n.rule = 3, 
                    shapes.out = c("star", "u.star", 
                                   "hexagon", "ellipse"), 
                    shapes.in = c("luck","dice"), 
                    all = c(1,5,7))
change.marg(select.diag)
draw.dist(dist_b3, select.m1, main = T)
# dovrebbe flippare solo una delle due figure ----