# generazione stimoli per riunione 2/2/2022 -----
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("provaDIstrattori.R")
draw.dist = function(dist.list, main = NULL) {
  par(mfrow = c(2, 4)) 

  if (is.null(main) == F) {
    for (i in 1:length(dist.list)) {
      draw(dist.list[[i]], main = names(dist.list)[i])
    } else {
      for (i in 1:length(dist.list)) {
        draw(dist.list[[i]])
      }
    }
  }
}

# matrici lofiche ----- 
M1<-logic_rules(Raven(square4()),"OR")
draw(M1)

M2<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"XOR")
draw(M2)

draw(com(M1,M2))
logic_1 = com(M1,M2)
draw(logic_1)

dist_logic1 = list(correct = correct(logic_1), 
                   r.top = repetition(logic_1)$r.top, 
                   r.left = repetition(logic_1)$r.left, 
                   d.union = d.union(logic_1, 
                                         choose.start = 3), 
                   wp.copy = wp(logic_1, choose.copy = 4)$wp.copy, 
                   wp.matrix = wp(logic_1, choose.matrix  = 4)$wp.matrix, 
                   ic.inc = hide(logic_1$Sq9, 1), 
                   ic.flip = cof(rotation(M2$Sq9, 2), M1$Sq9))

draw.dist(dist_logic1, main = T)

# altra logica -----
M1<-logic_rules(Raven(cof(pentagon(shd="line1.inv"), pentagon(shd="line2"),
                          pentagon(shd="line12.h"), hexagon(s.x=3,s.y=3)) ),"OR")
draw(M1)

M19 <- logic_rules(Raven(cof(vline(pos.x = 28, s.x = 15),
                             vline(pos.x = -28, s.x = 15 ),
                             hline(pos.y = 15, s.x=30),
                             hline(pos.y = -15, s.x=30))),"AND")
draw(com(M1,M19))

logic_2 = com(M1,M19)


dist_logic2 = list(correct = correct(logic_2), 
                   r.top = repetition(logic_2)$r.top, 
                   r.diag = repetition(logic_2)$r.diag, 
                   d.union = d.union(logic_2, 
                                     choose.start = 2), 
                   wp.copy = wp(logic_2, choose.copy = 1)$wp.copy, 
                   wp.matrix = wp(logic_2, choose.matrix  = 6)$wp.matrix, 
                   ic.inc = hide(logic_2$Sq9, 1), 
                   ic.neg = cof(pentagon(), 
                                hexagon(s.x=3,s.y=3)))
dist_logic2 = sample(dist_logic2)
draw.dist(dist_logic2, main = T)


# matrici non logiche -----

lilth<-lily()
s.lilth<-s.lily()
for(i in 1:length(lilth$shape)) {
  lilth$size.x[[i]] <-lilth$size.x[[i]]/2
  lilth$size.y[[i]] <-lilth$size.y[[i]]/2
  lilth$pos.y[[i]] <-lilth$pos.y[[i]]/2
  lilth$pos.x[[i]] <-lilth$pos.x[[i]]/2
  
}

s.lilth$size.x[[1]] <-s.lilth$size.x[[1]]/2
s.lilth$size.y[[1]] <-s.lilth$size.y[[1]]/2
s.lilth$pos.y[[1]] <-s.lilth$pos.y[[1]]/2
s.lilth$pos.x[[1]] <-s.lilth$pos.x[[1]]/2


papillon = bow.tie()
u.papillon = u.bow.tie()


for(i in 1:length(papillon$shape)) {
  papillon$size.x[[i]] <-papillon$size.x[[i]]/2
  papillon$size.y[[i]] <-papillon$size.y[[i]]/2
  papillon$pos.y[[i]] <-papillon$pos.y[[i]]/2
  papillon$pos.x[[i]] <-papillon$pos.x[[i]]/2
  
}

u.papillon$size.x[[1]] <-u.papillon$size.x[[1]]/2
u.papillon$size.y[[1]] <-u.papillon$size.y[[1]]/2
u.papillon$pos.y[[1]] <-u.papillon$pos.y[[1]]/2
u.papillon$pos.x[[1]] <-u.papillon$pos.x[[1]]/2

# a3----

a_3a = apply(
  Raven(
    st1 = cof(circle(s.x = 17, s.y = 17), 
              pentagon(s.x = 16, s.y = 16), 
              e.hexagon(s.x = 17, s.y = 17)), 
    vrule = c("diff_shapes"), 
    hrule = "diff_shapes"
  )
)
draw(a_3a)

a_3b = apply(
  Raven(
    st1 = pacman(), 
    vrule = c("rotation"), 
    hrule = "rotation"
  )
)
draw(a_3b)

a_3c = apply(
  Raven(
    st1 = circle(s.x = 2, s.y = 2), 
    vrule = c("fill"), 
    hrule = "fill"
  )
)
draw(a_3c)

a_3 = com(a_3a, a_3b, a_3c)
draw(a_3, hide = F)

dist_a3 = list(correct = correct(a_3), 
               r.top = repetition(a_3)$r.top, 
               r.diag = repetition(a_3)$r.diag, 
               d.union = d.union(a_3, 
                                 choose.start = 3), 
               wp.copy = wp(a_3, choose.copy = 4)$wp.copy, 
               wp.matrix = wp(a_3, choose.matrix  = 4)$wp.matrix, 
               ic.neg = cof(a_3a$Sq9, a_3b$Sq9, a_3c$Sq6), 
               ic.flip = cof(a_3a$Sq9, rotation(a_3b$Sq9, 3), 
                             a_3c$Sq9))
draw.dist(dist_a3, main = T)

# a1_3 ----
a1_3a = apply(Raven(
  st1 = square(s.x = 20, s.y = 20), 
  vrule = "identity", 
  hrule= "identity"
))

a1_3b = apply(Raven(
  st1 = cof(slice(), luck(s.x = 10, 
                          s.y = 12), triangle(s.x = 10, s.y = 10)), 
  vrule = "diff_shapes", 
  hrule= c("diff_shapes", "rotation")
))

a1_3c = apply(
  Raven(
    st1 = circle(s.x = 2, s.y = 2), 
    vrule = c("fill"), 
    hrule = "fill"
  )
)

a1_3 = com(a1_3a, a1_3b, a1_3c)
draw(a1_3, hide = T)

dist_a1_3 = list(correct = correct(a1_3), 
               r.top = repetition(a1_3)$r.top, 
               r.diag = repetition(a1_3)$r.diag, 
               d.union = d.union(a1_3, 
                                 choose.start = 1), 
               wp.copy = wp(a1_3)$wp.copy, 
               wp.matrix = wp(a1_3, choose.matrix  = 4)$wp.matrix, 
               ic.neg = cof(a1_3a$Sq9, a1_3b$Sq9, a1_3c$Sq5), 
               ic.flip = cof(a1_3a$Sq9, rotation(a1_3b$Sq9, 3), 
                             a1_3c$Sq9))
draw.dist(dist_a1_3, main = T)


# nuova matrice da elisa ---- 
e1 = apply(Raven(
  st1 = cof(circle(s.x = 12, s.y = 12, shd="black"), 
            u.star(), 
            square(shd="black", rot = pi/2)), 
  hrule="diff_shapes", 
  vrule = "diff_shapes"
    ))
draw(e1)

e2 = apply(Raven(
  st1 = cof(square(rot = pi/2, s.x =10, s.y =10, shd = "white"), 
            circle(s.x= 9, s.y = 9, shd = "white"), 
            triangle(shd = "white")), 
  hrule = c("diff_shapes", "size"), 
  vrule = c("diff_shapes", "size")
))

draw(e2)
e = (com(e1, e2))

draw(e)
dist_e = list(correct = correct(e), 
                 r.top = repetition(e)$r.top, 
                 r.diag = repetition(e)$r.diag, 
                 d.union = d.union(e, 
                                   choose.start = 1), 
                 wp.copy = wp(e)$wp.copy, 
                 wp.matrix = wp(e, choose.matrix  = 4)$wp.matrix, 
                 ic.neg = cof(square(shd="white",
                                     rot=pi/2), 
                              triangle(shd = "black", 
                                       s.x=2, s.y = 2)), 
                 ic.size = cof(e1$Sq9,  e2$Sq2))
draw.dist(dist_e, main = T)


