# genereazione matrici per esperimento ----- 
rm(list=ls())
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("provaDIstrattori.R")
source("bloccoPratica.R")

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


# camio forma (TL-LR) rotatione (LL-TR) e dimensione (H o V) ----
# c'è un problema con la rotazione 

# a2 ------

a2 = apply(Raven(st1 = cof(luck(), pacman(), 
                           pentagon()), 
                 hrule = c("diff_shapes", "rotation", "size"), 
                 vrule = c("diff_shapes", "rotation")))

svg(paste0(getwd(), "/StudioPreliminare/GruppoA/", 
           "a_2.svg"), width=14, height=8.5)

draw(a2, hide = T)
dev.off()

# a1_2 ------

a1_2 = apply(Raven(st1 = cof(slice(), luck(), 
                             e.hexagon()), 
                   hrule = c("diff_shapes", "rotation", "size"), 
                   vrule = c("diff_shapes", "rotation")))
svg(paste0(getwd(), "/StudioPreliminare/GruppoA/", 
           "a1_2.svg"), width=14, height=8.5)

draw(a1_2, hide = T)
dev.off()

# gruppo B ---- 
# b_2 ------

b2 = apply(Raven(st1 = cof(semi.circle(), pacman(), 
                           ellipse()), 
                 hrule = c("diff_shapes", "rotation", "size"), 
                 vrule = c("diff_shapes", "rotation")))

svg(paste0(getwd(), "/StudioPreliminare/GruppoA/", 
           "b_2.svg"), width=14, height=8.5)

draw(b2, hide = T)
dev.off()

# b1_2-----

b1_2 = apply(Raven(st1 = cof(pentagon(), triangle(rot = pi/2), 
                             ellipse()), 
                   hrule = c("diff_shapes", "rotation", "size"), 
                   vrule = c("diff_shapes", "rotation")))
svg(paste0(getwd(), "/StudioPreliminare/GruppoA/", 
           "b1_2.svg"), width=14, height=8.5)

draw(b1_2, hide = T)
dev.off()

# riempmento, rotazione, forma ----- 
# a_3 ------
a_3a = apply(
  Raven(
    st1 = cof(square(s.x = 18, s.y = 18), 
              pentagon(s.x = 16, s.y = 16), 
              e.hexagon(s.x = 18, s.y = 18)), 
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


a_3c = apply(
  Raven(
    st1 = circle(s.x = 2, s.y = 2), 
    vrule = c("fill"), 
    hrule = "fill"
  )
)


svg(paste0(getwd(), "/StudioPreliminare/GruppoA/", 
           "a_3.svg"), width=14, height=8.5)
a_3 = com(a_3a, a_3b, a_3c)
draw(a_3, hide = T)
dev.off()
# a1_3 -----
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

svg(paste0(getwd(), "/StudioPreliminare/GruppoA/", 
           "a1_3.svg"), width=14, height=8.5)
a1_3 = com(a1_3a, a1_3b, a1_3c)
draw(a1_3, hide = T)
dev.off()

# gruppo B ---- 
# b_3 ------
b_3a = apply(Raven(
  st1 = cof(pentagon(), 
            e.hexagon(), 
            ellipse(s.x = 13, s.y = 17)), 
  vrule = "diff_shapes", 
  hrule= c("diff_shapes", "rotation")
))

draw(b_3a)

b_3b = apply(Raven(
  st1 = papillon,
  hrule = "multifill", 
  vrule = "multifill"
))

b_3 = com(b_3a, b_3b)

svg(paste0(getwd(), "/StudioPreliminare/GruppoB/", 
           "b_3.svg"), width=14, height=8.5)
draw(b_3, hide = T)
dev.off()

# b1_3 -----

b1_3a = apply(Raven(
  st1 = semi.circle(), 
  hrule =c("rotation", "fill"),
  vrule = c("fill")
))

draw(b1_3a)

b1_3b = apply(Raven(
  st1 = cof(circle(s.x = 15, s.y = 15), 
            square(s.x = 17, s.y = 17), 
            e.hexagon()), 
  vrule = "diff_shapes", 
  hrule= c("diff_shapes")
))


b1_3 = com(b1_3a, b1_3b)

svg(paste0(getwd(), "/StudioPreliminare/GruppoB/", 
           "b1_3.svg"), width=14, height=8.5)
draw(b1_3, hide = T)
dev.off()

# brutto esempio da buttare via ---- 

x1_2 = apply(Raven(st1 = cof(pentagon(), 
                             triangle(), 
                             ellipse()), 
                   hrule = c("diff_shapes", "rotation"), 
                   vrule = c("diff_shapes", "rotation")))

draw(x1_2)


x1_3 = apply(Raven(st1 = circle(), 
                   hrule = "size", 
                   vrule = "identity" ))
draw(x1_3)
draw(com(x1_2, x1_3))
