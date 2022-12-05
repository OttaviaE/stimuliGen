# genereazione matrici per esperimento ----- 
rm(list=ls())
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("provaDIstrattori.R")
source("bloccoPratica.R")

# ic scale solo per le mainpolazioni della dimensiuone
# negativo solo pre quando ci sono i rimepimenti 
# d union percentuale di forme che vengono usate 
# d matrix non prenderre tutte le entrate della matrice

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

a2_mat = apply(Raven(st1 = cof(luck(shd = "white"), pacman(shd = "white"), 
                           pentagon(shd = "white")), 
                 hrule = c("diff_shapes", "size", "rotation"), 
                 vrule = c("diff_shapes")))

a2_frame = apply(Raven(
  st1= circle(shd = "grey", s.y =17, s.x = 17), 
  "identity", "identity"
))

a2 = com(a2_frame, a2_mat)
draw(com(a2_frame, a2), hide = T)

a2_dist = responses(a2, which.element = "pentagon")
draw.dist(a2_dist, n.resp = 10, main = T)

svg(paste0(getwd(), "/StudioPreliminare/GruppoA/",
           "a_2.svg"), width=14, height=8.5)

draw(a2, hide = T)
dev.off()
dist_a2 = responses(a2)

pdf(paste0(getwd(), "/StudioPreliminare/GruppoA/",
           "a_2.pdf"), width=14, height=8.5)

draw(a2, hide = T)
dev.off()


draw.dist(dist_a2, n.resp = 10, main = T)


# a1_2 ------

a1_2mat = apply(Raven(st1 = cof(slice(shd = "white", 
                                   s.x = 13), 
                             luck(shd="white", s.x = 10, s.y = 13), 
                             e.hexagon(shd="white", 
                                       s.x = 13, s.y = 13)), 
                   hrule = c("diff_shapes", "size", "rotation"), 
                   vrule = c("diff_shapes")))
a1_2frame = apply(Raven(
  st1 = square(s.x = 21, s.y=21, shd = "grey"), 
  vrule = "identity"
))
a1_2 = com(a1_2frame, a1_2mat)

svg(paste0(getwd(), "/StudioPreliminare/GruppoA/",
           "a1_2.svg"), width=14, height=8.5)

draw(a1_2, hide = T)
dev.off()

a1_2_dist = responses(a1_2, which.element = "e.hexagon")

draw.dist(a1_2_dist, n.resp = 10, main = T)

pdf(paste0(getwd(), "/StudioPreliminare/GruppoA/",
           "a1_2.pdf"), width=14, height=8.5)

draw(a1_2, hide = T)
dev.off()


dist_a2 = responses(a1_2, choose.matrix = 2)

draw.dist(dist_a2, main = T, n.resp = 10)


# gruppo B ---- 
# b_2 ------

b2_mat = apply(Raven(st1 = cof(semi.circle(shd = "white"), 
                               pacman(shd = "white"), 
                           ellipse(shd = "white")), 
                 hrule = c("diff_shapes",  "size", "rotation"), 
                 vrule = c("diff_shapes")))
b2_frame = apply(Raven(
  st1 = pentagon(shd = "grey")
))
b2 = (com(b2_frame, b2_mat))

svg(paste0(getwd(), "/StudioPreliminare/GruppoB/", 
           "b_2.svg"), width=14, height=8.5)

draw(b2, hide = T)
dev.off()


pdf(paste0(getwd(), "/StudioPreliminare/GruppoB/", 
           "b_2.pdf"), width=14, height=8.5)

draw(b2, hide = T)
dev.off()


b2_dist = responses(b2)

draw.dist(b2_dist, n.resp = 10, main = T)

# b1_2-----

b1_2mat = apply(Raven(st1 = cof(pentagon(shd = "white", 
                                        s.x = 12, s.y = 12), 
                               triangle(rot = pi/2, 
                                        s.x = 12, s.y = 12, shd = "white"), 
                             ellipse(shd = "white", 
                                     
                                     s.x = 9, s.y = 12)), 
                   hrule = c("diff_shapes",  "size", "rotation"), 
                   vrule = c("diff_shapes")))
b1_2frame = apply(Raven(
  st1 = square(shd = "grey", s.x = 20, s.y = 20)
))
b1_2 = com(b1_2frame, b1_2mat)

svg(paste0(getwd(), "/StudioPreliminare/GruppoB/", 
           "b1_2.svg"), width=14, height=8.5)

draw(b1_2, hide = T)
dev.off()


pdf(paste0(getwd(), "/StudioPreliminare/GruppoB/", 
           "b1_2.pdf"), width=14, height=8.5)

draw(b1_2, hide = T)
dev.off()

b1_2_dist = responses(b1_2, which.element = "ellipse")

draw.dist(b1_2_dist, n.resp = 10, main = T)

# riempmento, rotazione, forma ----- 
# a_3 ------
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
draw(a_3)
svg(paste0(getwd(), "/StudioPreliminare/GruppoA/", 
           "a_3.svg"), width=14, height=8.5)
draw(a_3, hide = T)
dev.off()

pdf(paste0(getwd(), "/StudioPreliminare/GruppoA/", 
           "a_3.pdf"), width=14, height=8.5)
draw(a_3, hide = T)
dev.off()

a3_dist = responses(a_3, which.element = "pacman")

draw.dist(a3_dist, n.resp = 10, main = T)

# a1_3 -----
a1_3a = apply(Raven(
  st1 = square(s.x = 20, s.y = 20), 
  vrule = "identity", 
  hrule= "identity"
))

a1_3b = apply(Raven(
  st1 = cof(slice(s.x = sqrt(square()$size.x[[1]]^2 /2)), luck(s.x = 10, 
                          s.y = 12), 
            triangle(s.x = 10, s.y = 10)), 
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
svg(paste0(getwd(), "/StudioPreliminare/GruppoA/", 
           "a1_3.svg"), width=14, height=8.5)

draw(a1_3, hide = T)
dev.off()

# non funziona 
a1_3_dist = responses(a1_3, which.element = "triangle")
draw.dist(a1_3_dist, n.resp = 10, main = T)

pdf(paste0(getwd(), "/StudioPreliminare/GruppoA/", 
           "a1_3.pdf"), width=14, height=8.5)

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

pdf(paste0(getwd(), "/StudioPreliminare/GruppoB/", 
           "b_3.pdf"), width=14, height=8.5)
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

pdf(paste0(getwd(), "/StudioPreliminare/GruppoB/", 
           "b1_3.pdf"), width=14, height=8.5)
draw(b1_3, hide = T)
dev.off()

b1_3_dist = responses(b1_3)
draw.dist(b1_3_dist, n.resp = 10, main = T)

# matrici logiche ----- 
bow.tie.inv <- function(pos.x = 0) {
  value <-cof(triangle(pos.x = pos.x+10, pos.y = pos.x, rot=pi/3, 
                       s.x = 10, s.y=10), 
              triangle(pos.x = pos.x-10, pos.y = pos.x, rot=-pi, 
                       s.x = 10, s.y=10))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "field"
  value
}

## a_logic3 
M1a<-logic_rules(Raven(square4()),"OR")

M1b<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"XOR")

logic_m1 = com(M1a, M1b)
draw(logic_m1)

logic_m1_dist = responses(logic_m1)
draw.dist(logic_m1_dist, n.resp = 10, main = T)

# seconda matrice logica ----- 
M2a<-logic_rules(Raven(cof(hline(pos.y = 3,s.y=12),hline(pos.y = -3,s.y=12),
                           square(shd="line12"),pentagon(s.x=3,s.y=3,shd="white"))
),"XOR")
M2b <- logic_rules(Raven(cof(vline(pos.x = 28, s.x = 15),
                             vline(pos.x = -28, s.x = 15 ),
                             hline(pos.y = 15, s.x=30),
                             hline(pos.y = -15, s.x=30))),"OR")
logic_m2 = com(M2a, M2b)

logic_m2_dist = responses(logic_m2, which.element = "square")
draw.dist(logic_m2_dist, n.resp = 10, main = T)


