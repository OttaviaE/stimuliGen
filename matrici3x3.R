# matrici 3 x 3 ------
rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")

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


thepie = pie.4()
u.thepie = u.pie.4()


for(i in 1:length(thepie$shape)) {
  thepie$size.x[[i]] <-thepie$size.x[[i]]/2
  thepie$size.y[[i]] <-thepie$size.y[[i]]/2
  thepie$pos.y[[i]] <-thepie$pos.y[[i]]/2
  thepie$pos.x[[i]] <-thepie$pos.x[[i]]/2
  
}

u.thepie$size.x[[1]] <-u.thepie$size.x[[1]]/2
u.thepie$size.y[[1]] <-u.thepie$size.y[[1]]/2
u.thepie$pos.y[[1]] <-u.thepie$pos.y[[1]]/2
u.thepie$pos.x[[1]] <-u.thepie$pos.x[[1]]/2

biscuit = star()
u.biscuit = u.star()


for(i in 1:length(biscuit$shape)) {
  biscuit$size.x[[i]] <-biscuit$size.x[[i]]/2
  biscuit$size.y[[i]] <-biscuit$size.y[[i]]/2
  biscuit$pos.y[[i]] <-biscuit$pos.y[[i]]/2
  biscuit$pos.x[[i]] <-biscuit$pos.x[[i]]/2
  
}

u.biscuit$size.x[[1]] <-u.biscuit$size.x[[1]]/2
u.biscuit$size.y[[1]] <-u.biscuit$size.y[[1]]/2
u.biscuit$pos.y[[1]] <-u.biscuit$pos.y[[1]]/2
u.biscuit$pos.x[[1]] <-u.biscuit$pos.x[[1]]/2

# forma e dimensione ----
M1 = apply(
  Raven(
    st1 = cof(square(shd = "black"), u.star(), triangle(rot = pi/2, shd = "black")), 
    vrule = c("diff_shapes", "size"), 
    hrule = "identity"
  )
)

draw(M1, n.cell = 9)

M2 = apply(
  Raven(
    st1 = cof(square(shd = "black"), u.star(), triangle(rot = pi/2, shd = "black")), 
    vrule = c("diff_shapes"), 
    hrule = "size"
  )
)
draw(M2, n.cell = 9)

# forma e riempimento ---- 

M3a = apply(
  Raven(
    st1 = cof(square(), 
              circle(s.x = 15, s.y = 15), pentagon()), 
    vrule = c("diff_shapes"), 
    hrule = "identity"
  )
)
draw(M3a, n.cell = 9)


M3b = apply(
  Raven(
    st1 = cof(s.lilth, 
              luck(s.x = 5, s.y = 7), 
              u.papillon), 
    vrule = c("diff_shapes"), 
    hrule = "identity"
  )
)
draw(com(M3a, M3b))





M4a = apply(
  Raven(
    st1 = cof(square(), 
              circle(), e.hexagon()), 
    vrule = c("diff_shapes"), 
    hrule = "identity"
  )
)
draw(M4a, n.cell = 9)

M4b = apply(
  Raven(
    st1 = cof(s.lilth, 
              luck(s.x = 5, s.y = 7), 
              u.papillon), 
    vrule = c("identity"), 
    hrule = "diff_shapes"
  )
)
draw(M4b, n.cell = 9)

draw(com(M4a, M4b), n.cell=9)

# tl-lr per la prima, v per la seconda 
M5a = apply(
  Raven(
    st1 = cof(square(), 
              ellipse(), e.hexagon()), 
    vrule = c("diff_shapes"), 
    hrule = "diff_shapes"
  )
)
draw(M5a, n.cell = 9)

M5b = apply(
  Raven(
    st1 = cof(s.lilth, 
              luck(s.x = 5, s.y = 7), 
              u.thepie), 
    vrule = c("diff_shapes"), 
    hrule = "identity"
  )
)
draw(M5b, n.cell = 9)

draw(com(M5a, M5b), n.cell=9)


# tl-lr per la prima, tp-ll per la seconda 
M6a = apply(
  Raven(
    st1 = cof(square(), 
              circle(s.x = 15, s.y = 15), pentagon()), 
    vrule = c("diff_shapes"), 
    hrule = "diff_shapes"
  )
)
draw(M6a, n.cell = 9)

M6b = apply(
  Raven(
    st1 = cof(u.biscuit, 
              luck(s.x = 5, s.y = 7, shd = "black"), 
              triangle(s.x = 8, s.y=8, rot = pi/2, shd = "black", pos.y = -1)), 
    vrule = c("diff_shapes.inv"), 
    hrule = "diff_shapes.inv"
  )
)
draw(M6b, n.cell = 9)

draw(com(M6a, M6b), n.cell=9)

# Forma e orientamento ----- 
# ve su entrambe 

M7 = apply(
  Raven(
    st1 = cof(luck(), 
              ellipse(rot=pi/2), pentagon()), 
    vrule = c("diff_shapes", "rotation"), 
    hrule = "identity"
  )
)
draw(M7, n.cell = 9)


M8 = apply(
  Raven(
    st1 = cof(luck(), 
              ellipse(rot=pi/2), pentagon()), 
    vrule = c("diff_shapes"), 
    hrule = "rotation"
  )
)
draw(M8, n.cell = 9)


M9 = apply(
  Raven(
    st1 = cof(luck(), 
              pacman(), pentagon()), 
    vrule = c("diff_shapes"), 
    hrule = "rotation"
  )
)
draw(M9, n.cell = 9)


M10 = apply(
  Raven(
    st1 = cof(luck(), 
              pacman(), pentagon()), 
    vrule = c("diff_shapes", "rotation"), 
    hrule = "diff_shapes"
  )
)
draw(M10, n.cell = 9)

M11 = apply(
  Raven(
    st1 = cof(luck(), 
              pacman(), e.hexagon()), 
    vrule = c("diff_shapes.inv", "rotation"), 
    hrule = c("diff_shapes.inv", "rotation")
  )
)
draw(M11, n.cell = 9)

# forma e bordo ----

M11 = apply(
  Raven(
    st1 = cof(luck(), 
              pacman(), e.hexagon()), 
    vrule = c("diff_shapes", "lty", "lwd"), 
    hrule = "identity"
  )
)
draw(M11, n.cell = 9)


M12 = apply(
  Raven(
    st1 = cof(s.lily(), 
              u.bow.tie(), triangle()), 
    vrule = c("diff_shapes"), 
    hrule = c("lty", "lwd")
  )
)
draw(M12, n.cell = 9)

M13 = apply(
  Raven(
    st1 = cof(s.lily(), 
              u.bow.tie(), pacman()), 
    vrule = c("diff_shapes"), 
    hrule = c("diff_shapes", "lty", "lwd")
  )
)
draw(M13, n.cell = 9)

M14 = apply(
  Raven(
    st1 = cof(s.lily(), 
              u.bow.tie(), pacman()), 
    vrule = c("diff_shapes", "lty", "lwd"), 
    hrule = c("diff_shapes", "lty", "lwd")
  )
)
draw(M14, n.cell = 9)

# rimepimento e orientamento ---- 

M15a = apply(
  Raven(
    st1 = pentagon(), 
    vrule = c("rotation"), 
    hrule = c("identity")
  )
)
draw(M15a, n.cell = 9)


M15b = apply(
  Raven(
    st1 = cof(u.biscuit, 
              luck(s.x = 5, s.y = 7, shd = "black"), 
              triangle(s.x = 8, s.y=8, rot = pi/2, shd = "black", pos.y = -1)), 
    vrule = c("diff_shapes.inv"), 
    hrule = "identity"
  )
)
draw(M15b, n.cell = 9)

draw(com(M15a, M15b), n.cell=9)

M16a = apply(
  Raven(
    st1 = e.hexagon(), 
    vrule = c("rotation"), 
    hrule = c("identity")
  )
)
draw(M16a, n.cell = 9)


M16b = apply(
  Raven(
    st1 = cof(s.lilth, 
              u.thepie, 
              u.papillon), 
    vrule = c("identity"), 
    hrule = "diff_shapes.inv"
  )
)
draw(M16b, n.cell = 9)

draw(com(M16a, M16b), n.cell=9)

M17a = apply(
  Raven(
    st1 = pacman(), 
    vrule = c("rotation"), 
    hrule = c("rotation")
  )
)
draw(M17a, n.cell = 9)


M17b = apply(
  Raven(
    st1 = circle(s.x = 2, s.y = 2), 
    vrule = c("fill"), 
    hrule = "fill"
  )
)
draw(M17b, n.cell = 9)

draw(com(M17a, M17b), n.cell=9)

# Riempimento e bordo -----

M18a = apply(
  Raven(
    st1 = square(), 
    vrule = c("identity"), 
    hrule = c("identity")
  )
)
draw(M18a, n.cell = 9)


M18b = apply(
  Raven(
    st1 = circle(s.x = 3, s.y = 3), 
    vrule = c("fill"), 
    hrule = "identity"
  )
)
draw(M18b, n.cell = 9)

M18c = apply(
  Raven(
    st1 = square(s.x = 13, s.y = 13), 
    vrule = c("lty", "lwd"), 
    hrule = c("identity")
  )
)
draw(M18c, n.cell = 9)


draw(com(M18a, M18b, M18c), n.cell=9)
draw(com(M18b, M18c))


M19a = apply(
  Raven(
    st1 = circle(s.x = 3, s.y = 3), 
    vrule = c("fill"), 
    hrule = "identity"
  )
)
draw(M19a, n.cell = 9)

M19b = apply(
  Raven(
    st1 = triangle(s.x = 13, s.y = 13), 
    vrule = c("identity"), 
    hrule = c("lty", "lwd")
  )
)
draw(M19b, n.cell = 9)
draw(com(M19a, M19b))


M20a = apply(
  Raven(
    st1 = pacman(), 
    vrule = c("lty", "lwd"), 
    hrule = c("lty", "lwd")
  )
)
draw(M20a, n.cell = 9)

M20b = apply(
  Raven(
    st1 = square(s.x = 3, s.y = 3), 
    vrule = c("fill"), 
    hrule = c("identity")
  )
)
draw(M20b, n.cell = 9)
draw(com(M20a, M20b))

M21a = apply(
  Raven(
    st1 = e.hexagon(), 
    vrule = c("lty", "lwd"), 
    hrule = c("lty", "lwd")
  )
)
draw(M21a, n.cell = 9)

M21b = apply(
  Raven(
    st1 = cof(u.biscuit, 
              luck(s.x = 5, s.y = 7, shd = "black"), 
              triangle(s.x = 8, s.y=8, rot = pi/2, shd = "black", pos.y = -1)), 
    vrule = c("diff_shapes"), 
    hrule = c("diff_shapes")
  )
)
draw(com(M21a, M21b))

# Forma riempimento bordo 

M22a = apply(
  Raven(
    st1 = cof(square(), 
              pentagon(), 
              e.hexagon()), 
    vrule = c("diff_shapes", "lty", "lwd"), 
    hrule = c("identity")
  )
)
draw(M22a, n.cell = 9)

M22b = apply(
  Raven(
    st1 = cof(s.lilth, 
              luck(s.x = 5, s.y = 7), 
              u.thepie), 
    vrule = c("diff_shapes"), 
    hrule = c("identity")
  )
)
draw(com(M22a, M22b))

M23a = apply(
  Raven(
    st1 = cof(square(), 
              pentagon(), 
              e.hexagon()), 
    vrule = c("diff_shapes", "lty", "lwd"), 
    hrule = c("identity")
  )
)
draw(M23a, n.cell = 9)

M23b = apply(
  Raven(
    st1 = cof(s.lilth, 
              u.papillon, 
              u.thepie), 
    vrule = c("identity"), 
    hrule = c("diff_shapes")
  )
)
draw(com(M23a, M23b))

M24a = apply(
  Raven(
    st1 = cof(triangle(), 
              pacman(), 
              u.bow.tie()), 
    vrule = c("diff_shapes", "lty", "lwd"), 
    hrule = c("diff_shapes")
  )
)
draw(M24a, n.cell = 9)

M24b = apply(
  Raven(
    st1 = circle(s.x = 3, s.y = 3), 
    vrule = c("identity"), 
    hrule = "fill"
  )
)
draw(com(M24a, M24b))

M25a = apply(
  Raven(
    st1 = cof(triangle(), 
              e.hexagon(), 
              luck()), 
    vrule = c("diff_shapes.inv", "lty", "lwd"), 
    hrule = c("diff_shapes.inv", "lty", "lwd")
  )
)
draw(M25a, n.cell = 9)

u.biscuit1 = u.biscuit
u.biscuit1$size.x[[1]] = u.biscuit1$size.x[[1]]-2
u.biscuit1$size.y[[1]] = u.biscuit1$size.y[[1]]-2
M25b = apply(
  Raven(
    st1 =  cof(u.biscuit1, 
    luck(s.x = 4, s.y = 6, shd = "black"), 
    circle(s.x = 3, s.y = 3, shd="black")), 
    vrule = c("diff_shapes"), 
    hrule = "diff_shapes"
  )
)

draw(com(M25b, M25a))

# forma rimepimento e dimensione ----

M26 = apply(
  Raven(
    st1 = cof(square(), 
              pentagon(), 
              circle()), 
    vrule = c("diff_shapes", "size", "parfill"), 
    hrule = c("identity")
  )
)
draw(M26, n.cell = 9)


M27 = apply(
  Raven(
    st1 = cof(luck(), 
              triangle(), 
              ellipse()), 
    vrule = c("diff_shapes", "size"), 
    hrule = c("parfill")
  )
)
draw(M27, n.cell = 9)

M28a = apply(
  Raven(
    st1 = cof(square(), 
              pentagon(), 
              luck(s.x=17, s.y = 14)), 
    vrule = c("diff_shapes"), 
    hrule = c("diff_shapes")
  )
)
draw(M28a)


M28b = apply(
  Raven(
    st1 = cof(circle(s.x=8, s.y=8)), 
    vrule = c("fill", "size"), 
    hrule = c("identity")
  )
)
draw(com(M28a, M28b))

M29a = apply(
  Raven(
    st1 = cof(square(), 
              pentagon(), 
              e.hexagon(s.x = 15, s.y=15)), 
    vrule = c("diff_shapes.inv"), 
    hrule = c("diff_shapes.inv")
  )
)
draw(M29a)




M29b = apply(
  Raven(
    st1 = cof(u.biscuit, 
              circle(s.x=8, s.y=8, shd = "black"), 
              luck(shd="black")), 
    vrule = c("diff_shapes", "size"), 
    hrule = c("diff_shapes", "size")
  )
)
draw(com(M29a, M29b))

M29b = apply(
  Raven(
    st1 = circle(s.x=8, s.y=8), 
    vrule = c("fill", "size"), 
    hrule = c("fill", "size")
  )
)
draw(com(M29a, M29b))



