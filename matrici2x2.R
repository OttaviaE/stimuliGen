# matrici Ottavia -----
rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")

m1 = apply(
  Raven(
    st1 = square(), 
    vrule = "rotation", 
    hrule = "rotation"
  )
)
draw(m1, n.cell = 4)

m2 = apply(
  Raven(
    st1 = square(), 
    vrule = "rotation", 
    hrule = "identity"
  )
)
draw(m2, n.cell = 4)

draw(pentagon(rot = pi/5))

# forma e dimensione su entrambe le regole 2 x 2 ----
m3 = apply(
  Raven(
    st1 = cof(pacman(), 
              square(), 
              ellipse()), 
    vrule = c("diff_shapes", "size"), 
    hrule = "identity"
  )
)
draw(m3, n.cell = 4)

m4 = apply(
  Raven(
    st1 = cof(pacman(), 
              square(), 
              ellipse()), 
    vrule = c("diff_shapes"), 
    hrule = "size"
  )
)
draw(m4, n.cell = 4)

# forma e riempimento su entrambe le regole


m5 = apply(
  Raven(
    st1 = cof(e.hexagon(), 
              ellipse(), pacman()), 
    vrule = c("diff_shapes", "filline"), 
    hrule = "identity"
  )
)
draw(m5, n.cell = 4)


m6 = apply(
  Raven(
    st1 = cof(e.hexagon(), 
              square(), pacman()), 
    vrule = c("diff_shapes"), 
    hrule =  "filline"
  )
)
draw(m6, n.cell = 4)


# froma e orientamento -----
m7 = apply(
  Raven(
    st1 = cof(slice(size.x = 15), 
              square(), pacman()), 
    vrule = c("diff_shapes", "rotation"), 
    hrule =  "identity"
  )
)
draw(m7, n.cell = 4)

m8 = apply(
  Raven(
    st1 = cof(ellipse(), 
              pentagon(), pacman()), 
    vrule = c("diff_shapes"), 
    hrule =  "rotation"
  )
)
draw(m8, n.cell = 4)

# forma e bordo -----
m8 = apply(
  Raven(
    st1 = cof(circle(), 
              square(), pacman()), 
    vrule = c("diff_shapes", "lty"), 
    hrule =  "identity"
  )
)
draw(m8, n.cell = 4)

m9 = apply(
  Raven(
    st1 = cof(pacman(), 
              u.bow.tie(), pacman()), 
    vrule = c("diff_shapes"), 
    hrule =  "lty"
  )
)
draw(m9, n.cell = 4)

