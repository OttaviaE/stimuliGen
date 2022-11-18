source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods.R")
source("Class and Methods v02.R")
source("Class and Methods extension.R")
source("Rules_27102022.R")


# generazione di matrici per lo studio dei distrattori ---- 
# gruppo A 
a5_sh_rot_size = apply(Raven(
  st1 = cof(ellipse(), triangle(), square()), 
  vrule= c("diff_shapes", "rotation", "size"), 
  hrule = c("diff_shapes", "rotation")
))

draw(a5_sh_rot_size, hide = T)

b5_sh_rot_size = apply(Raven(
  st1 = cof(square(), triangle(), e.hexagon()), 
  vrule= c("diff_shapes", "rotation", "size"), 
  hrule = c("diff_shapes", "rotation")
))

draw(b5_sh_rot_size, hide = T)

# il biscotto non ruota, questa non va bene
a5a_sh_rot_size = apply(Raven(
  st1 = cof(ellipse(shd="black"), 
            u.star(), square(shd = "black")), 
  vrule= c("diff_shapes", "rotation", "size"), 
  hrule = c("diff_shapes", "rotation")
))
draw(a5a_sh_rot_size, hide = F)

b5a_sh_rot_size = apply(Raven(
  st1 = cof(pentagon(shd="black"), 
            u.star(), square(shd = "black")), 
  vrule= c("diff_shapes", "rotation"), 
  hrule = c("diff_shapes", "rotation", "size")
))
draw(b5a_sh_rot_size, hide = F)
