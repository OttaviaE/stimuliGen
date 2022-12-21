rm(list = ls())
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")

u.biscuit = u.star()

u.biscuit$size.x[[1]] <-u.biscuit$size.x[[1]]/4
u.biscuit$size.y[[1]] <-u.biscuit$size.y[[1]]/4
u.biscuit$pos.y[[1]] <-u.biscuit$pos.y[[1]]/4
u.biscuit$pos.x[[1]] <-u.biscuit$pos.x[[1]]/4

blu = "deepskyblue3"

giallo = "gold"

rosso = "firebrick"

########################################################################
#                    005
########################################################################

draw(rectangle(s.x=-50,s.y=50,pos.x=+10,pos.y=-7),bg=blu)

for(j in seq(0, 3, by = .2)) {
  draw(diagline(pos.x = 30,pos.y = 10, s.x=100, rot=(pi/8)*j, lwd = 3,lty=2), 
       canvas = F)
}

for(j in c(seq(-20, -5, by = 3),seq(5, 15, by = 1.5))) {
  draw(vline(pos.x = j, s.x=100, lwd = 3,lty = 1), 
       canvas = F)
}

for(j in seq(-15, -5, by = 3)) {
  draw(hline(pos.y = j, s.x=100, lwd = 3,lty = 1), 
       canvas = F)
}
draw(rectangle(s.x=7,s.y=5,shd="white",pos.x=+15,pos.y=-10),
     canvas = FALSE)

##distrattori
par(mfrow =c(2, 3), mar = c(0.5, 6, 0.5, 2) + .1, 
    mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )

container = rectangle(s.x=7,s.y=5,shd=blu)

draw(container,xlim = 8)
clip(7,-7,5,-5)
for(j in seq(0, 3, by = .2)) {
  draw(diagline(pos.x =30- 15,pos.y = 10-(-10), s.x=100, rot=(pi/8)*j, lwd = 3,lty=2), 
       canvas = F)
}

for(j in c(seq(-20, -5, by = 3),seq(5, 15, by = 1.5))) {
  draw(vline(pos.x = j-15, s.x=100, lwd = 3,lty = 1), 
       canvas = F)
}

for(j in seq(-15, -5, by = 3)) {
  draw(hline(pos.y = j+10, s.x=100, lwd = 3,lty = 1), 
       canvas = F)
}


draw(container,xlim = 8)
clip(7,-7,5,-5)
for(j in seq(0, 3, by = .2)) {
  draw(diagline(pos.x =30- 15,pos.y = 10-(-10), s.x=100, rot=(pi/8)*j, lwd = 3,lty=2), 
       canvas = F)
}

for(j in c(seq(-20, -5, by = 3),seq(5, 15, by = 1.5))) {
  draw(vline(pos.x = j-15, s.x=100, lwd = 3,lty = 1), 
       canvas = F)
}

draw(container,xlim = 8)

draw(container,xlim = 8)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
      canvas = FALSE)

draw(container,xlim = 8)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12"),by=3.5,
      canvas = FALSE)

########################################################################
#                    013
########################################################################
young013 = apply(
  Raven(
    st1 = pentagon(rot=pi/2), 
    vrule = "reflection"
  )
)
draw(young013, n.cell = 4)


dist_young013 = responses(young013,mat.type = 4)
draw.dist(dist_young013, n.resp = 11, main = T)


########################################################################
#                    014
########################################################################

young014 = apply(
  Raven(
    st1 = pacman(), 
    vrule = "reflection",
    hrule = "reflection"
  )
)
draw(young014, n.cell = 4)


dist_young014 = responses(young014,mat.type = 4)
draw.dist(dist_young014, n.resp = 11, main = T)


########################################################################
#                    015
########################################################################

young015 = apply(
  Raven(
    st1 = triangle(), 
    vrule = "fill",
    hrule = "reflection"
  )
)
draw(young015, n.cell = 4)


dist_young015 = responses(young015,mat.type = 4)
draw.dist(dist_young015, n.resp = 11, main = T)


########################################################################
#                    016
########################################################################

young016 = apply(
  Raven(
    st1 = cof(square(),e.hexagon(),pacman()), 
    vrule = c("diff_shapes","fill")
  )
)
draw(young016, n.cell = 4)


dist_young016 = responses(young016,mat.type = 4)
draw.dist(dist_young016, n.resp = 11, main = T)


########################################################################
#                    017
########################################################################

young017a = apply(
  Raven(
    st1 = cof(luck(),circle(),pacman()), 
    vrule = c("diff_shapes")
  )
)

young017b = apply(
  Raven(
    st1 = cof(dot(),triangle(s.x=3,s.y = 3),pacman()), 
    hrule = c("diff_shapes")
  )
)

young017<- com(young017a,young017b)
draw(young017, n.cell = 4)


dist_young017 = responses(young017,mat.type = 4)
draw.dist(dist_young017, n.resp = 11, main = T)


########################################################################
#                    018
########################################################################

young018 = apply(
  Raven(
    st1 = cof(pentagon(),triangle() ,pacman()), 
    vrule = c("diff_shapes","reflection")
  )
)
draw(young018, n.cell = 4)


dist_young018 = responses(young018,mat.type = 4)
draw.dist(dist_young018, n.resp = 11, main = T)

########################################################################
#                    019
########################################################################

young019 = apply(
  Raven(
    st1 = cof(triangle(rot = pi) ,pacman(),square()), 
    vrule = c("diff_shapes"),
    hrule = c("reflection")
  )
)
draw(young019, n.cell = 4)


dist_young019 = responses(young019,mat.type = 4)
draw.dist(dist_young019, n.resp = 11, main = T)

########################################################################
#                    020
########################################################################

young020 = obj_addition_rules(
  Raven(
    st1 = cof(cross(),square()) 
  ), rule="v.add"
)


draw(young020, n.cell = 4, bg ="gold")

dist_young020 = responses(young020,mat.type = 4)
draw.dist(dist_young020, n.resp = 11, main = T)


########################################################################
#                    021
########################################################################

young021a = obj_addition_rules(
  Raven(
    st1 = cof(s.lily(),u.biscuit) 
  ), rule="vh.sott"
)

young021b = apply(
  Raven(
    st1 =e.hexagon(shd="firebrick") )
)

young021<-com(young021b,young021a)
draw(young021, n.cell = 4, bg="gold")

dist_young021 = responses(young021,mat.type = 4)
draw.dist(dist_young021, n.resp = 11, main = T)



########################################################################
#                    022
########################################################################


young022a = obj_addition_rules(
  Raven(
    st1 = cof(cross(),e.hexagon(s.x = 10,s.y=10))
  )
  , rule="vh.sott"
)

young022b = apply(
  Raven(
    st1 =square(shd = "gold") )
)

young022<-com(young022b,young022a)
draw(young022, n.cell = 4, bg="deepskyblue3")

dist_young022 = responses(young022,mat.type = 4)
draw.dist(dist_young022, n.resp = 11, main = T)




########################################################################
#                    023
########################################################################

young023 = obj_addition_rules(
  Raven(
    st1 = cof(cross(),square(rot = pi)) 
  ), rule="v.sott"
)

draw(young023, n.cell = 4)

dist_young023 = responses(young020,mat.type = 4)
draw.dist(dist_young020, n.resp = 11, main = T)


########################################################################
#                    024
########################################################################

young024a = obj_addition_rules(
  Raven(
    st1 = cof(circle(s.x=8,s.y = 8),dot()) 
  ), rule="vh.add"
)

young024b = apply(
  Raven(
    st1 = pentagon() )
)

young024<-com(young024a,young024b)
draw(young024, n.cell = 4)

dist_young024 = responses(young024,mat.type = 4)
draw.dist(dist_young024, n.resp = 11, main = T)



########################################################################
#                    025
########################################################################

young025a = obj_addition_rules(
  Raven(
    st1 = cof(cof(circle(s.x = 7,s.y = 7),dot(),name="oggetto",single=TRUE ),
              dice()) 
  ), rule="vh.add"
)

young025b = apply(
  Raven(
    st1 =luck() )
)

young025<-com(young025a,young025b)
draw(young025, n.cell = 4)

dist_young025 = responses(young022,mat.type = 4)
draw.dist(dist_young022, n.resp = 11, main = T)
