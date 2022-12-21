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

draw(young020, n.cell = 4)

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
    st1 =e.hexagon() )
)

young021<-com(young021a,young021b)
draw(young021, n.cell = 4)

dist_young021 = responses(young021,mat.type = 4)
draw.dist(dist_young021, n.resp = 11, main = T)



########################################################################
#                    022
########################################################################

young022a = obj_addition_rules(
  Raven(
    st1 = cof(cof(circle(s.x = 7,s.y = 7),dot(),name="oggetto",single=TRUE ),
              dice()) 
  ), rule="vh.add"
)

young022b = apply(
  Raven(
    st1 =luck() )
)

young022<-com(young022a,young022b)
draw(young022, n.cell = 4)

dist_young022 = responses(young022,mat.type = 4)
draw.dist(dist_young022, n.resp = 11, main = T)


