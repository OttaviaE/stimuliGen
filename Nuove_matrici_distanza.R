# a0_visuo2 ----

a_visuo2a = apply(Raven(
  st1 = cof(square(s.x =16, s.y = 16, shd = "grey", lty = 0), 
            pentagon(shd = "grey", lty = 0), 
            e.hexagon(shd = "grey", lty = 0)), 
  hrule = "diff_shapes", 
  vrule = "diff_shapes"
))

a_visuo2b = apply(Raven(
  st1 = pacman(size.x = 10, shd = "white"), 
  vrule = c("rotation.inv"), 
  hrule = c("rotation")
))


a_visuo2c = apply(Raven(
  st1 = size(circle(shd = "black"), 2), 
  vrule = "size"
))


a_visuo2 = com(a_visuo2a, a_visuo2b, a_visuo2c)

draw(a_visuo2)

vec_a_visuo2<-rbind(matiks2data.frame(a_visuo2a),
                    matiks2data.frame(a_visuo2b),
                    matiks2data.frame(a_visuo2c))

comp_a_visuo2<-c(matiks2competenze(a_visuo2a),
                 matiks2competenze(a_visuo2b),
                 matiks2competenze(a_visuo2c))

# a1_visuo2 ----
biscotto = cof(hexagon(shd = "grey", lty = 0), 
               rot.hexagon(shd = "grey", lty = 0), 
               single = T, 
               name = "biscotto")

a1_visuo2a = apply(Raven(
  st1 = cof(ellipse(s.x = 15, s.y = 12,shd = "grey", lty = 0), 
            biscotto, 
            square(s.x =16, s.y = 16, shd = "grey", lty = 0)), 
  hrule = "diff_shapes", 
  vrule = "diff_shapes"
))

a1_visuo2b = apply(Raven(
  st1 = slice(s.x = 11, shd = "white"), 
  vrule = c("rotation.inv"), 
  hrule = c("rotation")
))


a1_visuo2c = apply(Raven(
  st1 = size(circle(shd = "black"), 2), 
  vrule = "size"
))


a1_visuo2 = com(a1_visuo2a, a1_visuo2b, a1_visuo2c)

draw(a1_visuo2)

vec_a1_visuo2<-rbind(matiks2data.frame(a1_visuo2a),
                     matiks2data.frame(a1_visuo2b),
                     matiks2data.frame(a1_visuo2c))

comp_a1_visuo2<-c(matiks2competenze(a1_visuo2a),
                  matiks2competenze(a1_visuo2b),
                  matiks2competenze(a1_visuo2c))



## Accordo database ----

AccordoVisuoA2<-sum(vec_a1_visuo2==vec_a_visuo2)/(nrow(vec_a1_visuo2)*ncol(vec_a1_visuo2))


## Accordo competenze ----
serieA<-unique(c(comp_a_visuo2,comp_a1_visuo2))
skmapA<-matrix(0,ncol=length(serieA),nrow = 2)
for(ix in 1:length(serieA))
{
  skmapA[1,ix]<-any(comp_a1_visuo2==serieA[ix])
  skmapA[2,ix]<-any(comp_a_visuo2==serieA[ix])
}
HammingVisuoA2<-sum(abs(skmapA[1,]-skmapA[2,]))/length(serieA)


# a0_visuo1  ----
a_visuo1c <-apply(Raven(cof(circle(s.x = 3,s.y = 3),
                            square(s.x = 3,s.y = 3)),"trans.fill"))
a_visuo1a<-apply(Raven(cof(pentagon(),e.hexagon(),
                           circle(s.x = 15, s.y = 15)),
                       c("diff_shapes"),
                       c("diff_shapes.inv")))
coso = size(cof(slice(), rotation(slice(), 5), 
                name = "coso", single = T), 2)
a_visuo1b<-apply(Raven(coso, 
                       hrule = "rotation", 
                       vrule = "rotation.inv"))

a_visuo1 = com(a_visuo1a, a_visuo1b, a_visuo1c)

draw(a_visuo1, hide = F)

vec_a_visuo1<-rbind(matiks2data.frame(a_visuo1a),
                    matiks2data.frame(a_visuo1b),
                    matiks2data.frame(a_visuo1c))

comp_a_visuo1<-c(matiks2competenze(a_visuo1a),
                 matiks2competenze(a_visuo1b),
                 matiks2competenze(a_visuo1c))

# a1_visuo1  ----
a1_visuo1c<-apply(Raven(cof(ellipse(s.x = 5,s.y = 4),
                            luck(s.x = 5,s.y = 4)),
                        c("trans.fill")))

a1_visuo1a<-apply(Raven(cof(e.hexagon(s.x = 17, s.y = 17), 
                            
                            pentagon(s.x = 17, s.y = 17), 
                            square(s.x = 17, s.y = 17)),
                        c("diff_shapes"),
                        c("diff_shapes.inv")))

a1_visuo1b<-apply(Raven(u.pie.2.inv(size.x = 9), 
                        hrule = "rotation", 
                        vrule = "rotation.inv"))


a1_visuo1 = com(a1_visuo1a, a1_visuo1b, a1_visuo1c)
draw(a1_visuo1, hide = F)

vec_a1_visuo1<-rbind(matiks2data.frame(a1_visuo1a),
                     matiks2data.frame(a1_visuo1b),
                     matiks2data.frame(a1_visuo1c))

comp_a1_visuo1<-c(matiks2competenze(a1_visuo1a),
                  matiks2competenze(a1_visuo1b),
                  matiks2competenze(a1_visuo1c))

## Accordo database ----

AccordoVisuoA1<-sum(vec_a1_visuo1==vec_a_visuo1)/(nrow(vec_a1_visuo1)*ncol(vec_a1_visuo1))


## Accordo competenze ----
serieA<-unique(c(comp_a_visuo1,comp_a1_visuo1))
skmapA<-matrix(0,ncol=length(serieA),nrow = 2)
for(ix in 1:length(serieA))
{
  skmapA[1,ix]<-any(comp_a1_visuo1==serieA[ix])
  skmapA[2,ix]<-any(comp_a_visuo1==serieA[ix])
}
HammingVisuoA1<-sum(abs(skmapA[1,]-skmapA[2,]))/length(serieA)