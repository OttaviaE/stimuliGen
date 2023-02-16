## ----r------------------------------------------------------------------------
library(ggplot2)
library(psych)
library(TAM)
library(knitr)
library(patchwork)
library(lavaan)

set.seed(999)
knitr::opts_chunk$set(echo=FALSE,  
                      eval=TRUE,  
                      warning = FALSE, 
                      message = FALSE, 
                      fig.align = "center")
knitr::knit_hooks$set(purl = knitr::hook_purl)

rm(list = ls())
select.dist = function(dist.list, selection) {
resp = list()
for (i in 1:length(selection)) {
resp[[i]] = dist.list[[selection[i]]]
names(resp)[[i]] = selection[i]
}
return(resp)
}

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")

draws<- function(obj, main = NULL, canvas = TRUE, bg = "white",mar=c(1,1,1,1),xlim=16,by=1.5) {
library(DescTools)
if (canvas == TRUE)
{
Canvas(xlim=xlim,mar=mar, main = main, bg = bg)
}
for(j in 1:length(obj$shape))
{
if(obj$visible[[j]]==1)
{
if(obj$num[[j]][1]==1){
if(grepl("line",obj$shade[[j]][1]))
{
elements<-decof(obj)
plotting_lines<-which(obj$visible==1 & grepl("line",unlist(obj$shade)))
for(ll in 1:length(plotting_lines)){
line(elements[[plotting_lines[[ll]]]],obj$shade[[j]][1],lwd=3,by=by) #Pejo tacon che sbrego

}
obj$shade[[j]][1] <- NA
}
DrawRegPolygon(x = obj$pos.x[[j]], y = obj$pos.y[[j]], rot = obj$rotation[[j]],
 radius.x = obj$size.x[[j]], radius.y = obj$size.y[[j]], nv = obj$nv[[j]],
 lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]])


}else{

DrawCircle(x = obj$pos.x[[j]], y = obj$pos.y[[j]],
 r.out = obj$size.x[[j]],r.in= obj$size.y[[j]], theta.1=obj$theta.1[[j]],
 theta.2=obj$theta.2[[j]], nv = obj$nv[[j]],
 lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]])

}
}
}
}

empty <- function() {
value <- list(
shape = "empty",
size.x = list(5),
size.y = list(5),
theta.1= list(0),
theta.2= list(0),
rotation = list(pi),
pos.x = list(0),
pos.y = list(0),
lty = list(0),
lwd = list(1),
num = list(1),
nv = list(101),
shade = list(NA),
visible = 0,
tag=list(c('simple'))
)
attr(value, "class") <- "field"
value
}

bow.tie.inv <- function(pos.x = 0) {
value <-cof(triangle(pos.x = pos.x+10, pos.y = pos.x, rot=pi/3, 
 s.x = 10, s.y=10), 
triangle(pos.x = pos.x-10, pos.y = pos.x, rot=-pi, 
 s.x = 10, s.y=10))
value$tag <- list("compose2","fill", "rotate")
attr(value, "class") <- "field"
value
}

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


square4bis <- function(size.x = 15, size.y=15) {
value <-cof(hline(s.x = size.x, 
                  s.y = size.y, 
                  pos.y=-15),
            vline(s.x = size.x, 
                  s.y = size.y, 
                  pos.x=15),
hline(pos.y=15, s.x = size.x, 
                  s.y = size.y),vline(pos.x=-15, s.x = size.x, 
                  s.y = size.y))
value$tag <- list("compose4")
attr(value, "class") <- "field"
value
}


smallbow.tie.inv <- function(pos.x = 0,pos.y=0,shd=NA) {
value <-cof(triangle(pos.x = pos.x+5, pos.y = pos.y, rot=pi/3, 
 s.x = 5, s.y=5,shd = shd), 
triangle(pos.x = pos.x-5, pos.y = pos.y, rot=-pi, 
 s.x = 5, s.y=5,shd = shd))
value$tag <- list("compose2","fill", "rotate")
attr(value, "class") <- "field"
value
}


u.smallbow.tie.inv <- function(pos.x = 0,pos.y=0,shd=NA) {
value <-cof(triangle(pos.x = pos.x+5, pos.y = pos.y, rot=pi/3, 
 s.x = 5, s.y=5,shd = shd), 
triangle(pos.x = pos.x-5, pos.y = pos.y, rot=-pi, 
 s.x = 5, s.y=5,shd = shd), 
single = T, name = "u.smallbowtie.inv")
value$tag <- list("compose2","fill", "rotate")
attr(value, "class") <- "field"
value
}

bow.tie.inv <- function(pos.x = 0) {
value <-cof(triangle(pos.x = pos.x+10, pos.y = pos.x, rot=pi/3, 
 s.x = 10, s.y=10), 
triangle(pos.x = pos.x-10, pos.y = pos.x, rot=-pi, 
 s.x = 10, s.y=10))
value$tag <- list("compose2","fill", "rotate")
attr(value, "class") <- "field"
value
}



blu = "deepskyblue3"

giallo = "gold"

rosso = "firebrick"



## ----r------------------------------------------------------------------------
# vincolo non tangente non deve toccare interno con esterno
a_logic1a<- logic_rules(Raven(square4bis()),"OR")

a_logic1b<-logic_rules(Raven(cof(size(bow.tie(), 2),
                                 size(bow.tie.inv(), 2))),"AND")

a_logic1 = com(a_logic1a, a_logic1b)

draw(a_logic1, hide = F) 





## ----r------------------------------------------------------------------------


dist.a_logic1 = responses(a_logic1)


sel.al1 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.inc", 
            "ic.flip") 

resp.a_logic1 = select.dist(dist.a_logic1, sel.al1)
p = split.mat(a_logic1)
resp.a_logic1$ic.flip = replace(resp.a_logic1$correct, 
                                5, 
                                reflection(p$triangle, 1))

resp.a_logic1$d.union = cof(a_logic1$Sq1, 
                            pacman())

draw.dist(resp.a_logic1, n.resp = 8)

## ----r------------------------------------------------------------------------
# gemella a1_logic1 ------

a1_logic1a<- logic_rules(Raven(cof(vline(pos.x = 17, s.x = 15),
                                  vline(pos.x = -17, s.x = 15 ),
                                  hline(pos.y = 15, s.x=17),
                                  hline(pos.y = -15, s.x=17))),"OR")

petalo.su = cof(v.arc.left.up(), 
                v.arc.right.up(), 
                name="petalo.su", 
                single = T)
petalo.giu = cof(v.arc.left.down(), 
                v.arc.right.down(), 
                name="petalo.giu", 
                single = T)

petalo.sx = cof(h.arc.left.down(), 
                h.arc.left.up(), 
                name="petalo.sx", 
                single = T)
petalo.dx = cof(h.arc.right.down(), 
                h.arc.right.up(), 
                name="petalo.dx", 
                single = T)


a1_logic1b<-logic_rules(Raven(cof(petalo.giu, petalo.su, 
            petalo.sx, petalo.dx)),"AND")


a1_logic1 = com(a1_logic1a, a1_logic1b)

draw(a1_logic1, hide = F)


## ----r------------------------------------------------------------------------


dist.a1_logic1 = responses(a1_logic1)


sel.al11 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.inc", 
            "ic.flip") 

resp.a1_logic1 = select.dist(dist.a1_logic1, sel.al11)
resp.a1_logic1$ic.flip = cof(resp.a1_logic1$ic.inc, s.horizontal.inv())
resp.a1_logic1$d.union = cof(a1_logic1$Sq1, 
                            cross.dice())

draw.dist(resp.a1_logic1, n.resp = 8)

## ----r------------------------------------------------------------------------
a_logic2a<-logic_rules(Raven(cof(hexagon(shd="line1"),
                                 hexagon(shd="line2"),dot(),
                                 e.hexagon())),"AND")

a_logic2b<-logic_rules(Raven(cof(size(bow.tie(), 2),
                                 size(bow.tie.inv(), 2))),"XOR")
a_logic2 = com(a_logic2a, a_logic2b)

draw(a_logic2, hide =F)


## ----r------------------------------------------------------------------------
dist.a_logic2 = responses(a_logic2)

sel.al2 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.inc", 
            "ic.flip") 

resp.a_logic2 = select.dist(dist.a_logic2, sel.al2)
p = split.mat(a_logic2, cell = 3)
resp.a_logic2$ic.flip = cof(resp.a_logic2$ic.inc, p$triangle)

resp.a_logic2$d.union = cof(a_logic2$Sq5, 
                            luck(s.x = 17))

draw.dist(resp.a_logic2, n.resp = 8)




## ----r------------------------------------------------------------------------
a1_logic2a<-logic_rules(Raven(cof(pentagon(shd="line2"),
                                 pentagon(shd="line1"),dot(),
                                 pentagon())),"AND")

a1_logic2b<-logic_rules(Raven((cof(petalo.giu, petalo.su, 
            petalo.sx, petalo.dx))),"XOR")

a1_logic2 = com(a1_logic2a, a1_logic2b)

draw(a1_logic2, hide =F)

## ----r------------------------------------------------------------------------
dist.a1_logic2 = responses(a1_logic2)

sel.al12 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.inc", 
            "ic.flip") 

resp.a1_logic2 = select.dist(dist.a1_logic2, sel.al2)
resp.a1_logic2$ic.flip = cof(resp.a1_logic2$ic.inc, 
                             s.vertical.inv())

resp.a1_logic2$d.union = cof(a1_logic2$Sq4, 
                            size(cof(slice(), 
                                rotation(slice(), 5)), 2))

draw.dist(resp.a1_logic2, n.resp = 8)




## ----r------------------------------------------------------------------------
# vincolo non tangente non deve toccare interno con esterno
a_logic3a<- logic_rules(Raven(square4bis()),"OR")

a_logic3b<-logic_rules(Raven(cof((vline()), 
                                 (hline()), 
                                 (diagline()), 
                                 (diagline.inv()))),"AND")

a_logic3 = com(a_logic3a, a_logic3b)

draw(a_logic3, hide = F) 





## ----r------------------------------------------------------------------------


dist.a_logic3 = responses(a_logic3)


sel.al3 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.inc", 
            "ic.flip") 

resp.a_logic3 = select.dist(dist.a_logic3, sel.al3)
p = split.mat(a_logic3)
resp.a_logic3$ic.flip = replace(resp.a_logic3$correct, 
                                5, 
                                diagline.inv())

resp.a_logic3$d.union = cof(a_logic3$Sq1, 
                            e.hexagon())

draw.dist(resp.a_logic3, n.resp = 8)

## ----r------------------------------------------------------------------------
# gemella a1_logic1 ------

a1_logic3a<- logic_rules(Raven(cof(vline(pos.x = 17, s.x = 15),
                                  vline(pos.x = -17, s.x = 15 ),
                                  hline(pos.y = 15, s.x=17),
                                  hline(pos.y = -15, s.x=17))),"OR")



a1_logic3b<-logic_rules(Raven(cof(
  s_vertical(), s_horizontal(), diagline.inv(), diagline()
)),"AND")


a1_logic3 = com(a1_logic3a, a1_logic3b)

draw(a1_logic3, hide = F)


## ----r------------------------------------------------------------------------


dist.a1_logic1 = responses(a1_logic1)


sel.al11 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.inc", 
            "ic.flip") 

resp.a1_logic1 = select.dist(dist.a1_logic1, sel.al11)
resp.a1_logic1$ic.flip = cof(resp.a1_logic1$ic.inc, s.horizontal.inv())
resp.a1_logic1$d.union = cof(a1_logic1$Sq1, 
                            cross.dice())

draw.dist(resp.a1_logic1, n.resp = 8)

## ----r------------------------------------------------------------------------
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

## ----r------------------------------------------------------------------------
dist.a_visuo1 = responses(a_visuo1)

sel.av1 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.flip", 
            "ic.neg") 

resp.a_visuo1 = select.dist(dist.a_visuo1, sel.av1)
p = split.mat(a_visuo1)

resp.a_visuo1$ic.flip = cof(p$pentagon, 
                            rotation(p$coso, 3), 
                            p$circle)

resp.a_visuo1$d.union = cof(resp.a_visuo1$r.left, 
                            size(pie.4(), 2))

draw.dist(resp.a_visuo1, n.resp = 8)

## ----r------------------------------------------------------------------------

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


## ----r------------------------------------------------------------------------
dist.a1_visuo1 = responses(a1_visuo1)

sel.av11 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.flip", 
            "ic.neg") 

resp.a1_visuo1 = select.dist(dist.a1_visuo1, sel.av11)
p = split.mat(a1_visuo1)

resp.a1_visuo1$ic.flip = cof(p$e.hexagon, 
                            rotation(p$u.pie.2, 2), 
                            p$ellipse)

resp.a_visuo1$d.union = cof(resp.a_visuo1$r.left, 
                            size(pie.4(), 2))

draw.dist(resp.a1_visuo1, n.resp = 8)

## ----r------------------------------------------------------------------------

a2_visuo1c<-apply(Raven(cof(ellipse(s.x = 7,s.y = 5),
                           luck(s.x = 7,s.y = 5)),
                        c("trans.fill.line")))

a2_visuo1a<-apply(Raven(cof(e.hexagon(s.x = 17, s.y = 17), 
             
              pentagon(s.x = 17, s.y = 17), 
               square(s.x = 17, s.y = 17)),
                       c("diff_shapes"),
                       c("diff_shapes.inv")))

a2_visuo1b<-apply(Raven(pacman(size.x = 10), 
                       hrule = "rotation", 
                       vrule = "rotation.inv"))


a2_visuo1 = com(a2_visuo1a, a2_visuo1b, a2_visuo1c)
draw(a2_visuo1, hide = F)


## ----r------------------------------------------------------------------------
dist.a2_visuo1 = responses(a2_visuo1)

sel.av21 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.flip", 
            "ic.neg") 

resp.a2_visuo1 = select.dist(dist.a2_visuo1, sel.av21)
p = split.mat(a2_visuo1)

resp.a2_visuo1$ic.flip = cof(p$e.hexagon, 
                            rotation(p$pacman, 2), 
                            p$ellipse)

resp.a2_visuo1$d.union = cof(resp.a2_visuo1$r.left, 
                            size(pie.4(), 2))

draw.dist(resp.a2_visuo1, n.resp = 8)

## ----r------------------------------------------------------------------------
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





## ----r------------------------------------------------------------------------
dist.a_visuo2 = responses(a_visuo2)


sel.a_visuo2 = c("correct", "r.top", "r.diag", "wp.copy", 
            "wp.matrix" ,
            "d.union", 
            "ic.flip", # modifico pacman
            "ic.scale") 

resp.a_visuo2 = select.dist(dist.a_visuo2, 
                       sel.a_visuo2)

resp.a_visuo2$ic.flip = reflection(dist.a_visuo2$correct, 2)

draw.dist(resp.a_visuo2, n.resp = 8, main = T)

## ----r------------------------------------------------------------------------
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




## ----r------------------------------------------------------------------------
dist.a1_visuo2 = responses(a1_visuo2)


sel.dist.a1_visuo2 = c("correct", "r.top", "r.diag", "wp.copy", 
            "wp.matrix" ,
            "d.union", 
            "ic.flip", # modifico pacman
            "ic.scale") 

resp.a1_visuo2 = select.dist(dist.a1_visuo2, 
                       sel.dist.a1_visuo2)

resp.a1_visuo2$ic.flip = reflection(dist.a1_visuo2$correct, 2)

draw.dist(resp.a1_visuo2, n.resp = 8, main = T)

## ----r------------------------------------------------------------------------
a_3a = apply(
  Raven(
    st1 = cof(circle(s.x = 17, s.y = 17), 
              pentagon(s.x = 16, s.y = 16), 
              e.hexagon(s.x = 17, s.y = 17)), 
    vrule = c("diff_shapes"), 
    hrule = "diff_shapes"
  )
)

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

a_3 = com(a_3a, a_3b, a_3c)
draw(a_3, hide = F)



## ----r------------------------------------------------------------------------
dist.a3 = responses(a_3, choose.copy = 1)


sel.a3 = c("correct", "r.top", "r.diag", 
           "wp.copy", "wp.matrix", "d.union", 
           "ic.neg", "ic.flip")


resp.a3 = select.dist(dist.a3, 
                      sel.a3)

resp.a3$ic.flip = reflection(resp.a3$correct, 2)

draw.dist(resp.a3, n.resp = 8, main = T)

## ----r------------------------------------------------------------------------
a1_3a = apply(
  Raven(
    st1 = cof(e.hexagon(s.x = 17, s.y = 17), 
              square(s.x = 19, s.y = 19), 
              pentagon(s.x = 17, s.y = 17)), 
    vrule = c("diff_shapes"), 
    hrule = "diff_shapes"
  )
)

a1_3b = apply(
  Raven(
    st1 = pie.2(), 
    vrule = c("rotation"), 
    hrule = "rotation"
  )
)


a1_3c = apply(
  Raven(
    st1 = circle(s.x = 2, s.y = 2), 
    vrule = c("fill"), 
    hrule = "fill"
  )
)

a1_3 = com(a1_3a, a1_3b, a1_3c)
draw(a1_3, hide = F)




## ----r------------------------------------------------------------------------
dist.a13 = responses(a1_3, choose.copy = 1)


sel.a13 = c("correct", "r.top", "r.diag", 
           "wp.copy", "wp.matrix", "d.union", 
           "ic.neg", "ic.flip")


resp.a13 = select.dist(dist.a13, 
                      sel.a13)

p = split.mat(a1_3)

resp.a13$ic.flip = cof(p$pentagon, 
                       pie.2.inv(), 
                       p$circle)
resp.a13$d.union = cof(a1_3$Sq5, 
                       pie.4())

draw.dist(resp.a13, n.resp = 8, main = T)

