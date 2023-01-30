## ----setup, include=FALSE-----------------------------------------------------
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


square4bis <- function() {
value <-cof(hline(pos.y=-11),vline(pos.x=11),
hline(pos.y=11),vline(pos.x=-11))
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



## -----------------------------------------------------------------------------

draw(rectangle(s.x=-50,s.y=50,shd=blu,pos.x=+10,pos.y=-7))
draw(diagline(pos.x=-50, s.x = 50, lwd = 3, shd = giallo), canvas = F)
for(i in seq(-50, 50, by = 3.5)) {
draw(diagline(pos.x = i, s.x=40, lwd = 3), 
 canvas = F)
}


draw(diagline.inv(pos.x=-50, s.x = 50, lwd = 3), canvas = F)
for(i in seq(-50, 50, by = 3.5)) {
draw(diagline.inv(pos.x = i, s.x=40, lwd = 3), 
 canvas = F)
}


draw(rectangle(s.x=7,s.y=5,shd="white",pos.x=+10,pos.y=-7),
 canvas = FALSE)

## ----out.width="80%"----------------------------------------------------------

par(mfrow =c(2, 3), mar = c(0.5, 6, 0.5, 2) + .1, 
mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )

container = rectangle(s.x=7,s.y=5,shd=blu)

draw(container,xlim = 8, main = "correct")
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
canvas = FALSE)

draws(rectangle(s.x=7,s.y=5,shd="line.12"),by=3.5,
canvas = FALSE)

draw(container,xlim = 8, main = "ic.scale")
draws(rectangle(s.x=7,s.y=5,shd="line.12"),
canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),
canvas = FALSE)

draw(container,xlim = 8, main = "difference")

draw(container,xlim = 8, main = "difference")
for (i in seq(-6, 7, by = 3)) {
for (j in seq(-3, 3, by = 3)) {
draw(dot(pos.x = i, pos.y = j, size.x = 0.5, size.y = 0.5), canvas = F)
}
}


draw(container,xlim = 8, main = "ic.flip")
for(i in seq(-7, 7, by = 1)) {
draw(vline(pos.x = i, s.x=5, lwd = 3), canvas = F)
}

for(i in seq(-5, 5, by = 1)) {
draw(hline(pos.y = i, s.x=7, lwd = 3), canvas = F)
}


## -----------------------------------------------------------------------------
draw(rectangle(s.x=-50,s.y=50,shd=giallo,pos.x=+10,pos.y=-7))
draw(dot(pos.x = -30, pos.y = 15, size.x = 1, size.y = 1, shd = blu), canvas = F)

for (i in seq(-30, 30, by = 4)) {
for (j in seq(15, -19, by = -4)) {
draw(dot(pos.x = i, pos.y = j, size.x = 1, size.y = 1, shd = blu), canvas = F)
}
}

draw(rectangle(s.x=7,s.y=5,shd="white",pos.x=+15,pos.y=-10),
 canvas = FALSE)


## ----out.width="80%"----------------------------------------------------------

par(mfrow =c(2, 3), mar = c(0.5, 6, 0.5, 2) + .1, 
mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )

container = rectangle(s.x=7,s.y=5,shd=giallo)

draw(container,xlim = 8, main = "correct")
clip(7,-7,5,-5)
for (i in seq(-21, 25, by = 4)) {
for (j in seq(12.5, -15, by = -4)) {
draw(dot(pos.x = i, pos.y = j, size.x = 1, size.y = 1, shd = blu), canvas = F)
}
}

draw(container,xlim = 8, main = "ic.neg")
clip(7,-7,5,-5)
draw(rectangle(s.x=7,s.y=5,shd=blu,pos.x=0,pos.y=0),
 canvas = FALSE)
for (i in seq(-25, 25, by = 4)) {
for (j in seq(15, -15, by = -4)) {
draw(dot(pos.x = i, pos.y = j, size.x = 1, size.y = 1, shd = giallo), canvas = F)
}
}


draw(container,xlim = 8, main = "difference")


draw(container,xlim = 8, main = "difference1")
clip(7,-7,5,-5)
for (i in seq(-21, 25, by = 4)) {
for (j in seq(12.5, -15, by = -4)) {
draw(square(pos.x = i, pos.y = j, s.x = 1, s.y = 1, shd = blu), canvas = F)
}
}


draw(container,xlim = 8, main = "boh")
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12"),by=3.5,
canvas = FALSE)


## -----------------------------------------------------------------------------

draw(rectangle(s.x=-50,s.y=50,shd=blu,pos.x=+10,pos.y=-7))
draw(vline(pos.x = -20, s.x = 50, lwd = 3), canvas = F)
draw(vline(pos.x = 20, s.x = 50, lwd = 3, lty = 3), canvas = F)
draw(vline(pos.x = 17, s.x = 50, lwd = 3), canvas = F)
draw(hline(pos.y = 9, s.x = 50, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = -9, s.x = 50, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = -12, s.x = 50, lwd = 3, lty = 3), canvas = F)


draw(rectangle(s.x=7,s.y=5,shd="white",pos.x=+15,pos.y=-10),
 canvas = FALSE)

## ----out.width="80%"----------------------------------------------------------

par(mfrow =c(2, 3), mar = c(0.5, 6, 0.5, 2) + .1, 
mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )

container = rectangle(s.x=7,s.y=5,shd=blu)

draw(container,xlim = 8, main = "correct")

draw(rectangle(s.x=7,s.y=5,shd=NA,pos.x=0,pos.y=0),
 canvas = FALSE)

draw(vline(pos.x = 5, pos.y = -0, 
 s.x= 5, lwd = 3, lty = 3), canvas = F)
draw(vline(pos.x = 2, pos.y = 0, 
 s.x = 5, lwd = 3), canvas = F)

draw(hline(pos.y = 0, 
 pos.x = 0,s.x = 7, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = -3, pos.x = 0, s.x = 7, lwd = 3, lty = 3), canvas = F)

##primo distrattore
draw(container,xlim = 8, main = "ic")

draw(rectangle(s.x=7,s.y=5,shd=NA,pos.x=0,pos.y=0),
 canvas = FALSE)

draw(vline(pos.x = 5, pos.y = -0, 
 s.x= 5, lwd = 3, lty = 1), canvas = F)
draw(vline(pos.x = 2, pos.y = 0, 
 s.x = 5, lwd = 3), canvas = F)

draw(hline(pos.y = 0, 
 pos.x = 0,s.x = 7, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = -3, pos.x = 0, s.x = 7, lwd = 3, lty = 1), canvas = F)

##Secondo distrattore

draw(container,xlim = 8, main = "ic")

draw(rectangle(s.x=7,s.y=5,shd=NA,pos.x=0,pos.y=0),
 canvas = FALSE)

draw(vline(pos.x = -5, pos.y = -0, 
 s.x= 5, lwd = 3, lty = 3), canvas = F)
draw(vline(pos.x = -2, pos.y = 0, 
 s.x = 5, lwd = 3), canvas = F)

draw(hline(pos.y = 0, 
 pos.x = 0,s.x = 7, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = 3, pos.x = 0, s.x = 7, lwd = 3, lty = 3), canvas = F)

##terzo distrattore
draw(container,xlim = 8, main = "wp")
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
canvas = FALSE)

##quarto
draw(container,xlim = 8, main = "difference")


## -----------------------------------------------------------------------------

draw(rectangle(s.x=-50,s.y=50,shd=giallo,pos.x=+10,pos.y=-7))
draw(vline(pos.x=-50, s.x = 30, lwd = 1), canvas = F)
for(i in seq(-30, 30, by = 2)) {

draw(vline(pos.x = i, s.x=40, lwd = 1+ abs(i)/2), 
 canvas = F,bg = "white")
}

draw(rectangle(s.x=7,s.y=5,shd="white",pos.x=+15,pos.y=-10),
 canvas = FALSE)

## ----out.width="80%"----------------------------------------------------------
par(mfrow =c(2, 3), mar = c(0.5, 6, 0.5, 2) + .1, 
mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )

container = rectangle(s.x=7,s.y=5,shd=giallo)

draw(container,xlim = 8, main = "correct")
clip(7,-7,5,-5)
#for(i in seq(14, 30, by = 1)) {
for(i in seq(6, 22, by = 2)) {

draw(vline(pos.x = i-15, s.x=40, lwd = 1+ abs(i)/2), 
 canvas = F,bg = "white")
}


draw(container,xlim = 8, "wp")
clip(7,-7,5,-5)
for(i in seq(14, 30, by = 1)) {

draw(vline(pos.x = i-22, s.x=40, lwd = 5), 
 canvas = F,bg = "white")
}

draw(container,xlim = 8, main = "difference")

draw(container,xlim = 8, main = "boh")
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
canvas = FALSE)

draw(container,xlim = 8, "wp")
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12"),by=3.5,
canvas = FALSE)

## -----------------------------------------------------------------------------
draw(rectangle(s.x=-50,s.y=50,pos.x=+10,pos.y=-7),bg=blu)

for(j in seq(0, 3, by = .2)) {
draw(diagline(pos.x = 30,pos.y = 10, s.x=100, rot=(pi/8)*j, lwd = 3), 
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

## ----out.width="80%"----------------------------------------------------------
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


## ----young006-----------------------------------------------------------------
young006 = apply(Raven(
st1 = cof( 
circle(shd = blu), 
triangle(pos.y = 0, s.x = 12, s.y = 12, shd = giallo)), 
vrule = "reflection", 
hrule = "identity"
))
draw(young006, n.cell = 4, hide = F)

## ----out.width="80%"----------------------------------------------------------
dist_young006 = responses(young006,mat.type = 4, which.element = "triangle")

selection = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")




resp_young006 = select.dist(dist_young006, selection)

resp_young006[["d.union"]] =cof(resp_young006[["d.union"]], 
square())
resp_young006[["wp.matrix"]] =cof(resp_young006[["wp.matrix"]], 
e.hexagon())

p = split.mat(young006)

resp_young006[["ic.flip"]] = cof(p$circle, 
 rotation(p$triangle, 3))


draw.dist(resp_young006, n.resp = 5, main = T)

## -----------------------------------------------------------------------------
young007a = apply(Raven(
st1 = pacman(size.x = 7, shd = rosso), 
hrule = "reflection", 
vrule = "reflection"
))


young007b = apply(Raven(
st1 = pentagon(shd = giallo), 
hrule = "identity", 
vrule = "identity"
))

young007c = apply(Raven(
st1 = size(dot(shd = "black"),2), 
hrule = "identity", 
vrule = "identity"
))



young007 = com(young007b,  young007a, young007c)
draw(young007, n.cell = 4)


## ----out.width="80%"----------------------------------------------------------
dist_young007 = responses(young007,mat.type = 4)

selection = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")

resp_young007 = select.dist(dist_young007, selection)

resp_young007[["d.union"]] = cof(resp_young007[["d.union"]], 
 pie.4())
resp_young007[["wp.matrix"]] = cof(resp_young007[["wp.matrix"]], 
 dice())
p = split.mat(young007, mat.type= 4)

resp_young007[["ic.flip"]] = cof((p[[1]]), 
 reflection(p[[2]], 2), p$dot) 

draw.dist(resp_young007, n.resp = 5, main = T)

## -----------------------------------------------------------------------------

young008a = apply(Raven(
st1 = cof(dot(shd = "white"), dot(shd = "black"), 
          dot(shd = "grey")), 
vrule = "diff_shapes"
))

young008b = apply(Raven(
st1 = pacman(shd = rosso), 
"reflection"
))

young008 = com(young008b, young008a)

draw(young008, n.cell = 4)

## ----out.width="80%"----------------------------------------------------------
dist_young008 = responses(young008, mat.type = 4)
selection = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")
resp_young008 = select.dist(dist_young008, selection)

resp_young008[["wp.matrix"]] = cof(resp_young008[["wp.matrix"]], 
 rotation(pacman(shd = rosso), 3))
resp_young008[["d.union"]] = cof(resp_young008[["d.union"]], 
 pentagon())
p = split.mat(young008, mat.type = 4)

resp_young008[["ic.flip"]] = cof(rotation(p[[1]], 3),
 p[[2]])

draw.dist(resp_young008, n.resp = 5)


## -----------------------------------------------------------------------------
young009a = apply(Raven(
st1 = cof(s.lilth, 
pacman(size.x = 5), ellipse()),
vrule = "diff_shapes"

))

young009b = apply(Raven(
st1 = cof(e.hexagon(shd = blu), 
circle(shd = giallo,s.x = 15, s.y = 15), 
triangle()), 
vrule = "diff_shapes"
))

young009 = com(young009b, young009a)

draw(young009, n.cell = 4)


## ----out.width="80%"----------------------------------------------------------

dist_young009 = responses(young009, mat.type = 4)

resp_young009 = select.dist(dist_young009, selection)

resp_young009[["d.union"]] = cof(resp_young009[["d.union"]], 
 cross.dice())
resp_young009[["wp.matrix"]] = cof(resp_young009[["wp.matrix"]], 
 pacman())

p = split.mat(young009, mat.type= 4)

resp_young009[["ic.flip"]] = cof(
p[[1]], reflection(p[[2]], 2))

draw.dist(resp_young009, n.resp = 5)


## -----------------------------------------------------------------------------

young010a = apply(Raven(
st1 = cof(u.pie.2(size.x = 5, shd = blu),
u.pie.4(size.x = 5, shd = rosso), ellipse()),
vrule = "diff_shapes"

))

young010b = apply(Raven(
st1 = cof(e.hexagon(shd = giallo, s.x = 13, s.y = 13),
triangle(shd = giallo, s.x = 15, s.y = 15),
circle()),
hrule = "diff_shapes"
))

young010 = com(young010b, young010a)

draw(young010, n.cell = 4)

## ----out.width="80%"----------------------------------------------------------

dist_young010 = responses(young010, mat.type = 4)
resp_young010 = select.dist(dist_young010, selection)

resp_young010[["d.union"]] = cof(resp_young010[["d.union"]], 
 cross(), 
 pentagon())

resp_young010[["wp.matrix"]] = cof(resp_young010[["wp.matrix"]], 
 triangle())
p = split.mat(young010,mat.type= 4)


resp_young010[["ic.flip"]] = cof(reflection(p[[1]], 2), 
 p[[2]])

draw.dist(resp_young010, n.resp = 5)


## -----------------------------------------------------------------------------


young011a = apply(Raven(
st1 = cof(pentagon(shd = giallo, s.x = 13, s.y = 13),
triangle(shd = giallo, s.x = 15, s.y = 15),
circle()),
vrule = "reflection", 
hrule = c( "reflection", "diff_shapes")
))


young011b = apply(Raven(
st1 = semi.circle(size.x = 5, shd = rosso), 
hrule = "reflection", 
vrule = "reflection"
))

young011 = com(young011a, young011b)

draw(young011, n.cell = 4)

## ----out.width="80%"----------------------------------------------------------

# per ragioni a me ignote, non va il codice dei distrattori (anche se singolarmente funziona)
# ergo li faccio mano
#dist_young011 = responses(young011, mat.type = 4) 



#selection = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")
#resp_young011 = select.dist(dist_young011, selection)
resp_young011 = list()
resp_young011$correct = correct(young011, mat.type = 4)
resp_young011$d.union = cof(reflection(young011$Sq1, 2), dice())
resp_young011[["wp.matrix"]] = cof(young011$Sq1, (young011$Sq2))
resp_young011$r.top = repetition(young011, mat.type = 4)$r.top
p = split.mat(young011)

resp_young011$ic.flip = replace(resp_young011$correct, 4, reflection(p$semi.circle, 2))

draw.dist(resp_young011, n.resp = 5)

## -----------------------------------------------------------------------------

young012a = apply(Raven(
st1 = cof(pentagon(shd = giallo, s.x = 13, s.y = 13),
triangle(shd = giallo, s.x = 15, s.y = 15),
circle()),
vrule = "diff_shapes",
hrule = "reflection"
))

young012b = apply(Raven(
st1 = dot(size.x = 2, size.y = 1, shd = rosso)
))

young012 = com(young012a, young012b)
draw(young012, n.cell = 4)

## ----out.width="80%"----------------------------------------------------------
dist_young012 = responses(young012, mat.type = 4)

resp_young012 = select.dist(dist_young012, selection)

resp_young012[["d.union"]] = cof(resp_young012[["d.union"]], 
 size(square(shd = rosso), 3))

resp_young012[["wp.matrix"]] =cof(resp_young012[["r.top"]], 
resp_young012[["wp.matrix"]])

p = split.mat(young012, mat.type = 4)



resp_young012[["ic.flip"]] =cof(rotation(p[[1]], 3), 
p[[2]])

draw.dist(resp_young012, n.resp = 5)


## -----------------------------------------------------------------------------
young013a = apply(
Raven(
st1 = pentagon(rot=pi/2), 
vrule = "reflection"
)
)

young013b = apply(
Raven(
st1 = size(dot(), 2), 
vrule = "identity"
)
)

young013 = com(young013a, young013b)

draw(young013, n.cell = 4)




## ----out.width="80%"----------------------------------------------------------
sely013 = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")


dist_young013 = responses(young013,mat.type = 4)
resp_young013 = select.dist(dist_young013, sely013)
resp_young013$ic.flip = rotation(resp_young013$correct, 6)

draw.dist(resp_young013, n.resp = 5, main = T)

## -----------------------------------------------------------------------------
young014a = apply(
Raven(
st1 = pacman(), 
vrule = "reflection",
hrule = "reflection"
)
)


young014b = apply(
Raven(
st1 = size(dot(), 2), 
vrule = "identity"
)
)


young014 = com(young014a, young014b)


draw(young014, n.cell = 4)


## ----out.width="80%"----------------------------------------------------------
dist_young014 = responses(young014,mat.type = 4)
sely014 = sely013
resp_young014 = select.dist(dist_young014, sely014)
resp_young014$ic.flip = rotation(resp_young014$correct, 
                                   7)
draw.dist(resp_young014, n.resp = 5, main = T)

## -----------------------------------------------------------------------------
young015a = apply(
Raven(
st1 = triangle(), 
vrule = "fill",
hrule = "reflection"
)
)


young015b = apply(
Raven(
st1 = size(dot(), 2), 
vrule = "identity"
)
)


young015 = com(young015a, young015b)

draw(young015, n.cell = 4)



## ----out.width="80%"----------------------------------------------------------

dist_young015 = responses(young015,mat.type = 4)
sely015 = sely013
resp_young015 = select.dist(dist_young015, sely015)

resp_young015$ic.flip = rotation(resp_young015$correct, 
                                 3)

draw.dist(resp_young015, n.resp = 5, main = T)

## -----------------------------------------------------------------------------
young016a = apply(
Raven(
st1 = cof(square(),e.hexagon(),pacman()), 
vrule = c("diff_shapes","fill")
)
)

young016b = apply(
Raven(
st1 = cof(size(u.star(), 2),
          size(pacman(shd = "black"), 2),
          size(dot(), 2)), 
vrule = "diff_shapes"
)
)


young016 = com(young016a, young016b)
draw(young016, n.cell = 4)





## ----out.width="80%"----------------------------------------------------------

dist_young016 = responses(young016,mat.type = 4)
sely016 =  c("correct", "r.top", "d.union", "wp.matrix", "ic.neg")
resp_young016 = select.dist(dist_young016, sely016)

resp_young016[["wp.matrix"]] = cof(resp_young016[["wp.matrix"]] , 
 resp_young016[["r.top"]] )


p = split.mat(young016, mat.type = 4)

resp_young016$ic.neg = cof(change.col(p$e.hexagon), 
                    p$pacman)


#resp_young016$ic.neg$shade = "black"
draw.dist(resp_young016, n.resp = 5, main = T)


## -----------------------------------------------------------------------------
young017a = apply(
Raven(
st1 = cof(luck(),circle(),pacman()), 
vrule = c("fill"), 
hrule = "diff_shapes"
)
)

young017b = apply(
Raven(
st1 = cof(dot(),triangle(s.x=3,s.y = 3, shd = "black"),pacman()), 
hrule = c("diff_shapes")
)
)

young017<- com(young017a,young017b)
draw(young017, n.cell = 4)



## ----out.width="80%"----------------------------------------------------------
dist_young017 = responses(young017,mat.type = 4)
sely017 =  c("correct", "r.left", "d.union", "wp.matrix", "ic.neg")
resp_young017 = select.dist(dist_young017, sely017)

# p = split.mat(young017, mat.type = 4)
# 
# 
# p$triangle$shade = "black"
# resp_young017$ic.neg = cof(p$circle, 
#p$triangle)

p = split.mat(young017, mat.type = 4)

resp_young017$ic.neg = cof(change.col(p$circle), 
                    p$triangle)
draw.dist(resp_young017, n.resp = 5, main = T)



## -----------------------------------------------------------------------------
young018a = apply(
Raven(
st1 = cof(pentagon(),triangle() ,pacman()), 
vrule = c("diff_shapes","reflection")
)
)

young018b = apply(Raven(
st1 = slice(s.x = 7), 
hrule = "reflection", 
vrule = "reflection"
))

young018c = apply(Raven(
  st1 = size(dot(), 2)
))

young018 = com(young018a, young018b, young018c)

draw(young018, n.cell = 4)




## ----out.width="80%"----------------------------------------------------------
dist_young018 = responses(young018,mat.type = 4)
sely018 = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")
resp_young018 = select.dist(dist_young018, sely018)

p = split.mat(young018)

resp_young018$ic.flip = replace(resp_young018$correct, 
                                 2, 
                                (triangle()))

draw.dist(resp_young018, n.resp = 5, main = T)

## -----------------------------------------------------------------------------
young019a = apply(
Raven(
st1 = cof(triangle(rot = pi) ,pacman(),square()), 
vrule = c("diff_shapes"),
hrule = c("reflection")
)
)

young019b = apply(Raven(
  st1 = size(dot(), 2)
))


young019 = com(young019a, young019b)

draw(young019, n.cell = 4)




## ----out.width="80%"----------------------------------------------------------
dist_young019 = responses(young019,mat.type = 4)
sely019 = sely018
resp_young019 = select.dist(dist_young019, sely018)

resp_young019$ic.flip = rotation(resp_young019$correct, 
                                 7)


draw.dist(resp_young019, n.resp = 5, main = T)


## -----------------------------------------------------------------------------
young020 = obj_addition_rules(
Raven(
st1 = cof(pacman(shd = giallo), dot()) 
), rule="v.add"
)

young020frame =apply(Raven(
st1 = rectangle(s.x = 20, s.y = 15, shd = blu)
))

young020 = com(young020frame, young020)
draw(young020, n.cell = 4)



## ----out.width="80%"----------------------------------------------------------
selection.neg = c("correct", "r.top", "d.union", "wp.matrix", "ic.inc")
dist_young020 = responses(young020, mat.type = 4)



resp_young020 = select.dist(dist_young020, 
selection.neg)

resp_young020[["wp.matrix"]] = cof(resp_young020[["wp.matrix"]], 
 rotation(square(), 2))

# p = split.mat(young020)
# 
# resp_young020[["ic.scale"]] = cof(p[[1]], 
#size(p[[2]], 2), 
#size(p[[3]], 2))
# 
# resp_young020 = select.dist(dist_young020, selection.neg)
# 
# resp_young020[["wp.matrix"]] = cof(resp_young020[["wp.matrix"]] , 
#dot() )
# resp_young020[["ic.inc"]] = show(resp_young020[["ic.inc"]] ,index=1:2 )
# resp_young020[["ic.inc"]] = hide(resp_young020[["ic.inc"]], 3 )


draw.dist(resp_young020, n.resp = 5, main = T)

## -----------------------------------------------------------------------------
young021a = obj_addition_rules(
Raven(
st1 = cof(s.lily(),size(u.biscuit, 3)) 
), rule="vh.sott"
)

young021b = apply(
Raven(
st1 =e.hexagon(shd="firebrick") )
)

young021<-com(young021b,young021a)
draw(young021, n.cell = 4, bg="white")


## ----out.width="80%"----------------------------------------------------------
dist_young021 = responses(young021,mat.type = 4)


#draw.dist(resp_young021, n.resp = 5, main = T)


sely021 = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")
resp_young021 = select.dist(dist_young021, sely021)
resp_young021$ic.flip = rotation(resp_young021$correct, 
                                 3)
draw.dist(resp_young021, n.resp = 5, main = T)



## -----------------------------------------------------------------------------
young022a = obj_addition_rules(
Raven(
st1 = cof(cross(),vertical.eight())
)
, rule="vh.sott"
)

young022b = apply(
Raven(
st1 =square(shd = "gold") )
)

young022<-com(young022b,young022a)



draw(young022, n.cell = 4)



## ----out.width="80%"----------------------------------------------------------
dist_young022 = responses(young022,mat.type = 4)

selec_add = c("correct", "r.top", 
"d.union", "ic.scale", "wp.matrix")
resp_young022 = select.dist(dist_young022, 
selec_add)


#draw.dist(resp_young022, n.resp = 5, main = T)

selection.y022 = c("correct", "r.top", "d.union", "wp.matrix", "ic.inc")
resp_young022 = select.dist(dist_young022, selection.y022)

resp_young022[["d.union"]] = cof(dist_young022$r.top, 
 bow.tie.inv())



draw.dist(resp_young022, n.resp = 5, main = T)


## -----------------------------------------------------------------------------

young023b = obj_addition_rules(
Raven(
st1 = cof(size(u.bow.tie(shd = "grey"), 2), size(horizontal.eight(), 2)) 
), rule="v.sott"
)


young023a = apply(Raven(
  st1 = square(rot = pi)
))

young023 = com(young023a, young023b)


draw(young023, n.cell = 4)


## ----out.width = "80%"--------------------------------------------------------
selection.neg = c("correct", "r.top", "d.union", "wp.matrix", "ic.inc")
dist_young023 = responses(young023,mat.type = 4)


resp_young023 = select.dist(dist_young023, 
selection.neg)


#draw.dist(resp_young023, n.resp = 5, main = T)

#resp_young023 = select.dist(dist_young023, selection.neg)
resp_young023$d.union = cof(young023$Sq1, cross.dice())

draw.dist(resp_young023, n.resp = 5, main = T)


## -----------------------------------------------------------------------------
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


## ----out.width="80%"----------------------------------------------------------

selection.neg = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")
dist_young024 = responses(young024,mat.type = 4)
selec_add = c("correct", "r.top", 
"d.union", "ic.scale", "wp.matrix")

resp_young024 = select.dist(dist_young024, 
selec_add)
resp_young024[["d.union"]] = cof(resp_young024$d.union, 
 size(biscuit, 1))

#draw.dist(resp_young024, n.resp = 5, main = T)


resp_young024 = select.dist(dist_young024, selection.neg)
resp_young024$ic.flip = reflection(resp_young024$correct, 
                                 2)
resp_young024$d.union = cof(young024$Sq1, 
                            square4())
draw.dist(resp_young024, n.resp = 5, main = T)


## -----------------------------------------------------------------------------
young025a = obj_addition_rules(
Raven(
st1 = cof(cof(circle(s.x = 5,s.y = 5),size(dot(), 2),name="oggetto",single=TRUE ),
size(dice(), 2)) 
), rule="vh.add"
)

young025b = apply(
Raven(
st1 =luck(s.x = 8, s.y = 10) )
)

young025<-com(young025a,young025b)
draw(young025, n.cell = 4)

## ----out.width="80%"----------------------------------------------------------
dist_young025 = responses(young025,mat.type = 4)

sel025 = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")
resp_young025 = select.dist(dist_young025, 
sel025)
resp_young025[["d.union"]] = cof(resp_young025$d.union, 
 bow.tie())

p = split.mat(young025)

resp_young025[["ic.scale"]] = cof(size(p[[1]], 2), p[[2]],
size(p[[3]], 2))


# draw.dist(resp_young025, n.resp = 5, main = T)

resp_young025 = select.dist(dist_young025, sel025)


resp_young025$wp.matrix = cof(resp_young025$wp.matrix, 
rotation(resp_young025$wp.matrix, 3))

resp_young025$d.union = cof(young025$Sq2, X())
draw.dist(resp_young025, n.resp = 5, main = T)


## -----------------------------------------------------------------------------
set.seed(999)
young026a = apply(
Raven(
st1 = cof(square(shd = giallo), u.bow.tie(shd = giallo), triangle(rot = pi/2, shd = giallo)), 
vrule = c("diff_shapes"), 
hrule = "identity"
)
)

young026b = apply(Raven(
st1 = cof(dot(shd = blu))
))

young026 = com(young026a, young026b)
draw(young026, n.cell = 9)



## ----out.width="80%"----------------------------------------------------------


p = split.mat(young026)


selection.neg = c("correct", "r.diag", "d.union","wp.copy","wp.matrix", "ic.scale","ic.flip","ic.inc")

dist_young026 = responses(young026,mat.type = 9)
resp_young026 = select.dist(dist_young026, selection.neg)

# avendo cambiato il codice devo fare a mano ic flip 

resp_young026[["ic.flip"]] = replace(correct(young026), 
 3, 
 reflection(p$triangle, 2))



draw.dist(resp_young026, n.resp = 8, main = T)




## -----------------------------------------------------------------------------
young027a= apply(
Raven(
st1 = cof(pentagon(shd = rosso)), 
vrule = "identity"
)
)

young027b = apply(Raven(
st1 = cof(dot(shd = giallo, size.x = 7)), 
hrule = "size"
))


young027 = com(young027a, young027b)

draw(young027, hide = F)

## ----out.width="80%"----------------------------------------------------------

dist_young027 = responses(young027,mat.type = 9, choose.copy = 1)
sel.y027 = c("correct", "r.diag", "d.union","wp.copy","wp.matrix", "ic.scale","ic.flip","ic.inc")
resp_young027 = select.dist(dist_young027, sel.y027)

# ic flip non ha funzionato sul quadrato perché giustamente non funziona più sul quadrato
p = split.mat(young027)
m = correct(young027)
resp_young027$ic.flip = cof(reflection(p$pentagon, 2), p$dot)


resp_young027[["wp.copy"]] = young027$Sq1
resp_young027$d.union = cof(young027$Sq1, dice())


draw.dist(resp_young027, n.resp = 8, main = T)

## -----------------------------------------------------------------------------
young028a = apply(Raven(
st1 = square(shd = giallo)
))

young028b = apply(Raven(
st1 = pacman(), 
hrule = "fill"
))

young028 = com(young028a, young028b)
draw(young028)

## ----out.width="80%"----------------------------------------------------------

dist_young028 = responses(young028,mat.type = 9, choose.copy = 1)
sel6 = c("correct", "r.diag", "d.union","wp.copy","wp.matrix", "ic.scale","ic.flip","ic.inc")

resp_young028 = select.dist(dist_young028, 
sel6)
p = split.mat(young028)
resp_young028$ic.flip = replace(resp_young028$ic.flip, 2, reflection(p$pacman, 2))

# p = split.mat(young028)
# resp_young028$ic.inc = p$square
# 
# resp_young028$ic.scale = cof(p$square, 
#size(p$pacman, 2))
# 
# resp_young028$ic.flip = cof(p$square, 
#reflection(p$pacman, 2))
# 
# resp_young028$wp.copy = young028$Sq1

draw.dist(resp_young028, n.resp = 8, main = T)





## -----------------------------------------------------------------------------
young029a = apply(Raven(
st1=rectangle(shd = blu, s.x = 20, s.y = 15)
))



young029b= apply(
Raven(
st1 = cof(pentagon(shd = "black", s.x = 12, s.y = 12), u.star(), square(shd = "black")), 
vrule = c("diff_shapes", "size"), 
hrule = "identity"
)
)

young029 = com(young029a, young029b)

draw(young029)

## ----out.width="80%"----------------------------------------------------------
dist_young029 = responses(young029)

select.new1 = c("correct","r.diag", "d.union", "wp.copy", 
"wp.matrix", "ic.flip", "ic.neg", 
"ic.scale")

resp_young029 = select.dist(dist_young029, 
select.new1)


resp_young029$d.union = cof(resp_young029$d.union, 
luck(shd = "white"), 
smallbow.tie.inv(shd = "black"))


p =split.mat(young029)

resp_young029$ic.scale = cof(p$rectangle, square(shd = "black")) 


draw.dist(resp_young029, n.resp = 8, main = T)


## -----------------------------------------------------------------------------
young030a = apply(Raven(
st1 = cof(square(s.x = 18, s.y = 18), e.hexagon(), 
pentagon()), 
hrule = "diff_shapes"
))

young030b = apply(Raven(
st1 = pacman(shd = rosso, size.x = 8), 
vrule = "size"

))


young030 = com(young030a, young030b)

draw(young030)


## ----out.width="80%"----------------------------------------------------------
dist_young030 = responses(young030)
select.new = c("correct", "r.diag", "wp.copy", "wp.matrix", "d.union", "ic.scale", "ic.inc", "ic.flip")
resp_young030 = select.dist(dist_young030, 
select.new)
p= split.mat(young030)
# 
resp_young030$ic.flip = cof(p$pentagon,
reflection(p$pacman, 2))

draw.dist(resp_young030, n.resp = 8)




