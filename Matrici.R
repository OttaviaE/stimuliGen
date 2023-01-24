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
rectangle(s.x = 25, s.y = 15, pos.y = -1, shd = blu), 
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

resp_young006[["ic.flip"]] = cof(p$rectangle, 
 rotation(p$triangle, 2))


draw.dist(resp_young006, n.resp = 11, main = T)

