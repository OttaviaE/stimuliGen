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

l.r = 7
l.x = 10

rombo4.1 = cof(cof(diagline(s.x = l.x, pos.x = l.r, pos.y = l.r), 
         diagline.inv(s.x = l.x, pos.x = -l.r, pos.y = l.r), 
         diagline.inv(s.x = l.x, pos.x = l.r, pos.y = -l.r), 
         diagline(s.x = l.x, pos.x = -l.r, pos.y = -l.r)))

giallo = "gold"

rosso = "firebrick"

ninja = (cof(luck(shd = "black"), 
         rotation(luck(shd = "black"), 3)))



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

a = hide(a_logic1$Sq7, c(1,3,5))

resp.a_logic1$ic.flip = cof(resp.a_logic1$ic.inc, 
                            a)

resp.a_logic1$d.union = cof(a_logic1$Sq1, 
                            cross.dice())

draw.dist(resp.a_logic1, n.resp = 8)

## ----r------------------------------------------------------------------------
# gemella a1_logic1 ------

# a1_logic1a<- logic_rules(Raven(cof(vline(pos.x = 17, s.x = 15),
#                                   vline(pos.x = -17, s.x = 15 ),
#                                   hline(pos.y = 15, s.x=17),
#                                   hline(pos.y = -15, s.x=17))),"OR")

a1_logic1a<- logic_rules(Raven(rombo4.1),"OR")
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
resp.a1_logic1$ic.flip = cof(resp.a1_logic1$ic.inc, petalo.su)
resp.a1_logic1$d.union = cof(cof(petalo.giu, petalo.su, 
            petalo.sx, petalo.dx), 
                            cross.dice())

draw.dist(resp.a1_logic1, n.resp = 8)

## ----r------------------------------------------------------------------------

pos.x = 0
cost.x = 9
cost.y = 5

maxi = cof(luck(pos.x = pos.x+cost.x, pos.y = pos.x, rot=pi, 
 s.x = cost.x, s.y=cost.y), 
luck(pos.x = pos.x-cost.x, pos.y = pos.x, rot=-pi, 
 s.x = cost.x, s.y=cost.y), 
luck(pos.x = pos.x, pos.y = pos.x+cost.x, rot=-pi, 
 s.x = cost.y, s.y=cost.x),
luck(pos.x = pos.x, pos.y = pos.x-cost.x, rot=-pi, 
 s.x = cost.y, s.y=cost.x)) 

a2_logic1b = logic_rules(Raven(size(maxi, 2)),"OR")

a2_logic1a<- logic_rules(Raven(square4bis()),"AND")


a2_logic1 = com(a2_logic1a, a2_logic1b)




draw(a2_logic1)


## ----r------------------------------------------------------------------------
dist.a2_logic1 = responses(a2_logic1)


sel.al21 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.inc", 
            "ic.flip") 

resp.a2_logic1 = select.dist(dist.a2_logic1, sel.al21)
p = split.mat(a2_logic1, cell = 3)
resp.a2_logic1$ic.flip = replace(dist.a2_logic1$correct, 
                                 1, 
                                 p$vline)
resp.a2_logic1$ic.inc = hide(resp.a2_logic1$correct, 
                             1)
resp.a2_logic1$d.union = cof(size(maxi, 2), cross.dice())

draw.dist(resp.a2_logic1, n.resp = 8)

## ----r------------------------------------------------------------------------
# vincolo non tangente non deve toccare interno con esterno
# a_logic2a<- logic_rules(Raven(square4bis()),"OR")
# 
# a_logic2b<-logic_rules(Raven(cof((vline()), 
#                                  (hline()), 
#                                  (diagline()), 
#                                  (diagline.inv()))),"AND")
# 
# a_logic2 = com(a_logic2a, a_logic2b)
# 
# draw(a_logic2, hide = F) 


size.x = 15*sqrt(2)/2
size.y = 15*sqrt(2)/2

a_logic2a<- logic_rules(Raven(cof(hline(s.x = size.x, 
                  s.y = size.y, 
                  pos.y=-size.x),
            vline(s.x = size.x, 
                  s.y = size.y, 
                  pos.x=size.x),
hline(pos.y=size.x, s.x = size.x, 
                  s.y = size.y),
vline(pos.x=-size.x, s.x = size.x, 
                  s.y = size.y))),"OR")

a_logic2b<-logic_rules(Raven(cof(slice(shd = "grey", lty = 0), 
                                 rotation(slice(shd = "grey", lty = 0), 3), 
                                 rotation(slice(shd = "grey", lty = 0), 5), 
                                 rotation(slice(shd = "grey", lty = 0), 7))),"XOR")

a_logic2a1 = apply(Raven(st1 = dot()))
a_logic2 = com(a_logic2b,a_logic2a1, a_logic2a )

draw(a_logic2, hide = F) 




## ----r------------------------------------------------------------------------


dist.a_logic2 = responses(a_logic2)


sel.al3 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.inc", 
            "ic.flip") 

resp.a_logic2 = select.dist(dist.a_logic2, sel.al3)
p = split.mat(a_logic2)
resp.a_logic2$ic.flip = replace(resp.a_logic2$correct, 
                                5, 
                                diagline.inv())

resp.a_logic2$d.union = cof(a_logic2$Sq1, 
                            pie.4())

resp.a_logic2$ic.inc = hide(resp.a_logic2$correct, 
                            3)

resp.a_logic2$ic.flip = cof( a_logic2$Sq7, 
                             resp.a_logic2$ic.inc 
                           )
draw.dist(resp.a_logic2, n.resp = 8)



