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





