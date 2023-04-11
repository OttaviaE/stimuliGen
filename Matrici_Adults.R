## ----setup, include=FALSE-----------------------------------------------------
set.seed(999)
knitr::opts_chunk$set(echo=FALSE, 
                      eval=TRUE, 
                      warning = FALSE, 
                      message = FALSE, 
                      fig.align = "center")
knitr::knit_hooks$set(purl = knitr::hook_purl)

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
    theta.1  = list(0),
    theta.2  = list(0),
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

selec_add = c("correct", "r.top", 
              "d.union", "ic.scale", "wp.matrix")

select.new = c("correct", "r.diag", 
               "wp.copy", "wp.matrix", 
               "ic.inc", "ic.scale", "ic.flip", "d.union")



## -----------------------------------------------------------------------------
adult005a= apply(
  Raven(
    st1 = cof(pentagon(), triangle(), square()), 
    hrule = c("diff_shapes"), 
    vrule = "identity"
  )
)

adult005b = apply(Raven(
  st1 = cof(dot(size.x = 7)), 
  hrule = "size"
))


adult005 = com(adult005a, adult005b)

#draw(adult005, hide = F)

## ----out.width="80%"----------------------------------------------------------

dist_adult005 = responses(adult005,mat.type = 9)
resp_adult005 = select.dist(dist_adult005, selection.neg)

# ic flip non ha funzionato sul quadrato perché giustamente non funziona più sul quadrato
p = split.mat(adult005)

resp_adult005[["ic.flip"]] = replace(correct(adult005), 
                                     3, 
                                     rotation(p$square, 2)) 


resp_adult005[["wp.copy"]] = adult005$Sq1

##draw.dist(resp_adult005, n.resp = 8, main = T)

## -----------------------------------------------------------------------------
adult012a =  apply(Raven(
  st1 = cof(rectangle(s.x = 20, s.y = 15), 
            pentagon(s.x = 17, s.y = 17), 
            e.hexagon()), 
  hrule = "diff_shapes.inv", 
  vrule = "diff_shapes.inv"
))


adult012bb = apply(Raven(
  st1 = circle(shd = "black"), 
  vrule = "size", 
  hrule = "size"
))

adult012 = com(adult012a, adult012bb)

#draw(adult012)


## ----out.width="80%"----------------------------------------------------------
dist_adult012 = responses(adult012)

sel012 = sel011
resp_adult012 = select.dist(dist_adult012, sel012)
p = split.mat(adult012)
resp_adult012$ic.flip = cof(rotation(p$e.hexagon, 3), 
                            p$circle)
p$circle$shade[[1]] =  "white"

resp_adult012$ic.neg = cof(p$circle, p$e.hexagon)

##draw.dist(resp_adult012, n.resp = 8, main = T)

## -----------------------------------------------------------------------------
adult018a = apply(Raven(
  st1 = cof(pentagon(), e.hexagon(), 
            rectangle(s.y = 10, s.x = 15)), 
  hrule = "diff_shapes.inv", 
  vrule = "diff_shapes.inv"
))


adult018b = apply(Raven(
  st1 = u.smallbow.tie.inv(), 
  hrule = c("lty"), 
  vrule = c("lty")
))

adult018c = apply(Raven(
  st1 = dot(shd = "black"),
  hrule = c("identity"),
  vrule = c("identity")
))

adult018 = com(adult018a, adult018b, adult018c)

draw(adult018)


## ----out.width="80%"----------------------------------------------------------
dist_adult018 = responses(adult018)

sel018 = c("correct","r.top", "r.diag", "wp.copy",  "wp.matrix", "d.union", "ic.neg", "ic.inc")

resp_adult018 = select.dist(dist_adult018, sel018)


p = split.mat(adult018)
p$dot$shade[[1]] = "white"

resp_adult018$ic.neg = replace(resp_adult018$ic.neg, 
                               5, 
                               p$dot)

#draw.dist(resp_adult018,n.resp = 8, main = T)

## -----------------------------------------------------------------------------

adult031<-apply(Raven(pie.4(),"AND"))
draw(adult031)


## ----out.width="80%"----------------------------------------------------------

dist_adult031 = responses(adult031)

p = split.mat(adult031)

dist_adult031$ic.flip = (cof(rotation(p[[1]], 2), 
                             rotation(p[[2]], 2)))
dist_adult031$ic.scale = (cof(size(p[[1]], 2), 
                              size(p[[2]], 2)))

dist_adult031$ic.neg = (cof(change.col(p[[1]]), 
                            change.col(p[[2]])))

sel31 = c("correct", "r.top", "r.left", "wp.copy", "wp.matrix", "d.union", "ic.flip", "ic.inc")

resp_adult031 = select.dist(dist_adult031, sel31)

#draw.dist(resp_adult031, n.resp = 8)


## -----------------------------------------------------------------------------

adult037a<-apply(Raven(cof(hline(pos.y = 3),hline(pos.y = -3),
                           pentagon(s.x=3,s.y=3),pentagon(s.x=2.5,s.y=2.5,shd="grey",lty = 0)),"XOR"))
adult037b<-apply(Raven(pentagon(s.x=17, s.y=17)))
adult037<-com(adult037b,adult037a)
draw(adult037)

## ----out.width="80%"----------------------------------------------------------
dist_adult037 = responses(adult037)
sel37 = c("correct", "r.diag", "r.left" , "wp.copy", "wp.matrix", "d.union", "ic.flip", "ic.neg")
resp_adult037 = select.dist(dist_adult037,
                            sel37)

#draw.dist(resp_adult037, n.resp =8)


