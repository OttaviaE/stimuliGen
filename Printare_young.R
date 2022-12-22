rm(list = ls())
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

print.dist = function(resp.list, mat.name) {
  for (i in 1:length(resp.list)) {
    svg(paste0(getwd(), "/Test_young/Matrici/",
               mat.name, "_", names(resp.list)[i], ".svg")
    )
    draw(resp.list[[i]])
    dev.off()
  }
}


select.dist = function(dist.list, selection) {
  resp = list()
  for (i in 1:length(selection)) {
    resp[[i]] = dist.list[[selection[i]]]
    names(resp)[[i]] = selection[i]
  }
  return(resp)
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


print.mat = function(m, mat.name,mat.type=9) {
    if(mat.type==9)
    {
      squares <- paste0("Sq", 1:8)
      name<- c("11","12","13","21","22","23",
                "31","32")
    }else{
      squares <- paste0("Sq", c(1,2,4))
      name<- c("11","12","21")
    }
  
    for (i in 1:length(name)) {
    svg(paste0(getwd(), "/Test_young/Matrici/",
               mat.name, "_", name[i], ".svg")
    )
    draw(m[[squares[i]]])
    dev.off()
  }
}



############################################################
## YOUNG001
svg(paste0(getwd(), "/Test_young/Matrici/young001_11.svg"))

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
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young001_correct.svg"))
draw(container,xlim = 8)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12"),by=3.5,
      canvas = FALSE)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young001_ic.scale.svg"))
draw(container,xlim = 8)
draws(rectangle(s.x=7,s.y=5,shd="line.12"),
      canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),
      canvas = FALSE)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young001_diff1.svg"))

draw(container,xlim = 8)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young001_diff2.svg"))
draw(container,xlim = 8)
for (i in seq(-6, 7, by = 3)) {
  for (j in seq(-3, 3, by = 3)) {
    draw(dot(pos.x = i, pos.y = j, size.x = 0.5, size.y = 0.5), canvas = F)
  }
}

dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young001_ic.flip.svg"))
draw(container,xlim = 8)
for(i in seq(-7, 7, by = 1)) {
  draw(vline(pos.x = i, s.x=5, lwd = 3), canvas = F)
}

for(i in seq(-5, 5, by = 1)) {
  draw(hline(pos.y = i, s.x=7, lwd = 3), canvas = F)
}

dev.off()


############################################################
## YOUNG002
svg(paste0(getwd(), "/Test_young/Matrici/young002_11.svg"))
draw(rectangle(s.x=-50,s.y=50,shd=giallo,pos.x=+10,pos.y=-7))
draw(dot(pos.x = -30, pos.y = 15, size.x = 1, size.y = 1, shd = blu), canvas = F)

for (i in seq(-30, 30, by = 4)) {
  for (j in seq(15, -19, by = -4)) {
    draw(dot(pos.x = i, pos.y = j, size.x = 1, size.y = 1, shd = blu), canvas = F)
  }
}

draw(rectangle(s.x=7,s.y=5,shd="white",pos.x=+15,pos.y=-10),
     canvas = FALSE)

dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young002_correct.svg"))
container = rectangle(s.x=7,s.y=5,shd=giallo)

draw(container,xlim = 8)
clip(7,-7,5,-5)
for (i in seq(-21, 25, by = 4)) {
  for (j in seq(12.5, -15, by = -4)) {
    draw(dot(pos.x = i, pos.y = j, size.x = 1, size.y = 1, shd = blu), canvas = F)
  }
}
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young002_ic.neg.svg"))
draw(container,xlim = 8)
clip(7,-7,5,-5)
draw(rectangle(s.x=7,s.y=5,shd=blu,pos.x=0,pos.y=0),
     canvas = FALSE)
for (i in seq(-25, 25, by = 4)) {
  for (j in seq(15, -15, by = -4)) {
    draw(dot(pos.x = i, pos.y = j, size.x = 1, size.y = 1, shd = giallo), canvas = F)
  }
}
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young002_diff1.svg"))

draw(container,xlim = 8)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young002_diff2.svg"))
draw(container,xlim = 8)
clip(7,-7,5,-5)
for (i in seq(-21, 25, by = 4)) {
  for (j in seq(12.5, -15, by = -4)) {
    draw(square(pos.x = i, pos.y = j, s.x = 1, s.y = 1, shd = blu), canvas = F)
  }
}

dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young002_dist.svg"))
draw(container,xlim = 8)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12"),by=3.5,
      canvas = FALSE)

dev.off()


############################################################
## YOUNG003
svg(paste0(getwd(), "/Test_young/Matrici/young003_11.svg"))
draw(rectangle(s.x=-50,s.y=50,shd=blu,pos.x=+10,pos.y=-7))
draw(vline(pos.x = -20, s.x = 50, lwd = 3), canvas = F)
draw(vline(pos.x = 20, s.x = 50, lwd = 3, lty = 3), canvas = F)
draw(vline(pos.x = 17, s.x = 50, lwd = 3), canvas = F)
draw(hline(pos.y = 9, s.x = 50, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = -9, s.x = 50, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = -12, s.x = 50, lwd = 3, lty = 3), canvas = F)


draw(rectangle(s.x=7,s.y=5,shd="white",pos.x=+15,pos.y=-10),
     canvas = FALSE)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young003_correct.svg"))
container = rectangle(s.x=7,s.y=5,shd=blu)

draw(container,xlim = 8)

draw(rectangle(s.x=7,s.y=5,shd=NA,pos.x=0,pos.y=0),
     canvas = FALSE)

draw(vline(pos.x = 5, pos.y = -0, 
           s.x  = 5, lwd = 3, lty = 3), canvas = F)
draw(vline(pos.x = 2, pos.y = 0, 
           s.x = 5, lwd = 3), canvas = F)

draw(hline(pos.y = 0, 
           pos.x = 0,  s.x = 7, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = -3, pos.x = 0, s.x = 7, lwd = 3, lty = 3), canvas = F)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young003_ic.svg"))
draw(container,xlim = 8)

draw(rectangle(s.x=7,s.y=5,shd=NA,pos.x=0,pos.y=0),
     canvas = FALSE)

draw(vline(pos.x = 5, pos.y = -0, 
           s.x  = 5, lwd = 3, lty = 1), canvas = F)
draw(vline(pos.x = 2, pos.y = 0, 
           s.x = 5, lwd = 3), canvas = F)

draw(hline(pos.y = 0, 
           pos.x = 0,  s.x = 7, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = -3, pos.x = 0, s.x = 7, lwd = 3, lty = 1), canvas = F)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young003_ic1.svg"))
draw(container,xlim = 8)

draw(rectangle(s.x=7,s.y=5,shd=NA,pos.x=0,pos.y=0),
     canvas = FALSE)

draw(vline(pos.x = -5, pos.y = -0, 
           s.x  = 5, lwd = 3, lty = 3), canvas = F)
draw(vline(pos.x = -2, pos.y = 0, 
           s.x = 5, lwd = 3), canvas = F)

draw(hline(pos.y = 0, 
           pos.x = 0,  s.x = 7, lwd = 3, lty = 1), canvas = F)
draw(hline(pos.y = 3, pos.x = 0, s.x = 7, lwd = 3, lty = 3), canvas = F)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young003_diff.svg"))
draw(container,xlim = 8, main = "difference")
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young003_wp.svg"))
draw(container,xlim = 8, main = "wp")
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
      canvas = FALSE)

dev.off()

############################################################
## YOUNG004
svg(paste0(getwd(), "/Test_young/Matrici/young004_11.svg"))
draw(rectangle(s.x=-50,s.y=50,shd=giallo,pos.x=+10,pos.y=-7))
draw(vline(pos.x=-50, s.x = 30, lwd = 1), canvas = F)
for(i in seq(-30, 30, by = 2)) {
  
  draw(vline(pos.x = i, s.x=40, lwd = 1+ abs(i)/2), 
       canvas = F,  bg = "white")
}

draw(rectangle(s.x=7,s.y=5,shd="white",pos.x=+15,pos.y=-10),
     canvas = FALSE)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young004_correct.svg"))
container = rectangle(s.x=7,s.y=5,shd=giallo)

draw(container,xlim = 8)
clip(7,-7,5,-5)
#for(i in seq(14, 30, by = 1)) {
for(i in seq(6, 22, by = 2)) {
  
  draw(vline(pos.x = i-15, s.x=40, lwd = 1+ abs(i)/2), 
       canvas = F,  bg = "white")
}
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young004_wp1.svg"))
draw(container,xlim = 8)
clip(7,-7,5,-5)
for(i in seq(14, 30, by = 1)) {
  
  draw(vline(pos.x = i-22, s.x=40, lwd = 5), 
       canvas = F,  bg = "white")
}
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young004_diff.svg"))
draw(container,xlim = 8, main = "difference")
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young004_dist.svg"))
draw(container,xlim = 8)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young004_wp2.svg"))
draw(container,xlim = 8)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12"),by=3.5,
      canvas = FALSE)
dev.off()


############################################################
## YOUNG005
svg(paste0(getwd(), "/Test_young/Matrici/young005_11.svg"))
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
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young005_correct.svg"))
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
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young005_wp1.svg"))
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

dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young005_diff1.svg"))

draw(container,xlim = 8)
dev.off()

svg(paste0(getwd(), "/Test_young/Matrici/young005_wp2.svg"))
draw(container,xlim = 8)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
dev.off()


svg(paste0(getwd(), "/Test_young/Matrici/young005_diff2.svg"))

draw(container,xlim = 8)
draws(rectangle(s.x=7,s.y=5,shd="line.12.inv"),by=3.5,
      canvas = FALSE)
draws(rectangle(s.x=7,s.y=5,shd="line.12"),by=3.5,
      canvas = FALSE)
dev.off()


######################################################################
young006 = apply(Raven(
  st1 = cof( 
    rectangle(s.x = 25, s.y = 15, pos.y = -1, shd = blu), 
    triangle(pos.y = 0, s.x = 12, s.y = 12, shd = giallo)), 
  vrule = "reflection", 
  hrule = "identity"
))

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

young007 = com(young007b, young007a)

young008a = apply(Raven(
  st1 = dot(), 
  vrule = "fill"
))

young008b = apply(Raven(
  st1 = pacman(shd = rosso), 
  "reflection"
))

young008 = com(young008b, young008a)

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

young011 = apply(Raven(
  st1 = cof(e.hexagon(shd = giallo, s.x = 13, s.y = 13),
            triangle(shd = giallo, s.x = 15, s.y = 15),
            circle()),
  vrule = c( "reflection", "diff_shapes")
))

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


#################################################################################
#FASE DI STAMPA 1
###################################################################################
lista<-ls()
lista<-lista[grepl("young",lista) & (!grepl("a",lista)&!grepl("b",lista)&!grepl("dist",lista))  ]

for(i in 1:length(lista))
{
  print.mat(get(lista[i]),lista[i],4)
}


selection = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")

dist_young006 = responses(young006,mat.type = 4)

resp_young006 = select.dist(dist_young006, selection)

resp_young006[["d.union"]] =cof(resp_young006[["d.union"]], 
                                square())
resp_young006[["wp.matrix"]] =cof(resp_young006[["wp.matrix"]], 
                                  e.hexagon())
###############
dist_young007 = responses(young007,mat.type = 4)

resp_young007 = select.dist(dist_young007, selection)

resp_young007[["d.union"]] = cof(resp_young007[["d.union"]], 
                                 pie.4())
resp_young007[["wp.matrix"]] = cof(resp_young007[["wp.matrix"]], 
                                   dice())
p = split.mat(young007, mat.type  = 4)

resp_young007[["ic.flip"]] = cof(rotation(p[[1]], 2), 
                                 p[[2]]) 
#########
dist_young008 = responses(young008, mat.type = 4)
resp_young008 = select.dist(dist_young008, selection)

resp_young008[["wp.matrix"]] = cof(resp_young008[["wp.matrix"]], 
                                   rotation(pacman(shd = rosso), 3))
resp_young008[["d.union"]] = cof(resp_young008[["d.union"]], 
                                 pentagon())
p = split.mat(young008, mat.type = 4)

resp_young008[["ic.flip"]] = cof(rotation(p[[1]], 3), 
                                 p[[2]])


##########

dist_young009 = responses(young009, mat.type = 4)

resp_young009 = select.dist(dist_young009, selection)

resp_young009[["d.union"]] = cof(resp_young009[["d.union"]], 
                                 cross.dice())
resp_young009[["wp.matrix"]] = cof(resp_young009[["wp.matrix"]], 
                                   pacman())

p = split.mat(young009, mat.type  = 4)

resp_young009[["ic.flip"]] = cof( 
  p[[1]], rotation(p[[2]], 3)) 

draw.dist(resp_young009)

#############

dist_young010 = responses(young010, mat.type = 4)
resp_young010 = select.dist(dist_young010, selection)

resp_young010[["d.union"]] = cof(resp_young010[["d.union"]], 
                                 cross(), 
                                 pentagon())

resp_young010[["wp.matrix"]] = cof(resp_young010[["wp.matrix"]], 
                                   triangle())
p = split.mat(young010,mat.type  = 4)


resp_young010[["ic.flip"]] = cof(rotation(p[[1]], 3), 
                                 p[[2]])

#####
dist_young011 = responses(young011, mat.type = 4) 

resp_young011 = select.dist(dist_young011, selection)

resp_young011[["d.union"]] = cof(resp_young011[["d.union"]], 
                                 resp_young011[["wp.matrix"]], 
                                 lily())

resp_young011[["wp.matrix"]] = cof(
  resp_young011[["wp.matrix"]], 
  triangle())

######

dist_young012 = responses(young012, mat.type = 4)

resp_young012 = select.dist(dist_young012, selection)

resp_young012[["d.union"]] = cof(resp_young012[["d.union"]], 
                                 size(square(shd = rosso), 3))

resp_young012[["wp.matrix"]] =  cof(resp_young012[["r.top"]], 
                                    resp_young012[["wp.matrix"]])

p = split.mat(young012, mat.type = 4)



resp_young012[["ic.flip"]] =cof(reflection(p[[1]], 2), 
                                p[[2]])


##############################################################
#Stampa distratttori
lista<-ls()
lista<-lista[grepl("resp_young",lista) & (!grepl("a",lista)&!grepl("b",lista)&!grepl("dist",lista))  ]

for(i in 1:length(lista))
{
  print.dist(get(lista[i]),strsplit(lista,"_")[[i]][2])
}

########################################################################
#                    013
########################################################################
young013 = apply(
  Raven(
    st1 = pentagon(rot=pi/2), 
    vrule = "reflection"
  )
)



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


########################################################################
#                    016
########################################################################

young016 = apply(
  Raven(
    st1 = cof(square(),e.hexagon(),pacman()), 
    vrule = c("diff_shapes","fill")
  )
)


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


########################################################################
#                    018
########################################################################

young018 = apply(
  Raven(
    st1 = cof(pentagon(),triangle() ,pacman()), 
    vrule = c("diff_shapes","reflection")
  )
)



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




########################################################################
#                    020
########################################################################

young020 = obj_addition_rules(
  Raven(
    st1 = cof(cross(),square()) 
  ), rule="v.add"
)

young020frame =  apply(Raven(
  st1 = rectangle(s.x = 20, s.y = 15, shd = blu)
))

young020 = com(young020frame, young020)
draw(young020, n.cell = 4)



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



########################################################################
#                    023
########################################################################

young023 = obj_addition_rules(
  Raven(
    st1 = cof(cross(),square(rot = pi)) 
  ), rule="v.sott"
)

draw(young023, n.cell = 4)


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


#################################################################################
#FASE DI STAMPA 2
###################################################################################
lista<-ls()
lista<-lista[grepl("young",lista) & (!grepl("a",lista)&!grepl("b",lista)&!grepl("dist",lista))  ]

for(i in 1:length(lista))
{
  print.mat(get(lista[i]),lista[i],4)
}


##############################################

selection.neg = c("correct", "r.top", "d.union", "wp.matrix", "ic.neg")

dist_young013 = responses(young013,mat.type = 4)
resp_young013 = select.dist(dist_young013, selection.neg)


dist_young014 = responses(young014,mat.type = 4)

resp_young014 = select.dist(dist_young014, selection.neg)

dist_young015 = responses(young015,mat.type = 4)

resp_young015 = select.dist(dist_young015, selection.neg)

dist_young016 = responses(young016,mat.type = 4)

resp_young016 = select.dist(dist_young016, selection.neg)

resp_young016[["wp.matrix"]] = cof(resp_young016[["wp.matrix"]] , 
                                   resp_young016[["r.top"]] )

dist_young017 = responses(young017,mat.type = 4)
resp_young017 = select.dist(dist_young017, selection.neg)

dist_young018 = responses(young018,mat.type = 4)

resp_young018 = select.dist(dist_young018, selection.neg)

dist_young019 = responses(young019,mat.type = 4)
resp_young019 = select.dist(dist_young019, selection.neg)

selection.neg = c("correct", "r.top", "d.union", "wp.matrix", "ic.inc")
dist_young020 = responses(young020,mat.type = 4)
resp_young020 = select.dist(dist_young020, selection.neg)

resp_young020[["wp.matrix"]] = cof(resp_young020[["wp.matrix"]] , 
                                   dot() )
resp_young020[["ic.inc"]] = show(resp_young020[["ic.inc"]] ,index=1:2 )
resp_young020[["ic.inc"]] = hide(resp_young020[["ic.inc"]], 3 )


selection.neg = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")
dist_young021 = responses(young021,mat.type = 4)
resp_young021 = select.dist(dist_young021, selection.neg)

selection.neg = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")
dist_young022 = responses(young022,mat.type = 4)
resp_young022 = select.dist(dist_young022, selection.neg)

selection.neg = c("correct", "r.top", "d.union", "wp.matrix", "ic.inc")
dist_young023 = responses(young023,mat.type = 4)
resp_young023 = select.dist(dist_young023, selection.neg)

selection.neg = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")
dist_young024 = responses(young024,mat.type = 4)
resp_young024 = select.dist(dist_young024, selection.neg)

selection.neg = c("correct", "r.top", "d.union", "wp.matrix", "ic.flip")
dist_young025 = responses(young025,mat.type = 4)
resp_young025 = select.dist(dist_young025, selection.neg)
resp_young025[["ic.flip"]] = replace(resp_young025[["ic.flip"]] ,3 ,luck(rot = pi) )

########################################################
###################Stampa distrattori 

lista<-ls()
lista<-lista[grepl("resp_young",lista) & (!grepl("a",lista)&!grepl("b",lista)&!grepl("dist",lista))  ]

for(i in 1:length(lista))
{
  print.dist(get(lista[i]),strsplit(lista,"_")[[i]][2])
}

############################

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

young027a= apply(
  Raven(
    st1 = cof(pentagon(shd = rosso), triangle(shd = rosso), square(shd = rosso)), 
    hrule = c("diff_shapes"), 
    vrule = "identity"
  )
)

young027b = apply(Raven(
  st1 = cof(dot(shd = giallo, size.x = 7)), 
  hrule = "size"
))


young027 = com(young027a, young027b)

young028a = apply(Raven(
  st1 = square(shd = giallo)
))

young028b = apply(Raven(
  st1 = pacman(), 
  hrule = "fill"
))

young028 = com(young028a, young028b)

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

young031a = apply(Raven(
  st1=rectangle(shd = giallo, s.x = 20, s.y = 15)
))

young031b = apply(Raven(
  st1 = cof(u.smallbow.tie.inv(shd = blu), size(u.star(), 2), u.pie.4( 
    shd = rosso)), 
  vrule  = "diff_shapes"
))

young031 = com(young031a, young031b)

young032a = apply(Raven(
  st1=cof(square(shd = giallo), 
          square(shd = blu), 
          square(shd = rosso)), 
  hrule = "diff_shapes"
))

young032b = apply(Raven(
  st1 = cof(u.smallbow.tie.inv(shd = "black"), size(u.star(), 2), 
            size(pacman(shd = "black"), 2)), 
  vrule  = "diff_shapes"
))

young032 = com(young032a, young032b)

young033 = apply(
  Raven(
    st1 = cof(square(shd = "black"), u.star(), triangle(rot = pi/2, shd = "black")), 
    vrule = c("diff_shapes", "size")
  )
)

young034a = apply(Raven(
  st1 = cof(u.star(), pentagon(shd = "black"), e.hexagon(shd = "black")), 
  vrule = "diff_shapes"
))

young034b = apply(
  Raven(
    st1 = dot(shd = "white", size.x = 10), 
    hrule = c("size")
  )
)

young034 = com(young034a, young034b)

young035a = apply(Raven(
  st1 = square()
))

young035b = apply(Raven(
  st1 = cof(u.smallbow.tie.inv(), u.pie.2(), pacman()), 
  vrule = c("diff_shapes", "fill")
))

young035 = com(young035a, young035b)

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
    st1 = pacman()
  )
)


a_3c = apply(
  Raven(
    st1 = circle(s.x = 2, s.y = 2), 
    vrule = c("fill"), 
    hrule = "fill"
  )
)

young036 = com(a_3a, a_3b, a_3c)

#################################################################################
#FASE DI STAMPA 2
###################################################################################
lista<-ls()
lista<-lista[grepl("young",lista) & (!grepl("a",lista)&!grepl("b",lista)&!grepl("dist",lista))  ]

for(i in 1:length(lista))
{
  print.mat(get(lista[i]),lista[i],9)
}


#############


selection.neg = c("correct", "r.diag", "d.union","wp.copy","wp.matrix", "ic.scale","ic.flip","ic.inc")

dist_young026 = responses(young026,mat.type = 9)
resp_young026 = select.dist(dist_young026, selection.neg)


dist_young027 = responses(young027,mat.type = 9)
resp_young027 = select.dist(dist_young027, selection.neg)
resp_young027[["wp.copy"]] = young027$Sq1


selection.neg = c("correct", "r.diag", "d.union","wp.matrix", "ic.scale","ic.flip","ic.inc","ic.neg")

dist_young028 = responses(young028,mat.type = 9)
resp_young028 = select.dist(dist_young028, selection.neg)


selection.neg = c("correct", "r.diag", "d.union","wp.copy","wp.matrix", "ic.flip", "ic.scale","ic.neg")

dist_young029 = responses(young029,mat.type = 9)
resp_young029 = select.dist(dist_young029, selection.neg)

selection.neg = c("correct", "r.top", "d.union","wp.copy","wp.matrix", "ic.flip", "ic.scale","ic.inc")

dist_young030 = responses(young030,mat.type = 9)
resp_young030 = select.dist(dist_young030, selection.neg)

selection.neg = c("correct", "r.top", "d.union","wp.copy","wp.matrix", "ic.flip", "ic.scale","ic.inc")

dist_young031 = responses(young031,mat.type = 9)
resp_young031 = select.dist(dist_young031, selection.neg)
resp_young031[["ic.flip"]] = rotation(resp_young031[["ic.flip"]],2)
resp_young031[["wp.matrix"]] = cof(resp_young031[["wp.matrix"]],resp_young031[["wp.copy"]])
resp_young031[["ic.neg"]] = cof(resp_young031[["ic.neg"]],semi.circle(shd=rosso),semi.circle.inv(shd=rosso))


selection.neg = c("correct", "r.top", "d.union","wp.copy","wp.matrix", "ic.flip", "ic.scale","ic.inc")

dist_young032 = responses(young032,mat.type = 9)
resp_young032 = select.dist(dist_young032, selection.neg)

selection.neg = c("correct", "r.diag", "d.union","wp.copy","wp.matrix", "ic.flip", "ic.scale","ic.neg")

dist_young033 = responses(young033,mat.type = 9)
resp_young033 = select.dist(dist_young033, selection.neg)

selection.neg = c("correct", "r.top", "d.union","wp.copy","wp.matrix", "ic.flip", "ic.scale","ic.inc")

dist_young034 = responses(young034,mat.type = 9)
resp_young034 = select.dist(dist_young034, selection.neg)

selection.neg = c("correct", "r.top", "d.union","wp.copy","wp.matrix", "ic.flip", "ic.scale","ic.inc")

dist_young035 = responses(young035,mat.type = 9)
resp_young035 = select.dist(dist_young035, selection.neg)

selection.neg = c("correct", "r.top", "d.union","wp.copy","wp.matrix", "ic.flip", "ic.scale","ic.inc")

dist_young036 = responses(young036,mat.type = 9)
resp_young036 = select.dist(dist_young036, selection.neg)

###################Stampa distrattori 

lista<-ls()
lista<-lista[grepl("resp_young",lista) & (!grepl("a",lista)&!grepl("b",lista)&!grepl("dist",lista))  ]

for(i in 1:length(lista))
{
  print.dist(get(lista[i]),strsplit(lista,"_")[[i]][2])
}