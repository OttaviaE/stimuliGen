## ----setup, include=FALSE-----------------------------------------------------

source("C:/Users/huawei/OneDrive/Documenti/GitHub/stimuliGen/Shapes_list-10-11-Ottavia.R")
source("C:/Users/huawei/OneDrive/Documenti/GitHub/stimuliGen/Class and Methods v02.R")
source("C:/Users/huawei/OneDrive/Documenti/GitHub/stimuliGen/Rules_27102022.R")
source("C:/Users/huawei/OneDrive/Documenti/GitHub/stimuliGen/CodiceDistrattoriVero.R")

print.dist = function(resp.list, mat.name) {
  for (i in 1:length(resp.list)) {
    svg(paste0(getwd(), "/StudioPreliminare/distrattori/",
               mat.name, "_", names(resp.list)[i], ".svg")
    )
    draw(resp.list[[i]])
    dev.off()
  }
}

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

# ic neg ----- 

ic.neg = function(m) {
  m.correct = correct(m)
  
  if (any(unlist(m.correct$shade == "black")) == T) {
    m.correct$shade[[1]] = rep("white", 
                               length(any(unlist(m.correct$shade == "black"))))
  } else if (any(unlist(m.correct$shade == "white")) == T) {
    m.correct$shade[[1]] = rep("black", 
                               length(any(unlist(m.correct$shade == "white"))))
  } else if(is.na(any(unlist(m.correct$shade))) == T) {
    m.correct$shade[[1]] = rep("black", 
                               length(is.na(any(unlist(m.correct$shade)))))
  } 
  ic.col = m.correct
  return(ic.col)
}


set.seed(999)
## -----------------------------------------------------------------------------
m_pratica1 <-apply(Raven(st1=cof(e.hexagon()),
                    hrule=c("fill"),vrule=c("identity")))
draw(m_pratica1, hide = T)

pratica_1_dist = responses(m_pratica1, choose.copy = 1)


draw.dist(pratica_1_dist, n.resp = 10, main = T)

# distrattori manuali m pratica 1 

resp_m_pratica1 = within(pratica_1_dist, 
                         rm(r.left, r.top, ic.scale, ic.inc))

resp_m_pratica1[["ic.neg"]] = ic.neg(m_pratica1)
resp_m_pratica1[["wp.copy"]] = rotation(resp_m_pratica1$wp.copy, 2)
resp_m_pratica1[["d.difference"]] = square()

names(resp_m_pratica1)

draw(m_pratica1, hide = T)
draw.dist(resp_m_pratica1, main = T, single.print = F)

print.dist(resp_m_pratica1, "pratica_1")



## -----------------------------------------------------------------------------

m_pratica2 = apply(Raven(st1=pie.4(),
                  hrule=c("identity"),
                  vrule=c("size")))
draw(m_pratica2, hide = TRUE)

pratica_2_dist = responses(m_pratica2)


draw.dist(pratica_2_dist, n.resp = 10, main = T)

# cambiare lo spessore del cerchio ------

resp_m_pratica2 = list(correct = correct(m_pratica2), 
                       dist1 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist2 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist3 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist4 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist5 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist6 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist7 = circle(s.x = 15, s.y = 15, lwd = 3)) 

draw(m_pratica2, hide = T)
draw.dist(resp_m_pratica2, n.resp = 8, main = T)

print.dist(resp_m_pratica2, "pratica_2")



## -----------------------------------------------------------------------------
m_pratica3 = apply(Raven(st1=u.pie.2(),
                     hrule=c("lty"),
                     vrule=c("identity")))
draw(m_pratica3, hide = T)

pratica_3_dist = responses(m_pratica3, choose.copy = 1, choose.start = 3)
draw.dist(pratica_3_dist, n.resp = 10, main = T)
resp_m_pratica3 = pratica_3_dist

resp_m_pratica3 = within(resp_m_pratica3, 
                         rm(r.left, r.top, ic.scale, ic.inc))

draw.dist(resp_m_pratica3, main = T)
correct_pratic3 = correct(m_pratica3)
correct_pratic3$shade[[1]] = rep("grey", 2)
resp_m_pratica3[["ic.neg"]] = (correct_pratic3)
resp_m_pratica3[["difference"]] = ellipse(shd = "black")

draw(m_pratica3, hide = T)
draw.dist(resp_m_pratica3, n.resp = 8, main = T)

print.dist(resp_m_pratica3, "pratica_3")

## ----out.width="80%"----------------------------------------------------------



## -----------------------------------------------------------------------------
m_pratica4a = apply(Raven(st1 = cof(dot(), 
                            s.lily(), 
                            square(s.x = 5, s.y = 5, 
                                   shd = "black", rot = pi/2)), 
                 hrule = "diff_shapes"))
        
m_pratica4b = apply(Raven(st1=pentagon(),
                 hrule=c("identity"),
                 vrule=c("identity")))

m_pratica4 = (com(m_pratica4a, m_pratica4b))

draw(m_pratica4, hide = T)

pratica_4_dist = responses(m_pratica4, choose.copy = 1)


## ----out.width="80%"----------------------------------------------------------
draw.dist(pratica_4_dist, n.resp = 10, main = T)


resp_m_pratica4 = within(pratica_4_dist, 
                         rm(r.left, r.top, ic.scale))

s_p4 = split.mat(m_pratica4)

s_p4[[2]]$shade[[1]] = "black"
s_p4[[1]]$shade[[1]] = "white"

resp_m_pratica4[["ic.neg"]] = cof(s_p4[[2]], s_p4[[1]])


draw(m_pratica4, hide = T)

draw.dist(resp_m_pratica4, main = T)
print.dist(resp_m_pratica4, "pratica_4")


## -----------------------------------------------------------------------------
## a_logic3 
a_logic3a <-logic_rules(Raven(square4()),"OR")

a_logic3b<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"XOR")

a_logic3 = com(a_logic3a, a_logic3b)

draw(a_logic3, hide = T)

a_logic3_dist = responses(a_logic3)


## ----out.width="80%"----------------------------------------------------------
draw.dist(a_logic3_dist, n.resp = 10, main = T)


dist_alogic3 = list(correct = correct(a_logic3),
                   r.top = repetition(a_logic3)$r.top,
                   r.left = repetition(a_logic3)$r.left,
                   d.union = d.union(a_logic3,
                                     choose.start = 3),
                   wp.copy = wp(a_logic3, choose.copy = 4)$wp.copy,
                   wp.matrix = wp(a_logic3, choose.matrix  = 4)$wp.matrix,
                   ic.inc = hide(a_logic3$Sq9, 1),
                   ic.flip = cof(rotation(a_logic3b$Sq9, 2), a_logic3a$Sq9))

draw.dist(dist_alogic3, main = T)

print.dist(dist_alogic3, "a_logic3")

## -----------------------------------------------------------------------------
## a_logic3 
a1_logic3a<-logic_rules(Raven(cof(hline(pos.y = 3,s.y=12),hline(pos.y = -3,s.y=12),
                                  square(shd="line12"),pentagon(s.x=3,s.y=3,shd="white"))
),"XOR")
a1_logic3b <- logic_rules(Raven(cof(vline(pos.x = 15, s.x = 15),
                                    vline(pos.x = -15, s.x = 15 ),
                                    hline(pos.y = 15, s.x=15),
                                    hline(pos.y = -15, s.x=15))),"OR")

a1_logic3 = com(a1_logic3a, a1_logic3b)

draw(a1_logic3, hide = F)


a1_logic3_dist = responses(a1_logic3)


## ----out.width="80%"----------------------------------------------------------
draw.dist(a1_logic3_dist, n.resp = 10, main = T)

resp_a1_logic3 = within(a1_logic3_dist, 
                        rm(r.diag, ic.scale))

draw.dist(resp_a1_logic3, n.resp = 10, main = T)

# non funziona dc
draw.dist(resp_a1_logic3, main = T, single.print = F)

print.dist(resp_a1_logic3, "a1_logic3")

## -----------------------------------------------------------------------------
a_logic2a<-logic_rules(Raven(square4bis()),"OR")

a_logic2b<-logic_rules(Raven(cof(pie.4())),"AND")

a_logic2 = com(a_logic2a, a_logic2b)

draw(a_logic2, hide = T)

a_logic2_dist = responses(a_logic2)


## ----out.width="80%"----------------------------------------------------------
draw.dist(a_logic2_dist, n.resp = 10, main = T)


resp_a_logic2 = within(a_logic2_dist, 
                       rm(r.diag, ic.scale, ic.flip))

al2_correct  = correct(a_logic2)
p = split.mat(a_logic2)

resp_a_logic2[["ic.flip"]] = cof(p[[1]], p[[2]],p[[3]], p[[4]], 
                                 rotation(p$slice, 2))
draw.dist(resp_a_logic2, 
          n.resp = 8, main = T)

print.dist(resp_a_logic2, "a_logic2")

## -----------------------------------------------------------------------------
a1_logic2a<-logic_rules(Raven(cof(pentagon(shd="line1.inv"), pentagon(shd="line2"),
                                  pentagon(shd="line12.h"), hexagon(s.x=3,s.y=3)) ),"OR")

a1_logic2b <- logic_rules(Raven(cof(vline(pos.x = 17, s.x = 15),
                                    vline(pos.x = -17, s.x = 15 ),
                                    hline(pos.y = 15, s.x=17),
                                    hline(pos.y = -15, s.x=17))),"AND")

a1_logic2 = com(a1_logic2a, a1_logic2b)

draw(a1_logic2, hide = T)

a1_logic2_dist = responses(a1_logic2, choose.matrix = 3)



## ----out.width="80%"----------------------------------------------------------
draw.dist(a1_logic2_dist, n.resp = 10, main = T)


resp_a1_logic2 = within(a1_logic2_dist, 
                       rm(r.diag, ic.scale, ic.flip, ic.inc))

draw.dist(resp_a1_logic2, n.resp = 10, main = T)

a1l2_p = split.mat(a1_logic2)

resp_a1_logic2[["ic.inc"]] = (cof(a1l2_p[[1]], a1l2_p[[2]], 
         a1l2_p[[3]], a1l2_p[[4]]))


resp_a1_logic2[["ic.flip"]] = (cof(a1l2_p[[1]], a1l2_p[[2]], 
                                  a1l2_p[[3]], rotation(a1l2_p[[4]], 3), a1l2_p[[5]]))

draw(resp_a1_logic2$ic.flip)
draw.dist(resp_a1_logic2,
          n.resp = 8, main = T)
# bisogna salvarli a mano dc

resp_a1_logic2$d.union = d.union(a1_logic2, choose.start = 3)

draw.dist(resp_a1_logic2, 
          single.print = F)

print.dist(resp_a1_logic2, "a1_logic2")
## -----------------------------------------------------------------------------
a_logic1a<- logic_rules(Raven(cof(circle(pos.x = 11,pos.y = 11, s.x=3,s.y=3),
                         circle(pos.x = 0,pos.y = 0, s.x=3,s.y=3),
                         cof(diagline(pos.x = 11,pos.y = -11,s.x=3,s.y=3),
                             diagline.inv(pos.x = 11,pos.y = -11,s.x=3,s.y=3),
                             single = TRUE,name = "smallcross"),
                         cof(diagline(pos.x = -11,pos.y = -11,s.x=3,s.y=3),
                             diagline.inv(pos.x = -11,pos.y = -11,s.x=3,s.y=3),
                             single = TRUE,name = "smallcross")
                         )),"AND")

a_logic1b<-logic_rules(Raven(cof(margin(hline(pos.y=5,s.x = 18),1,"lty"),
                            margin(hline(pos.y=-5,s.x = 18),1,"lty"),
                            margin(vline(pos.x=5,s.x = 18),1,"lty"),
                            margin(vline(pos.x=-5,s.x = 18),1,"lty"))),"OR")

a_logic1 = com(a_logic1a, a_logic1b)
draw(a_logic1, hide = T)
a_logic1_dist = responses(a_logic1)

## ----out.width="80%"----------------------------------------------------------
draw.dist(a_logic1_dist, n.resp = 10, main = T)


resp_a_logic1 = within(a_logic1_dist, 
                       rm(ic.scale, ic.flip, r.left))

al1_p = split.mat(a_logic1)

al1_p$circle$pos.x[[1]] = -11

resp_a_logic1[["ic.flip"]] = cof(al1_p[[1]], al1_p[[2]], al1_p[[3]], al1_p[[4]], al1_p[[5]])


draw.dist(resp_a_logic1, 
          n.resp = 8, 
          main = T)

resp_a_logic1$d.union = d.union(a_logic1, 
                                choose.start = 2)


print.dist(resp_a_logic1, "a_logic1")


## -----------------------------------------------------------------------------
a1_logic1a<- logic_rules(Raven(square4bis()),"OR")

a1_logic1b<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"AND")

a1_logic1 = com(a1_logic1a, a1_logic1b)

draw(a1_logic1, hide = T) 

a1_logic1_dist = responses(a1_logic1)


## ----out.width="80%"----------------------------------------------------------
draw.dist(a1_logic1_dist, n.resp = 10, main = T)

resp_a1_logic1 = within(a1_logic1_dist, 
                        rm(ic.scale, ic.flip, r.left))


a1l1_p = split.mat(a1_logic1)

resp_a1_logic1[["ic.flip"]] = cof(a1l1_p[[1]], 
                                  a1l1_p[[2]], 
                                  a1l1_p[[3]], 
                                  a1l1_p[[4]], 
                                  diagline())
draw.dist(resp_a1_logic1, 
          n.resp = 8, main = T)

print.dist(resp_a1_logic1, "a1_logic1")

## -----------------------------------------------------------------------------
b_logic3a<-logic_rules(Raven(cof(bow.tie(),
                          bow.tie.inv())),"XOR")

b_logic3b<-logic_rules(Raven(cof(circle(s.x=4,s.y=4,shd="black")
                          ,cross.dice(),
                          hline(),vline())),"OR")

b_logic3 = com(b_logic3a, b_logic3b)

draw(b_logic3, hide = T)

b_logic3_dist = responses(b_logic3)

## ----out.width="80%"----------------------------------------------------------
draw.dist(b_logic3_dist, n.resp = 10, main = T)

resp_b_logic3 = within(b_logic3_dist, 
                       rm(r.top, ic.scale, ic.flip))

bl3_p = split.mat(b_logic3)

resp_b_logic3[["ic.flip"]] = cof(bl3_p[[1]], bl3_p[[2]], bl3_p[[3]], 
                                 diagline(s.x = 15), diagline.inv(s.x=15))

draw.dist(resp_b_logic3, 
          n.resp = 8, main = T)
print.dist(resp_b_logic3, "b_logic3")
## -----------------------------------------------------------------------------

b1_logic3a<-logic_rules(Raven(cof(square(s.x = 20,s.y = 20),
                           margin(square(s.x = 17,s.y = 17,shd = "line12"),3,"lty"),
                           dice(),
                           margin(square(s.x = 17,s.y = 17),3,"lty")
                           )),"XOR")

b1_logic3b<-logic_rules(Raven(cof(diagline(),horizontal_eight(),
                           vertical_eight(),
                           diagline.inv())),"OR")

b1_logic3 = com(b1_logic3a, b1_logic3b)

draw(b1_logic3, hide = T)

b1_logic3_dist = responses(b1_logic3)


## ----out.width="80%"----------------------------------------------------------
draw.dist(b1_logic3_dist, n.resp = 10, main = T)

resp_b1_logic3 = within(b1_logic3_dist, 
                        rm(r.top, ic.scale, ic.flip))

b1l3_p = split.mat(b1_logic3)

resp_b1_logic3[["ic.flip"]] = cof(cross.dice(), 
                                  b1l3_p[[2]], 
                                  b1l3_p[[3]], 
                                  b1l3_p[[4]], 
                                  b1l3_p[[5]])

draw.dist(resp_b1_logic3, 
          n.resp = 8, main = T)

print.dist(resp_b1_logic3, "b1_logic3")

## -----------------------------------------------------------------------------
b_logic2a<-logic_rules(Raven(cof(hexagon(shd="line1"),
                           hexagon(shd="line2"),dot(),
                           e.hexagon())),"OR")

b_logic2b<-logic_rules(Raven(cof(bow.tie(),
                           bow.tie.inv())),"AND")
b_logic2 = com(b_logic2a, b_logic2b)

draw(b_logic2, hide =T)

b_logic2_dist = responses(b_logic2)

## ----out.width="80%"----------------------------------------------------------
draw.dist(b_logic2_dist, n.resp = 10, main = T)

resp_b_logic2 = within(b_logic2_dist, 
                       rm(ic.scale, ic.flip, r.diag))


bl2_p = split.mat(b_logic2)
bl2_p$triangle$rotation[[1]] = pi/2
bl2_p$triangle$pos.y[[1]] = -10

resp_b_logic2[["ic.flip"]] = cof(bl2_p[[1]], bl2_p[[2]], bl2_p[[3]], bl2_p$triangle)

draw.dist(resp_b_logic2, 
          n.resp = 8, main = T)

resp_b_logic2$d.union = d.union(b_logic2, 
                                choose.start = 4)

print.dist(resp_b_logic2, "b_logic2")

## -----------------------------------------------------------------------------

b1_logic2a<-logic_rules(Raven(cof(pentagon(),
                           margin(pentagon(s.x=13,s.y =13),3,"lty"),
                           margin(pentagon(s.x=17,s.y =17),2,"lty"),
                           dice()
                           )),"OR")

b1_logic2b<-logic_rules(Raven(cof(margin(hline(pos.y=5,s.x=5),1,"lty"),
                           margin(hline(pos.y=-5,s.x=5),1,"lty"),
                           margin(vline(pos.x=5,s.x=5),1,"lty"),
                           margin(vline(pos.x=-5,s.x=5),1,"lty"))),"AND")


b1_logic2 = com(b1_logic2a, b1_logic2b)

draw(b1_logic2, hide =T)

b1_logic2_dist = responses(b1_logic2)

## ----out.width="80%"----------------------------------------------------------
draw.dist(b1_logic2_dist, n.resp = 10, main = T)

resp_b1_logic2 = within(b1_logic2_dist, 
                        rm(r.diag, ic.scale, ic.flip))

b1l2_p = split.mat(b1_logic2)

resp_b1_logic2[["ic.flip"]] = cof((b1l2_p[[1]]), 
                                  (b1l2_p[[2]]), 
                                  (b1l2_p[[3]]), 
                                  (b1l2_p[[4]]),
                                  rotation(b1l2_p[[5]], 3))

draw.dist(resp_b1_logic2, 
          n.resp = 8, main = T)

resp_b1_logic2$d.union = d.union(b1_logic2, choose.start = 3)
print.dist(resp_b1_logic2, "b1_logic2")
## -----------------------------------------------------------------------------
b_logic1a<- logic_rules(Raven(cof(vline(pos.x = 17, s.x = 15),
                                  vline(pos.x = -17, s.x = 15 ),
                                  hline(pos.y = 15, s.x=17),
                                  hline(pos.y = -15, s.x=17))),"OR")

b_logic1b<-logic_rules(Raven(lily()),"AND")


b_logic1 = com(b_logic1a, b_logic1b)

draw(b_logic1, hide = F)

b_logic1_dist = responses(b_logic1)



## ----out.width="80%"----------------------------------------------------------
draw.dist(b_logic1_dist, n.resp = 10, main = T)


resp_b_logic1 = within(b_logic1_dist, 
                       rm(r.diag, ic.scale, ic.flip))


bl1_p = split.mat(b_logic1)

resp_b_logic1[["ic.flip"]] = cof(bl1_p[[1]], bl1_p[[2]], bl1_p[[3]], bl1_p[[4]], 
                    s.vertical())


draw.dist(resp_b_logic1, n.resp = 8,single.print = F)
print.dist(resp_b_logic1, "b_logic1")

## -----------------------------------------------------------------------------



b1_logic1a<-logic_rules(Raven(cof(smallbow.tie.inv(pos.y=-7),
                            smallbow.tie.inv(pos.y=7)
                            )),"XOR")

b1_logic1b<-logic_rules(Raven(cof(margin(smallbow.tie.inv(pos.y=-7,shd ="grey"),1,"lty"),
                            margin(smallbow.tie.inv(pos.y=7,shd ="grey"),1,"lty"))
                        ),"AND")


b1_logic1 = com(b1_logic1a, b1_logic1b)

draw(b1_logic1, hide =T)

b1_logic1_dist = responses(b1_logic1)

## ----out.width="80%"----------------------------------------------------------
draw.dist(b1_logic1_dist, n.resp = 10, main = T)


resp_b1_logic1 = within(b1_logic1_dist, 
                        rm(ic.scale, ic.flip, r.diag))


draw.dist(resp_b1_logic1, 
          main =T)

c.b1l1 = correct(b1_logic1)

c.b1l1$shade

for(i in 1:length(unlist(c.b1l1$shade))) {
  if (is.na(c.b1l1$shade[[i]]) == T) {
    c.b1l1$shade[[i]] = "grey"
  } else {
    c.b1l1$shade[[i]] = "white"
  }
}

resp_b1_logic1[["ic.neg"]] = c.b1l1 
draw.dist(resp_b1_logic1, main =T)

print.dist(resp_b1_logic1, "b1_logic1")


### a_logic4

a_logic4<-logic_rules(Raven(cof(diagline(),hline(),vline(),diagline.inv())),"OR")
draw(a_logic4, hide = T )

a_logic4_dist = responses(a_logic4)

## ----out.width="80%"----------------------------------------------------------
draw.dist(a_logic4_dist, n.resp = 10, main = T)


resp_a_logic4 = within(a_logic4_dist, 
                        rm(ic.scale, ic.flip, r.top))

a4_p = split.mat(a_logic4)

for (i in 1:length(a4_p)) {
  a4_p[[i]]$size.x[[1]] = a4_p[[i]]$size.x[[1]]- 7
  a4_p[[i]]$size.y[[1]]  = a4_p[[i]]$size.y[[1]] -7
}


resp_a_logic4$ic.scale = (cof(a4_p[[1]], a4_p[[2]], 
         a4_p[[3]], a4_p[[4]]))

resp_a_logic4$d.union = d.union(a_logic4, choose.start = 4)

draw.dist(resp_a_logic4, 
          main =T)

print.dist(resp_a_logic4, "a_logic4")

### a1_logic4

a1_logic4<-logic_rules(Raven(cof(semi.circle.inv(),
                                 cof(vline(pos.x=11),vline(pos.x=-11),name="h.parallel",single = TRUE),
                                 cof(hline(pos.y=11),hline(pos.y=-11),name="v.parallel",single = TRUE),
                                 semi.circle())),"OR")
draw(a1_logic4)

a1_logic4_dist = responses(a1_logic4)

## ----out.width="80%"----------------------------------------------------------
draw.dist(a1_logic4_dist, n.resp = 10, main = T)

resp_a1_logic4 = within(a1_logic4_dist, 
                       rm(ic.scale, ic.flip, r.top))

a14_p = split.mat(a1_logic4)

resp_a1_logic4$ic.scale = (cof(a14_p[[2]], 
         a14_p[[3]], 
         (size(pie.2(),2))))

draw(correct(a1_logic4))

resp_a1_logic4$d.union = cof(a1_logic4$Sq9, 
                              
                             s.vertical(), 
                             s.horizontal())

draw.dist(resp_a1_logic4, 
          main =T)

print.dist(resp_a1_logic4, "a1_logic4")

### b_logic4

b_logic4<-logic_rules(Raven(cof(diagline(),diagline.inv(),square(),dot())),"OR")
draw(b_logic4, hide = T)


b_logic4_dist = responses(b_logic4)

## ----out.width="80%"----------------------------------------------------------
draw.dist(b_logic4_dist, n.resp = 10, main = T)


resp_b_logic4 = within(b_logic4_dist, 
                       rm(ic.scale, ic.flip, r.top))

b4_p = split.mat(b_logic4)

for (i in 1:length(grep("diagline", names(b4_p)))) {
  b4_p[[i]]$size.x[[1]] = b4_p[[i]]$size.x[[1]]- 7
  b4_p[[i]]$size.y[[1]]  = b4_p[[i]]$size.y[[1]] -7
}
b4_p$dot$size.x[[1]] = 1
b4_p$dot$size.y[[1]] = 1


resp_b_logic4$ic.scale = (cof(b4_p[[1]], b4_p[[2]], 
                              b4_p[[3]], b4_p[[4]]))


draw.dist(resp_b_logic4, 
          main =T)

print.dist(resp_b_logic4, "b_logic4")


### b1_logic4

b1_logic4<-logic_rules(Raven(cof(diagline(),pentagon(),diagline.inv(),dot(pos.y = 15))),"OR")
draw(b1_logic4)


b1_logic4_dist = responses(b1_logic4)

## ----out.width="80%"----------------------------------------------------------
draw.dist(b1_logic4_dist, n.resp = 10, main = T)


resp_b1_logic4 = within(b1_logic4_dist, 
                       rm(ic.scale, ic.flip, r.top))

b14_p = split.mat(b1_logic4)

resp_b1_logic4$ic.inc = (cof(b14_p[[1]], b14_p[[2]], 
                         b14_p[[3]]))

b14_p[[1]]$size.x[[1]] = b14_p[[1]]$size.x[[1]]- 7
b14_p[[1]]$size.y[[1]]  = b14_p[[1]]$size.y[[1]] -7
b14_p[[3]]$size.x[[1]] = b14_p[[3]]$size.x[[1]]- 7
b14_p[[3]]$diagline$size.y[[1]]  = b14_p[[3]]$size.y[[1]] -7

draw(b14_p$diagline.inv)

resp_b1_logic4$ic.scale = (cof(b14_p[[1]], b14_p[[2]], 
                               b14_p[[3]], b14_p[[4]]))



draw.dist(resp_b1_logic4, 
          main =T)

print.dist(resp_b1_logic4, "b1_logic4")

## -----------------------------------------------------------------------------
a2_mat = apply(Raven(st1 = cof(luck(shd = "white"), pacman(shd = "white"), 
                           pentagon(shd = "white")), 
                 hrule = c("diff_shapes", "size", "rotation"), 
                 vrule = c("diff_shapes")))

a2_frame = apply(Raven(
  st1= circle(shd = "grey", s.y =17, s.x = 17), 
  "identity", "identity"
))

a2 = com(a2_frame, a2_mat)
draw(a2, hide = F)

a2_dist = responses(a2, which.element = "pentagon")


## ----out.width="80%"----------------------------------------------------------
draw.dist(a2_dist, n.resp = 10, main = T)


resp_a2 = within(a2_dist, 
                 rm(r.diag, ic.scale, ic.flip))

a2_p = split.mat(a2)


for (i in 1:length(a2_p)) {
  if (a2_p[[i]]$shade[[1]] == "grey") {
    a2_p[[i]]$shade[[1]] = "white"
  } else if(a2_p[[i]]$shade[[1]] == "white") {
    a2_p[[i]]$shade[[1]] = "grey"
  }
}


resp_a2[["ic.neg"]] = cof(a2_p[[1]], a2_p[[2]])

draw.dist(resp_a2, 
          n.resp = 8, main = T)

resp_a2$d.union = cof(resp_a2$ic.inc, star())

print.dist(resp_a2, "a_visuo2")

## -----------------------------------------------------------------------------
a1_2mat = apply(Raven(st1 = cof(slice(shd = "white", 
                                   s.x = 13), 
                             luck(shd="white", s.x = 10, s.y = 13), 
                             e.hexagon(shd="white", 
                                       s.x = 13, s.y = 13)), 
                   hrule = c("diff_shapes", "size", "rotation"), 
                   vrule = c("diff_shapes")))
a1_2frame = apply(Raven(
  st1 = square(s.x = 21, s.y=21, shd = "grey"), 
  vrule = "identity"
))
a1_2 = com(a1_2frame, a1_2mat)
draw(a1_2, hide = F)

## ----out.width="80%"----------------------------------------------------------
a1_2_dist = responses(a1_2, which.element = "e.hexagon")

draw.dist(a1_2_dist, n.resp = 10, main = T)

resp_a1_2 = within(a1_2_dist, 
                   rm(r.top, wp.matrix, ic.flip, ic.scale))


draw.dist(resp_a1_2, 
          n.resp = 8, main = T)

a1_2_p = split.mat(a1_2)




resp_a1_2[["wp.matrix"]] = cof(a1_2_p[[1]], 
                               a1_2_p[[2]], 
                               slice(shd="white")) 

draw.dist(resp_a1_2, 
          n.resp = 8, main = T)

a1_2_p = split.mat(a1_2)


for (i in 1:length(a1_2_p)) {
  if (a1_2_p[[i]]$shade[[1]] == "grey") {
    a1_2_p[[i]]$shade[[1]] = "white"
  } else if(a1_2_p[[i]]$shade[[1]] == "white") {
    a1_2_p[[i]]$shade[[1]] = "grey"
  }
}


resp_a1_2[["ic.neg"]] = cof(a1_2_p[[1]], a1_2_p[[2]])
draw(resp_a1_2$ic.neg)

draw.dist(resp_a1_2, 
          n.resp = 8, main = T)

print.dist(resp_a1_2, "a1_visuo2")

## -----------------------------------------------------------------------------
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
draw(a_3, hide = T)

## ----out.width="80%"----------------------------------------------------------
a3_dist = responses(a_3, 
                    which.element = "pacman", 
                    choose.start = 3)

draw.dist(a3_dist, n.resp = 10, main = T)

resp_a3 = within(a3_dist, 
                rm(r.left, ic.scale))

resp_a3$d.union = cof(resp_a3$ic.inc, 
                      pie.4())

draw.dist(resp_a3, n.resp = 8, main = T)

print.dist(resp_a3, "a_visuo3")

## -----------------------------------------------------------------------------
a1_3a = apply(Raven(
  st1 = square(s.x = 20, s.y = 20), 
  vrule = "identity", 
  hrule= "identity"
))

a1_3b = apply(Raven(
  st1 = cof(slice(s.x = sqrt(square()$size.x[[1]]^2 /2)), luck(s.x = 10, 
                          s.y = 12), 
            triangle(s.x = 10, s.y = 10)), 
  vrule = "diff_shapes", 
  hrule= c("diff_shapes", "rotation")
))

a1_3c = apply(
  Raven(
    st1 = circle(s.x = 2, s.y = 2), 
    vrule = c("fill"), 
    hrule = "fill"
  )
)
a1_3 = com(a1_3a, a1_3b, a1_3c)
draw(a1_3, hide = F)

## ----out.width="80%"----------------------------------------------------------
a1_3_dist = responses(a1_3, which.element = "triangle", 
                      choose.copy = 7, 
                      choose.start = 1)

draw.dist(a1_3_dist, n.resp = 10, main = T)

resp_a1_3 = within(a1_3_dist, 
                   rm(r.diag, ic.scale, ic.flip))

a13_p = split.mat(a1_3)
a13_p[[2]]$rotation = pi/2

resp_a1_3[["ic.flip"]] =   cof(a13_p[[1]], 
                               a13_p[[2]], 
                               a13_p[[3]])

resp_a1_3$d.union = cof(a1_3$Sq1, 
                        size(pie.4(), 3))

draw.dist(resp_a1_3, main = T)




print.dist(resp_a1_3, "a1_visuo3")


## -----------------------------------------------------------------------------
b2_mat = apply(Raven(st1 = cof(semi.circle(shd = "white"), 
                               pacman(shd = "white"), 
                           ellipse(shd = "white")), 
                 hrule = c("diff_shapes",  "size", "rotation"), 
                 vrule = c("diff_shapes")))
b2_frame = apply(Raven(
  st1 = pentagon(shd = "grey")
))
b2 = (com(b2_frame, b2_mat))
draw(b2, hide = T)



## ----out.width="80%"----------------------------------------------------------
b2_dist = responses(b2, which.element = "ellipse")

resp_b2 = within(b2_dist, 
                 rm(r.left, ic.scale, ic.flip))

draw.dist(b2_dist, n.resp = 10, main = T)


b2_p = split.mat(b2)


for (i in 1:length(b2_p)) {
  if (b2_p[[i]]$shade[[1]] == "grey") {
    b2_p[[i]]$shade[[1]] = "white"
  } else if(b2_p[[i]]$shade[[1]] == "white") {
    b2_p[[i]]$shade[[1]] = "grey"
  }
}

resp_b2[["ic.neg"]] = cof(b2_p[[1]], b2_p[[2]])


resp_b2[["wp.matrix"]] = cof(hide(b2_dist$wp.matrix,c(2,3)), 
                             hide(b2$Sq4, 1), 
                             hide(b2_dist$wp.matrix,c(1,2)))
resp_b2$d.union = cof(resp_b2$ic.inc, 
                      rotation(pacman(shd = "white"),3), 
                      vertical.eight())

draw.dist(resp_b2, n.resp = 8, main = T)

print.dist(resp_b2, "b_visuo2")


## -----------------------------------------------------------------------------
b1_2mat = apply(Raven(st1 = cof(pentagon(shd = "white", 
                                        s.x = 12, s.y = 12), 
                               triangle(rot = pi/2, 
                                        s.x = 12, s.y = 12, shd = "white"), 
                             ellipse(shd = "white", 
                                     
                                     s.x = 9, s.y = 12)), 
                   hrule = c("diff_shapes",  "size", "rotation"), 
                   vrule = c("diff_shapes")))
b1_2frame = apply(Raven(
  st1 = square(shd = "grey", s.x = 20, s.y = 20)
))
b1_2 = com(b1_2frame, b1_2mat)

draw(b1_2, hide = T)


## ----out.width="80%"----------------------------------------------------------
b1_2_dist = responses(b1_2, which.element = "ellipse")

draw.dist(b1_2_dist, n.resp = 10, main = T)

resp_b1_2 = within(b1_2_dist, 
                   rm(r.top, ic.flip, ic.scale))

draw.dist(resp_b1_2, main = T)

# metti ic neg al posto di scale ------
b12_p = split.mat(b1_2)


for (i in 1:length(b12_p)) {
  if (b12_p[[i]]$shade[[1]] == "grey") {
    b12_p[[i]]$shade[[1]] = "white"
  } else if(b12_p[[i]]$shade[[1]] == "white") {
    b12_p[[i]]$shade[[1]] = "grey"
  }
}

resp_b1_2[["ic.neg"]] = cof(b12_p[[1]], b12_p[[2]])

draw.dist(resp_b1_2, main = T)


print.dist(resp_b1_2, "b1_visuo2")

## -----------------------------------------------------------------------------
b_3a = apply(Raven(
  st1 = cof(pentagon(), 
            e.hexagon(), 
            ellipse(s.x = 13, s.y = 17)), 
  vrule = "diff_shapes", 
  hrule= c("diff_shapes", "rotation")
))



b_3b = apply(Raven(
  st1 = papillon,
  hrule = "multifill", 
  vrule = "multifill"
))

b_3 = com(b_3a, b_3b)

draw(b_3, hide = F)


## ----out.width="80%"----------------------------------------------------------
b_3_dist = responses(b_3, 
                     choose.start = 3, 
                     choose.matrix = 4)


resp_b_3 = within(b_3_dist, 
                  rm(r.diag, 
                     ic.scale, ic.inc))

draw.dist(b_3_dist, n.resp = 10, main = T)


b3_p = split.mat(b_3)

for (i in 2:length(b3_p)) {
  if (any(b3_p[[i]]$shade[[1]] == "grey", na.rm = T) == T) {
    b3_p[[i]]$shade[[1]] = rep("white", 
                               length((b3_p[[i]]$shade[[1]])))
  } else if(any(b3_p[[i]]$shade[[1]] == "white") == T) {
    b3_p[[i]]$shade[[1]] = rep("grey", 
                               length((b3_p[[i]]$shade[[1]])))
  } 
}

b3_p[[2]]$shade[[1]] = rep("white", 3)


resp_b_3[["ic.neg"]] = cof(b3_p[[1]], b3_p[[2]], b3_p[[3]])


resp_b_3$d.union = cof(b_3$Sq1, smallbow.tie.inv(shd = "grey"))

draw.dist(resp_b_3, n.resp = 8, main = T)


print.dist(resp_b_3, "b_visuo3")
## -----------------------------------------------------------------------------
b1_3a = apply(Raven(
  st1 = semi.circle(), 
  hrule =c("rotation", "fill"),
  vrule = c("fill")
))


b1_3b = apply(Raven(
  st1 = cof(circle(s.x = 15, s.y = 15), 
            square(s.x = 17, s.y = 17), 
            e.hexagon()), 
  vrule = "diff_shapes", 
  hrule= c("diff_shapes")
))


b1_3 = com(b1_3a, b1_3b)

draw(b1_3, hide = T)


## ----out.width="80%"----------------------------------------------------------
b_1_3_dist = responses(b1_3)

draw.dist(b_1_3_dist, n.resp = 10, main = T)

resp_b_1_3 = within(b_1_3_dist, 
                    rm(r.left, ic.scale, ic.inc))

b13_p = split.mat(b1_3)


draw.dist(resp_b_1_3, main = T)

b13_p$semi.circle$shade[[1]] = rep("white", 3)


resp_b_1_3[["ic.neg"]] = cof(b13_p[[1]], b13_p[[2]])

draw.dist(resp_b_1_3, main = T)

print.dist(resp_b_1_3, "b1_visuo3")

## -----------------------------------------------------------------------------
a_trasf1a <-apply(Raven(cof(circle(s.x = 6,s.y = 6),square(s.x = 6,s.y = 6)),"trans.fill"))
a_trasf1b<-apply(Raven(cof(e.hexagon(),square(),pentagon()),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))

a_1 = com(a_trasf1a, a_trasf1b)

draw(a_1, hide = T)

a_1_dist = responses(a_1, which.element = "pentagon")

draw.dist(a_1_dist, n.resp = 10, main = T)

resp_a1 = within(a_1_dist, 
                 rm(r.top, ic.scale, ic.flip, ic.inc))

a1_p = split.mat(a_1)


resp_a1[["ic.flip"]] = cof(a1_p[[1]], 
                           rotation(a1_p[[2]], 5))

draw.dist(resp_a1, main = T)

inv_a1 = a1_p[[1]]
inv_a1$shade[[1]] = "white"

resp_a1[["ic.neg"]] = cof(inv_a1, a1_p[[2]])

print.dist(resp_a1, "a_visuo1")



a_trasf2a<-apply(Raven(cof(circle(s.x = 6,s.y = 6),square(s.x = 6,s.y = 6)),"trans.fill.line"))
a_trasf2b<-apply(Raven(cof(e.hexagon(),luck(s.x=15,s.y=17),triangle(s.x=17,s.y=17)),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))

a1_1 = com(a_trasf2a, a_trasf2b)

## -----------------------------------------------------------------------------
draw(a1_1, hide = T)

## ----out.width="80%"----------------------------------------------------------


a_trasf1_dist = responses(a1_1, which.element = "triangle")

draw.dist(a_trasf1_dist, n.resp = 10, main = T)


resp_a1_1 = within(a_trasf1_dist,
                   rm(r.diag, ic.scale, ic.inc))

a1_1_p = split.mat(a1_1)

inv_a1_1 = a1_1_p[[1]]
inv_a1_1$shade[[1]] = "white"

resp_a1_1[["ic.neg"]] = cof(inv_a1_1, a1_1_p[[2]])

draw.dist(resp_a1_1, n.resp = 8, main = T)

resp_a1_1$d.union = cof(a1_1$Sq4, size(star(), 2), 
                        dot(shd = "white"))

print.dist(resp_a1_1, "a1_visuo1")


b_trasf3a<-apply(Raven(cof(ellipse(s.x = 9,s.y = 6),circle(s.x = 6,s.y = 6)),c("trans.fill")))
b_trasf3b<-apply(Raven(cof(e.hexagon(),square(),pentagon()),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))


## -----------------------------------------------------------------------------
b_trasf1 = com(b_trasf3a, b_trasf3b)
draw(b_trasf1, hide = T)



## ----out.width="80%"----------------------------------------------------------

b_1_dist = responses(b_trasf1, which.element = "pentagon")

draw.dist(b_1_dist, n.resp = 10, main = T)


resp_b1 = within(b_1_dist, 
                 rm(r.left, ic.scale, ic.flip, ic.inc))

b1_p = split.mat(b_trasf1)

resp_b1[["ic.flip"]] = cof(b1_p[[1]], 
                           rotation(b1_p[[2]], 5))


b1_inv = b1_p[[1]]
b1_inv$shade[[1]] = "white"


resp_b1[["ic.neg"]] = cof(b1_p[[2]], b1_inv)

draw.dist(resp_b1, main = T)

resp_b1$d.union = cof(b_trasf1$Sq1, 
                      ellipse(shd = "grey"), 
                      smallbow.tie.inv(shd = "white"))

print.dist(resp_b1, "b_visuo1")


b_trasf4a<-apply(Raven(cof(luck(s.x = 6,s.y = 6),circle(s.x = 3,s.y = 3)),"trans.fill.line"))
b_trasf4b<-apply(Raven(cof(pentagon(),ellipse(s.x=14,s.y=17),triangle(s.x=17,s.y=17)),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))

b1_trasf1 = com(b_trasf4a, b_trasf4b)

draw(b1_trasf1, hide = T)


## ----out.width="80%"----------------------------------------------------------

b1_trasf1_dist = responses(b1_trasf1)

draw.dist(b1_trasf1_dist, n.resp = 10, main = T)

resp_b1_1 = within(b1_trasf1_dist, 
                   rm(r.top, ic.scale, ic.inc))

b1_1_p = split.mat(b1_trasf1)

b11_inv = b1_1_p[[1]]

b11_inv$shade[[1]] = "black"

resp_b1_1[["ic.neg"]] = cof(b11_inv, b1_1_p[[2]])

resp_b1_1$d.union = cof(hide(resp_b1_1$d.union, 5), 
                        e.hexagon())

draw.dist(resp_b1_1, main = T)

print.dist(resp_b1_1, "b1_visuo1")
