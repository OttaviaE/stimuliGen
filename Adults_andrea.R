rm(list = ls())
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")

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

adult013a<-apply(Raven(cof(empty(),cross(),X()),"diff_shapes"))
adult013b<-apply(Raven(cof(pentagon(s.x=15, s.y=15),e.hexagon(s.x=14, s.y=14),
                           square(s.x=15, s.y=15)),vrule = "diff_shapes"))
adult013<-com(adult013a,adult013b)
draw(adult013)

adult014a<-apply(Raven(cof(dice(),empty(),cross()),"diff_shapes","diff_shapes"))
adult014b<-apply(Raven(cof(size(u.star(),3),size(s.lily(),3),
                           dot()),vrule = "diff_shapes.inv","diff_shapes.inv"))
adult014<-com(adult014a,adult014b)
draw(adult014)


adult015a<-apply(Raven(cof(cross.dice(),cross(),X()),"diff_shapes"))
adult015b<-apply(Raven(cof(square(shd="grey",s.x=9,s.y=9),e.hexagon(shd="grey",s.x=9,s.y=9),
                           luck(shd="grey",s.x=7,s.y=9)),vrule = "diff_shapes"))
adult015<-com(adult015b,adult015a)
draw(adult015)

adult016a<-apply(Raven(cof(circle(),square(),pentagon()),"diff_shapes","diff_shapes"))
adult016b<-apply(Raven(cof(size(circle(),3),size(circle(),2),
                           square(s.x=3,s.y=3)),vrule = "diff_shapes.inv","diff_shapes.inv"))
adult016<-com(adult016a,adult016b)
draw(adult016)



adult031<-apply(Raven(pie.4(),"AND"))
draw(adult031)


adult032<-logic_rules(Raven(cof(
                                semi.circle.inv(lty=0,shd = "line12.inv"),
                                semi.circle(lty=0,shd = "line12"),
                                semi.circle.inv(lwd=5),semi.circle(lwd=5)
                           )),"AND")
draw(adult032)

adult033<-logic_rules(Raven(cof(cof(diagline(lty = 3),diagline.inv(lty = 3),name="oggetto",single=TRUE),
                                hline(lty = 3),vline(lty = 3),circle(s.x = 11,s.y = 11))),"AND")
draw(adult033)


adult034<-apply(Raven(cof(square(),lily()),"OR"))
draw(adult034)


adult035<-logic_rules(Raven(cof(diagline(),diagline.inv(),square(),dot())),"OR")
draw(adult035)

adult036<-logic_rules(Raven(cof(pentagon(),pentagon(shd="grey",lty=0),
                                cof(diagline(),diagline.inv(),name = "croce",single = TRUE),
                                dot(pos.y = 15))),"OR")
draw(adult036)

adult037a<-apply(Raven(cof(hline(pos.y = 3),hline(pos.y = -3),
                     pentagon(s.x=3,s.y=3),pentagon(s.x=2.5,s.y=2.5,shd="grey",lty = 0)),"XOR"))
adult037b<-apply(Raven(pentagon(s.x=17, s.y=17)))
adult037<-com(adult037a,adult037b)
draw(adult037)

adult038<-logic_rules(Raven(cof(cross(),square(),
                           margin(square(s.x=18,s.y=18),1,"lty")
                           ,cross.dice())),"XOR")
draw(adult038)

adult039<-logic_rules(Raven(cof(cof(vline(pos.x = -15,s.x=15 ),vline(pos.x = 15,s.x=15 ),name="oggetto",single =TRUE) 
                                ,pentagon(shd="grey"),
                                margin(pacman(shd = "white"),1,"lty")
                                ,dot(pos.x = 5))),"XOR")
draw(adult039)

adult040<-logic_rules(Raven(cof(dice(),dot(),cross.dice(),X())),"XOR")
draw(adult040)

adult041a<-logic_rules(Raven(cof(hline(pos.y = 3,s.y=12),hline(pos.y = -3,s.y=12),
                     square(shd="line12"),pentagon(s.x=3,s.y=3,shd="white"))),
                 "XOR")
adult041b <- logic_rules(Raven(cof(vline(pos.x = 15, s.x = 15),
                       vline(pos.x = -15, s.x = 15 ),
                       hline(pos.y = 15, s.x=15),
                       hline(pos.y = -15, s.x=15))),"AND")
adult041<-com(adult041a,adult041b)
draw(adult041)

adult042a<- apply(Raven(cof(circle(pos.x = 11,pos.y = 11, s.x=3,s.y=3),
                             circle(pos.x = 0,pos.y = 0, s.x=3,s.y=3),
                             cof(diagline(pos.x = 11,pos.y = -11,s.x=3,s.y=3),
                                 diagline.inv(pos.x = 11,pos.y = -11,s.x=3,s.y=3),
                                 single = TRUE,name = "smallcross"),
                             cof(diagline(pos.x = -11,pos.y = -11,s.x=3,s.y=3),
                                 diagline.inv(pos.x = -11,pos.y = -11,s.x=3,s.y=3),
                                 single = TRUE,name = "smallcross")
),"XOR"))

adult042b<- apply(Raven(cof(margin(hline(pos.y=5,s.x = 18),1,"lty"),
                            margin(hline(pos.y=-5,s.x = 18),1,"lty"),
                            margin(vline(pos.x=5,s.x = 18),1,"lty"),
                            margin(vline(pos.x=-5,s.x = 18),1,"lty")),vrule="AND"))

adult042<-com(adult042b,adult042a)
draw(adult042)

adult043a<-logic_rules(Raven(cof(bow.tie(),
                           bow.tie.inv())),"XOR")

adult043b<-logic_rules(Raven(cof(circle(s.x=4,s.y=4,shd="black")
                           ,cross.dice(),
                           hline(),vline())),"OR")
adult043<-com(adult043a,adult043b)
draw(adult043)

adult044a<-apply(Raven(cof(hline(pos.y = 3,s.x=7),hline(pos.y = -3,s.x=7),
                     margin(circle(s.x=7.5, s.y = 7.5),5,"lwd"),
                     hexagon(s.x=3,s.y=3)),"XOR"))
adult044b<- apply(Raven(cof(hline(pos.y = -11),hline(pos.y = 11),
                       vline(pos.x = -11),vline(pos.x = +11)),"identity","OR"))
adult044<-com(adult044a,adult044b)
draw(adult044)


adult045a<-apply(Raven(cof(luck(s.x = 6,s.y = 6),circle(s.x = 3,s.y = 3)),"trans.fill.line"))
adult045b<-apply(Raven(cof(pentagon(),ellipse(s.x=14,s.y=17),triangle(s.x=17,s.y=17)),vrule = "diff_shapes.inv"))

adult045 = com(adult045a, adult045b)

draw(adult045)


adult046a<-apply(Raven(cof(pentagon(s.x = 6,s.y = 6),ellipse(s.x = 4,s.y = 3)),"trans.fill"))
adult046b<-apply(Raven(cof(e.hexagon(), luck(s.x=14,s.y=14),square(s.x=17,s.y=17)),"diff_shapes","diff_shapes"))

adult046 = com(adult046a, adult046b)

draw(adult046)

adult047a<-apply(Raven(cof(circle(s.x = 12,s.y = 12),pentagon(s.x = 12,s.y = 12)),"trans.fill.line"))
adult047b<-apply(Raven(cof(pentagon(s.x = 4, s.y = 4,shd = "white"), luck(s.x=4,s.y=5,shd = "white"),
                           triangle(s.x=4,s.y=4,shd = "white")),
                       c("diff_shapes","rotation.inv6"),c("diff_shapes","rotation.inv3")))

adult047 = com(adult047a, adult047b)

draw(adult047)

adult048a<-apply(Raven(cof(circle(s.x = 14,s.y = 14),pentagon(s.x = 14,s.y = 14)),
                       "trans.fill"))

adult048b<-apply(Raven(cof(triangle(shd="white"),e.hexagon(shd="white"),
                           square(shd="white")),
                       c("diff_shapes"),c("diff_shapes")))
adult048b<-numeric_progression(adult048b,"LL-TR")

adult048 = com(adult048a, adult048b)

draw(adult048)
