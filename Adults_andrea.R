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


adult031<-apply(Raven(pie.4(),"AND"))
draw(adult031)


adult032<-logic_rules(Raven(cof(
                                semi.circle.inv(lty=0,shd = "grey"),
                                semi.circle(lty=0,shd = "black"),
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
