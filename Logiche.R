rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("IC_multiforme.R")

stamapa_su_file=FALSE
################################################################################
#####                      LOGICHE OR e XOR                                #####
################################################################################
bow.tie.inv <- function(pos.x = 0) {
  value <-cof(triangle(pos.x = pos.x+10, pos.y = pos.x, rot=pi/3, 
                       s.x = 10, s.y=10), 
              triangle(pos.x = pos.x-10, pos.y = pos.x, rot=-pi, 
                       s.x = 10, s.y=10))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "field"
  value
}

## a_logic3 
M1a<-logic_rules(Raven(square4()),"OR")

M1b<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"XOR")

if(stamapa_su_file==TRUE){svg("a_logic3.svg")}
draw(com(M1a,M1b),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}

## a1_logic3

M2a<-logic_rules(Raven(cof(hline(pos.y = 3,s.y=12),hline(pos.y = -3,s.y=12),
                           square(shd="line12"),pentagon(s.x=3,s.y=3,shd="white"))
),"XOR")
M2b <- logic_rules(Raven(cof(vline(pos.x = 28, s.x = 15),
                             vline(pos.x = -28, s.x = 15 ),
                             hline(pos.y = 15, s.x=30),
                             hline(pos.y = -15, s.x=30))),"OR")
if(stamapa_su_file==TRUE){svg("a1_logic3.svg")}
draw(com(M2a,M2b),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}

## b_logic3 
M3a<-logic_rules(Raven(cof(bow.tie(),
                          bow.tie.inv())),"XOR")

M3b<-logic_rules(Raven(cof(circle(s.x=4,s.y=4,shd="black")
                          ,cross.dice(),
                          hline(),vline())),"OR")
if(stamapa_su_file==TRUE){svg("b_logic3.svg")}
draw(com(M3a,M3b),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}

## b1_logic3 
M4a<-logic_rules(Raven(cof(square(s.x = 20,s.y = 20),
                           margin(square(s.x = 17,s.y = 17,shd = "line12"),3,"lty"),
                           dice(),
                           margin(square(s.x = 17,s.y = 17),3,"lty")
                           )),"XOR")

#M4b<-logic_rules(Raven(cof(horizontal_eight(),vertical_eight(),
#                           diagline(),diagline.inv())),"OR")

M4b<-logic_rules(Raven(cof(diagline(),horizontal_eight(),
                           vertical_eight(),
                           diagline.inv())),"OR")

if(stamapa_su_file==TRUE){svg("b1_logic3.svg")}                           
draw(com(M4a,M4b),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}

################################################################################
#####                      LOGICHE AND e XOR                               #####
################################################################################
square4bis <- function() {
  value <-cof(hline(pos.y=-11),vline(pos.x=11),
              hline(pos.y=11),vline(pos.x=-11))
  value$tag <- list("compose4")
  attr(value, "class") <- "field"
  value
}
###############################

## a_logic2 
M5a<-logic_rules(Raven(square4bis()),"OR")

M5b<-logic_rules(Raven(cof(pie.4())),"AND")
if(stamapa_su_file==TRUE){svg("a_logic2.svg")}
draw(com(M5a,M5b),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}

## a1_logic2 
M6a<-logic_rules(Raven(cof(pentagon(shd="line1.inv"), pentagon(shd="line2"),
                            pentagon(shd="line12.h"), hexagon(s.x=3,s.y=3)) ),"OR")

M6b <- logic_rules(Raven(cof(vline(pos.x = 28, s.x = 15),
                             vline(pos.x = -28, s.x = 15 ),
                             hline(pos.y = 15, s.x=30),
                             hline(pos.y = -15, s.x=30))),"AND")
if(stamapa_su_file==TRUE){svg("a1_logic2.svg")}
draw(com(M6a,M6b),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}
## b_logic2 
M7a<-logic_rules(Raven(cof(hexagon(shd="line1"),
                           hexagon(shd="line2"),dot(),
                           e.hexagon())),"OR")

M7b<-logic_rules(Raven(cof(bow.tie(),
                           bow.tie.inv())),"AND")
if(stamapa_su_file==TRUE){svg("b_logic2.svg")}
draw(com(M7a,M7b),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}

## b1_logic2 
M8a<-logic_rules(Raven(cof(pentagon(),
                           margin(pentagon(s.x=13,s.y =13),3,"lty"),
                           margin(pentagon(s.x=17,s.y =17),2,"lty"),
                           dice()
                           )),"OR")

M8b<-logic_rules(Raven(cof(margin(hline(pos.y=5,s.x=5),1,"lty"),
                           margin(hline(pos.y=-5,s.x=5),1,"lty"),
                           margin(vline(pos.x=5,s.x=5),1,"lty"),
                           margin(vline(pos.x=-5,s.x=5),1,"lty"))),"AND")
if(stamapa_su_file==TRUE){svg("b1_logic2.svg")}
draw(com(M8a,M8b),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}
################################################################################
#####                      LOGICHE AND e OR                                #####
################################################################################

## a_logic1 
M9a<- logic_rules(Raven(square4bis()),"OR")

M9b<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"AND")

if(stamapa_su_file==TRUE){svg("a_logic1.svg")}
draw(com(M9a,M9b),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}

## a1_logic1 
M10a<- logic_rules(Raven(cof(circle(pos.x = 11,pos.y = 11, s.x=3,s.y=3),
                         circle(pos.x = 0,pos.y = 0, s.x=3,s.y=3),
                         cof(diagline(pos.x = 11,pos.y = -11,s.x=3,s.y=3),
                             diagline.inv(pos.x = 11,pos.y = -11,s.x=3,s.y=3),
                             single = TRUE,name = "smallcross"),
                         cof(diagline(pos.x = -11,pos.y = -11,s.x=3,s.y=3),
                             diagline.inv(pos.x = -11,pos.y = -11,s.x=3,s.y=3),
                             single = TRUE,name = "smallcross")
                         )),"AND")

M10b<-logic_rules(Raven(cof(margin(hline(pos.y=5,s.x = 18),1,"lty"),
                            margin(hline(pos.y=-5,s.x = 18),1,"lty"),
                            margin(vline(pos.x=5,s.x = 18),1,"lty"),
                            margin(vline(pos.x=-5,s.x = 18),1,"lty"))),"OR")

if(stamapa_su_file==TRUE){svg("a1_logic1.svg")}
draw(com(M10b,M10a),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}

## b_logic1 
M11a<- logic_rules(Raven(cof(vline(pos.x = 28, s.x = 15),
                            vline(pos.x = -28, s.x = 15 ),
                            hline(pos.y = 15, s.x=28),
                            hline(pos.y = -15, s.x=28))),"OR")

M11b<-logic_rules(Raven(lily()),"AND")

if(stamapa_su_file==TRUE){svg("b_logic1.svg")}
draw(com(M11a,M11b),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}
## b1_logic1 

smallbow.tie.inv <- function(pos.x = 0,pos.y=0,shd=NA) {
  value <-cof(triangle(pos.x = pos.x+5, pos.y = pos.y, rot=pi/3, 
                       s.x = 5, s.y=5,shd = shd), 
              triangle(pos.x = pos.x-5, pos.y = pos.y, rot=-pi, 
                       s.x = 5, s.y=5,shd = shd))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "field"
  value
}

M12a<-logic_rules(Raven(cof(smallbow.tie.inv(pos.y=-7),
                            smallbow.tie.inv(pos.y=7)
                            )),"XOR")

M12b<-logic_rules(Raven(cof(margin(smallbow.tie.inv(pos.y=-7,shd ="grey"),1,"lty"),
                            margin(smallbow.tie.inv(pos.y=7,shd ="grey"),1,"lty"))
                        ),"AND")
if(stamapa_su_file==TRUE){svg("b1_logic1.svg")}
draw(com(M12b,M12a),hide=TRUE)
if(stamapa_su_file==TRUE){dev.off()}

##########
# BONUS  #
##########
smallbow.tie.inv <- function(pos.x = 0) {
  value <-cof(triangle(pos.x = pos.x+5, pos.y = pos.x-7, rot=pi/3, 
                       s.x = 5, s.y=5), 
              triangle(pos.x = pos.x-5, pos.y = pos.x-7, rot=-pi, 
                       s.x = 5, s.y=5))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "field"
  value
}

#draw(cof(pacman(pos.y = 5),fill(smallbow.tie.inv(),3,"defualt"),dot(pos.y = 9)))
###

M12c<-logic_rules(Raven(cof(pacman(pos.y = 3),fill(smallbow.tie.inv(),3,"defualt"),dot(pos.y = 7))),"XOR")

M12d<-logic_rules(Raven(cof(square(s.x=21,s.y=21),
                            square(s.x=22,s.y=22),
                            square(s.x=24,s.y=24),
                            square(s.x=21,s.y=21,shd="line12.h"))
                            ),"AND")
draw(com(M12c,M12d),hide=TRUE)
M12bis<-com(M12c,M12d)


################################################################################
#####                      TRANSFORMAZIONE MENTALE                         #####
################################################################################


M13a<-apply(Raven(cof(circle(s.x = 6,s.y = 6),square(s.x = 6,s.y = 6)),"trans.fill"))
M13b<-apply(Raven(cof(e.hexagon(),square(),pentagon()),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))

if(stamapa_su_file==TRUE){svg("a_1.svg")}
draw(com(M13a,M13b),hide = TRUE)
if(stamapa_su_file==TRUE){dev.off()}

###

M14a<-apply(Raven(cof(circle(s.x = 6,s.y = 6),square(s.x = 6,s.y = 6)),"trans.fill.line"))
M14b<-apply(Raven(cof(e.hexagon(),luck(s.x=15,s.y=17),triangle(s.x=17,s.y=17)),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))

if(stamapa_su_file==TRUE){svg("a1_1.svg")}
draw(com(M14a,M14b),hide = TRUE)
if(stamapa_su_file==TRUE){dev.off()}

### Bonus

M15a<-apply(Raven(cof(ellipse(s.x = 9,s.y = 6),circle(s.x = 6,s.y = 6)),c("trans.fill")))
M15b<-apply(Raven(cof(e.hexagon(),square(),pentagon()),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))

if(stamapa_su_file==TRUE){svg("b_1.svg")}
draw(com(M15a,M15b),hide = TRUE)
if(stamapa_su_file==TRUE){dev.off()}


### Bonus alternativa 

M15c<-apply(Raven(cof(pentagon(),circle()),c("trans.fill")))
M15d<-apply(Raven(cof(e.hexagon(s.x = 3, s.y = 3,shd = "white"),
                      square(s.x = 3, s.y = 3,shd = "white"),
                      pentagon(s.x = 3, s.y = 3,shd = "white")),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))

if(stamapa_su_file==TRUE){svg("b_1_bonus.svg")}
draw(com(M15c,M15d),hide = TRUE)
if(stamapa_su_file==TRUE){dev.off()}

###

M16a<-apply(Raven(cof(luck(s.x = 6,s.y = 6),circle(s.x = 3,s.y = 3)),"trans.fill.line"))
M16b<-apply(Raven(cof(pentagon(),ellipse(s.x=14,s.y=17),triangle(s.x=17,s.y=17)),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))

if(stamapa_su_file==TRUE){svg("b1_1.svg")}
draw(com(M16a,M16b),hide = TRUE)
if(stamapa_su_file==TRUE){dev.off()}

