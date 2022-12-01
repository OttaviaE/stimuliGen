rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("IC_multiforme.R")
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
M1<-logic_rules(Raven(square4()),"OR")

M2<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"XOR")

draw(com(M1,M2),hide=TRUE)

## a1_logic3

M3<-logic_rules(Raven(cof(hline(pos.y = 3,s.y=12),hline(pos.y = -3,s.y=12),
                           square(shd="line12"),pentagon(s.x=3,s.y=3,shd="white"))
),"XOR")
M4 <- logic_rules(Raven(cof(vline(pos.x = 28, s.x = 15),
                             vline(pos.x = -28, s.x = 15 ),
                             hline(pos.y = 15, s.x=30),
                             hline(pos.y = -15, s.x=30))),"OR")
draw(com(M3,M4),hide=TRUE)

## b_logic3 
M1<-logic_rules(Raven(cof(bow.tie(),
                          bow.tie.inv())),"XOR")
draw(M2)

M2<-logic_rules(Raven(cof(circle(s.x=4,s.y=4,shd="black")
                          ,cross.dice(),
                          hline(),vline())),"OR")

draw(com(M1,M2))
     
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
M5<-logic_rules(Raven(square4bis()),"OR")

M6<-logic_rules(Raven(cof(pie.4())),"AND")
draw(com(M5,M6),hide=TRUE)


## a1_logic2 
M7<-logic_rules(Raven(cof(pentagon(shd="line1.inv"), pentagon(shd="line2"),
                            pentagon(shd="line12.h"), hexagon(s.x=3,s.y=3)) ),"OR")

M8 <- logic_rules(Raven(cof(vline(pos.x = 28, s.x = 15),
                             vline(pos.x = -28, s.x = 15 ),
                             hline(pos.y = 15, s.x=30),
                             hline(pos.y = -15, s.x=30))),"AND")
draw(com(M7,M8),hide=TRUE)

