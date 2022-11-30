## a_logic3 
M1<-logic_rules(Raven(square4()),"OR")
draw(M1)

M2<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"XOR")
draw(M2)

draw(com(M1,M2))

###############################
square4bis <- function() {
  value <-cof(hline(pos.y=-11),vline(pos.x=11),
              hline(pos.y=11),vline(pos.x=-11))
  value$tag <- list("compose4")
  attr(value, "class") <- "field"
  value
}
###############################

## a_logic2 
M1<-logic_rules(Raven(square4bis()),"OR")
draw(M1)

M13<-logic_rules(Raven(cof(pie.4())),"AND")
draw(com(M1,M13))

## a1_logic3

M18<-logic_rules(Raven(cof(hline(pos.y = 3,s.y=12),hline(pos.y = -3,s.y=12),
                     square(shd="line12"),pentagon(s.x=3,s.y=3,shd="white"))
                 ),"XOR")
M19 <- logic_rules(Raven(cof(vline(pos.x = 28, s.x = 15),
                       vline(pos.x = -28, s.x = 15 ),
                       hline(pos.y = 15, s.x=30),
                       hline(pos.y = -15, s.x=30))),"OR")
draw(com(M19,M18))

## a1_logic2 
M1<-logic_rules(Raven(cof(pentagon(shd="line1.inv"), pentagon(shd="line2"),
                            pentagon(shd="line12.h"), hexagon(s.x=3,s.y=3)) ),"OR")
draw(M1)

M19 <- logic_rules(Raven(cof(vline(pos.x = 28, s.x = 15),
                             vline(pos.x = -28, s.x = 15 ),
                             hline(pos.y = 15, s.x=30),
                             hline(pos.y = -15, s.x=30))),"AND")
draw(com(M1,M19))

