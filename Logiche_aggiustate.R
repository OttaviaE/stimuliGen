source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("CodiceDistrattoriVero.R")


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


###
a1_logic2a<-logic_rules(Raven(cof(pentagon(shd="line1.inv"), pentagon(shd="line2"),
                           pentagon(shd="line12.h"), hexagon(s.x=3,s.y=3)) ),"OR")

a1_logic2b <- logic_rules(Raven(cof(vline(pos.x = 17, s.x = 15),
                             vline(pos.x = -17, s.x = 15 ),
                             hline(pos.y = 15, s.x=17),
                             hline(pos.y = -15, s.x=17))),"AND")

a1_logic2 = com(a1_logic2a, a1_logic2b)

draw(a1_logic2, hide = F)

a1_logic2_dist = responses(a1_logic2)


##
b_logic1a<- logic_rules(Raven(cof(vline(pos.x = 17, s.x = 15),
                             vline(pos.x = -17, s.x = 15 ),
                             hline(pos.y = 15, s.x=17),
                             hline(pos.y = -15, s.x=17))),"OR")

b_logic1b<-logic_rules(Raven(lily()),"AND")


b_logic1 = com(b_logic1a, b_logic1b)

draw(b_logic1, hide = F)

b_logic1_dist = responses(b_logic1)

### a_logic4

a_logic4<-logic_rules(Raven(cof(diagline(),hline(),vline(),diagline.inv())),"OR")
draw(a_logic4)

### a1_logic4

a1_logic4<-logic_rules(Raven(cof(semi.circle.inv(),
                           cof(vline(pos.x=11),vline(pos.x=-11),name="h.parallel",single = TRUE),
                           cof(hline(pos.y=11),hline(pos.y=-11),name="v.parallel",single = TRUE),
                           semi.circle())),"OR")
draw(a1_logic4)


### b_logic4

b_logic4<-logic_rules(Raven(cof(diagline(),diagline.inv(),square(),dot())),"OR")
draw(b_logic4)

### b1_logic4

b1_logic4<-logic_rules(Raven(cof(diagline(),pentagon(),diagline.inv(),dot(pos.y = 15))),"OR")
draw(b1_logic4)

