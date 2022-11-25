# matrici 3 x 3 ------
rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")

shapes<-shapes_list("Shapes_list-10-11-Ottavia.R")
shapes_riducible<-shapes[shapes$small&shapes$num_shapes==1,]
shapes_compose<-shapes[shapes$num_shapes>=2,]
shapes_single<-shapes[shapes$num_shapes==1,]
##Progressione Quantitaiva	Crescente in H e decrescente in V

M1<-apply(Raven(create_dice(square(shd="black")),"hquant","vquant.inv"))
draw(M1)

M2<-apply(Raven(create_dice(luck()),"hquant","vquant.inv"))
draw(M2)


##Progressione Quantitaiva	Crescente in H e in V

M3<-apply(Raven(create_dice(hexagon()),"hquant","vquant"))
draw(M3)

M4<-apply(Raven(create_dice(ellipse(shd="grey")),"hquant.x2","vquant.x2"))
draw(M4)

##AND	H
M5<-apply(Raven(pie.4(),"AND"))
draw(M5)

M6<-apply(Raven(cof(vline(),square(),hline(),s_vertical()),"AND"))
draw(M6)


##AND	H o V

M7<-logic_rules(Raven(cof(diagline(),hline(),vline(),diagline.inv())),"AND")
draw(M7)

M8<-logic_rules(Raven(cof(semi.circle.inv(),
                cof(vline(pos.x=11),vline(pos.x=-11),name="h.parallel",single = TRUE),
                cof(hline(pos.y=11),hline(pos.y=-11),name="v.parallel",single = TRUE),
                semi.circle())),"AND")
draw(M8)


##	OR H
M10<-apply(Raven(cof(square(),lily()),"OR"))
draw(M10)

M11<-apply(Raven(cof(vline(),square(),hline(),s_vertical()),"OR"))
draw(M11)
