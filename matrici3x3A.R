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
M1<-apply(Raven(square(shd="black")))
M1<-numeric_progression(M1,"LL-TR")
draw(M1)

M2<-apply(Raven(luck()))
M2<-numeric_progression(M2,"LL-TR")
draw(M2)

##Progressione Quantitaiva	Crescente in H e in V

M3<-apply(Raven(hexagon()))
M3<-numeric_progression(M3,"TL-LR-increasing")
draw(M3)

##Forma, Progressione Quantitaiva	V su entrambe le regole 
M4 <- apply(Raven(cof(pentagon(),triangle(),e.hexagon()),"identity","diff_shapes"))
M4<-numeric_progression(M4,"v.increasing")
draw(M4)

##Forma, Progressione Quantitaiva	V per una regola e H per l'altra
M5 <- apply(Raven(cof(pentagon(),luck(),e.hexagon()),"diff_shapes"))
M5<-numeric_progression(M5,"h.increasing",n=2)
draw(M5)

M6 <- apply(Raven(cof(square(),luck(),triangle()),"identity","diff_shapes"))
M6<-numeric_progression(M6,"v.increasing",n=1.7)
draw(M6)

##Progressione Quantitaiva, Orientamento	V su entrambe le regole
M7 <- apply(Raven(luck(),
                  "identity","rotation"))
M7<-numeric_progression(M7,"v.increasing",n=2.5)
draw(M7)

#M8 <- apply(Raven(cof(vline(pos.x = 30, s.x = 15),
#                      vline(pos.x = -30, s.x = 15 ),
#                      hline(pos.y = 15, s.x=30),
#                      hline(pos.y = -15, s.x=30)),"rotation","rotation"))
#M8 <- apply(Raven(e.hexagon(s.x=25,s.y = 20),"rotation"))

#Progressione Quantitaiva, Orientamento	V per una regola e H per l'altra

M8<- apply(Raven(e.hexagon()))
M8<-numeric_progression(M8,"v.increasing",n=1)
M9<- apply(Raven(vline(pos.y=5, pos.x=-1,s.x=5),"rotation"))
draw(com(M9,M8))

M10<- apply(Raven(pentagon(),vrule = "rotation"))
M10<-numeric_progression(M10,"h.increasing",n=1.5)
draw(com(M10))

##AND	H
M11<-apply(Raven(pie.4(),"AND"))
draw(M11)

M12<-apply(Raven(cof(vline(),square(),hline(),s_vertical()),"AND"))
draw(M12)


##AND	H o V

M13<-logic_rules(Raven(cof(diagline(),hline(),vline(),diagline.inv())),"AND")
draw(M13)

M13<-logic_rules(Raven(cof(semi.circle.inv(),
                cof(vline(pos.x=11),vline(pos.x=-11),name="h.parallel",single = TRUE),
                cof(hline(pos.y=11),hline(pos.y=-11),name="v.parallel",single = TRUE),
                semi.circle())),"AND")
draw(M13)


##	OR H
M14<-apply(Raven(cof(square(),lily()),"OR"))
draw(M14)

M15<-apply(Raven(cof(vline(),square(),hline(),s_vertical()),"OR"))
draw(M15)

##OR	H o V
M13<-logic_rules(Raven(cof(diagline(),diagline.inv(),square(),dot())),"OR")
draw(M13)

M14<-logic_rules(Raven(cof(diagline(),pentagon(),diagline.inv(),dot(pos.y = 15))),"OR")
draw(M14)

## XOR	H 
M15<-apply(Raven(cof(hline(pos.y = 3),hline(pos.y = -3),
                     pentagon(s.x=3,s.y=3)),"XOR"))
M16<-apply(Raven(pentagon(s.x=17, s.y=17)))
draw(com(M15,M16))

## XOR	H o V
M17<-logic_rules(Raven(cof(cross(),square(),
                           margin(square(s.x=18,s.y=18),1,"lty")
                           ,cross.dice())),"XOR")
draw(M17)

## AND e XOR	la prima a parte esterne e la seconda a parti interne
# V il primo H il secondo

M18<-apply(Raven(cof(hline(pos.y = 3,s.y=12),hline(pos.y = -3,s.y=12),
                     square(shd="line12"),pentagon(s.x=3,s.y=3,shd="white")),
                 "XOR"))
M19 <- apply(Raven(cof(vline(pos.x = 30, s.x = 15),
                     vline(pos.x = -30, s.x = 15 ),
                     hline(pos.y = 15, s.x=30),
                     hline(pos.y = -15, s.x=30)),"identity","AND"))
draw(com(M19,M18))


## OR e XOR	la prima a parte esterne e la seconda a parti interne
# V il primo H il secondo

M18<-apply(Raven(cof(hline(pos.y = 3,s.x=7),hline(pos.y = -3,s.x=7),
                     margin(circle(s.x=7, s.y = 7),5,"lwd"),
                     hexagon(s.x=3,s.y=3)),"XOR"))
M19 <- apply(Raven(square4(),"identity","OR"))
draw(com(M19,M18))

