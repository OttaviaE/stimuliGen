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
M7<-numeric_progression(M7,"v.increasing",n=1)
draw(M7)
##NON SI PUO FARE COM MI VIENE DA PIANGERE


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
