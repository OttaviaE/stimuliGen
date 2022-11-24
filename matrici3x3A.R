# matrici 3 x 3 ------
rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")

shapes<-shapes_list("Shapes_list-10-11-Ottavia.R")
shapes_riducible<-shapes[shapes$small&shapes$num_shapes==1,]
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

##Forma, Progressione Quantitaiva	V su entrambe le regole
