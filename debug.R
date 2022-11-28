current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cat("\014")
rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")

lilth<-lily()
s.lilth<-s.lily()
for(i in 1:length(lilth$shape)) {
  lilth$size.x[[i]] <-lilth$size.x[[i]]/2
  lilth$size.y[[i]] <-lilth$size.y[[i]]/2
  lilth$pos.y[[i]] <-lilth$pos.y[[i]]/2
  lilth$pos.x[[i]] <-lilth$pos.x[[i]]/2
  
}

s.lilth$size.x[[1]] <-s.lilth$size.x[[1]]/2
s.lilth$size.y[[1]] <-s.lilth$size.y[[1]]/2
s.lilth$pos.y[[1]] <-s.lilth$pos.y[[1]]/2
s.lilth$pos.x[[1]] <-s.lilth$pos.x[[1]]/2

for(i in 1:length(lilth$shape)) {
  lilth$size.x[[i]] <-lilth$size.x[[i]]
  lilth$size.y[[i]] <-lilth$size.y[[i]]
  lilth$pos.y[[i]] <-lilth$pos.y[[i]]
  lilth$pos.x[[i]] <-lilth$pos.x[[i]]
  
}

s.lilth$size.x[[1]] <-s.lilth$size.x[[1]]
s.lilth$size.y[[1]] <-s.lilth$size.y[[1]]
s.lilth$pos.y[[1]] <-s.lilth$pos.y[[1]]
s.lilth$pos.x[[1]] <-s.lilth$pos.x[[1]]


xcros<- cross()
xcros$lwd[[1]]<-xcros$lwd[[1]]+4
xcros$size.x[[1]]<-xcros$size.x[[1]]-4
xcros$size.y[[1]]<-xcros$size.y[[1]]-4


# nuovi problemi yay
m1 = apply(Raven(st1 = cof(pacman(),  square(s.x = 5, s.y = 5, 
                                             shd = "black", rot = pi/2),
                           s.lily()
), 
hrule = "diff_shapes", 
vrule = c("diff_shapes","size")))
draw(m1, n.cell = 4, hide = F)      


###ESEMPIO 1
M1<-apply(Raven(cof(s.lilth,square(s.x=3,s.y=3,rot = pi/2),xcros),
                "diff_shapes"))
draw(M1)

M2<-apply(Raven(cof(square(),pentagon(),square(rot=0)),
                "diff_shapes","diff_shapes"))
draw(M2)

draw(com(M1,M2))

###ESEMPIO 2

M1<-apply(Raven(cof(s.lily(),square(s.x=3,s.y=3,rot = pi/2),cross()),
                "diff_shapes","diff_shapes"))
draw(M1)

M2<-apply(Raven(cof(square()),vrule = "lty"))   
draw(M2)

M <-com(M2,M1)
draw(M)

# risposta corretta
Correct<-M$Sq9
# Matrice
risposte<-list()

M$Sq9<-hide(M$Sq9)
risposte[[1]] = (M[[4]])
risposte[[2]] = cof(rotation(Correct,2))

# d union
d.union = hide(Correct, c(1, 2))
risposte[[3]] = cof(d.union, Correct, square(s.x=3,s.y=3, shd="black"), X())

risposte[[4]] = M$Sq8
risposte[[5]] = hide(Correct, c(4))
risposte[[6]] = size(Correct,2)
risposte[[7]] = Correct
risposte[[8]] = cof(hide(M$Sq8,1),dot())

#jpeg("Matrice01.jpg", width = 1200, height = 1200)
draw(M)
#dev.off()
for(i in 1:length(risposte))
{
#  jpeg(paste0("Risposta",i,"_matrice01.jpg"), width = 500, height = 500)
  draw(risposte[[i]])
#  dev.off()
}
par(mfrow=c(2,4))
##distrattori assieme  

for(i in 1:length(risposte))
{
  draw(risposte[[i]])
}


###ESEMPIO 4

M1<-logic_rules(Raven(square4()),"OR")
draw(M1)

M2<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"XOR")
draw(M2)

draw(com(M1,M2))

M<-com(M1,M2)

# risposta corretta
Correct<-M$Sq9
# Matrice
risposte<-list()

M$Sq9<-hide(M$Sq9)
risposte[[1]] = (M[[4]])
risposte[[2]] = cof(rotation(Correct,2))

# d union
d.union = hide(Correct, c(1, 2))
risposte[[3]] = cof(d.union, Correct, square(s.x=3,s.y=3, shd="black"), X())

risposte[[4]] = M$Sq8
risposte[[5]] = hide(Correct, 2)
risposte[[6]] = size(Correct,2)
risposte[[7]] = Correct
risposte[[8]] = cof(hide(M$Sq8,1),dot())

#jpeg("Matrice02.jpg", width = 1200, height = 1200)
draw(M)
#dev.off()
for(i in 1:length(risposte))
{
 # jpeg(paste0("Risposta",i,"_matrice02.jpg"), width = 500, height = 500)
  draw(risposte[[i]])
 #dev.off()
}

par(mfrow=c(2,4))
##distrattori assieme  

for(i in 1:length(risposte))
{
  draw(risposte[[i]])
}


### ESEMPIO 5

M1<-apply(Raven(lily(),"rotation","rotation"))
draw(M1)


### ESEMPIO 6

M1<-apply(Raven(bow.tie(),"multifill","multifill"))
draw(M1)

### ESEMPIO 7
M1<-apply(Raven(cof(pentagon(), 
                    square(), 
                    circle()),"parfill","diff_shapes"))
draw(M1)

### ESEMPIO 8 item4, item10, item4, item10

M1<-apply(Raven(cof(circle(s.x = 6,s.y = 6),square(s.x = 6,s.y = 6)),"trans.fill"))
M2<-apply(Raven(cof(e.hexagon(),square(),pentagon()),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))
draw(M1)
draw(M2)
draw(com(M1,M2))

###

M1<-apply(Raven(cof(circle(s.x = 6,s.y = 6),square(s.x = 6,s.y = 6)),"trans.fill.line"))
M2<-apply(Raven(cof(e.hexagon(),square(),pentagon()),c("diff_shapes.inv","rotation"),c("diff_shapes.inv","rotation")))
draw(M1)
draw(M2)
draw(com(M1,M2)) 

## ESEMPIO 9 #Gallifrey fall
#M1<-apply(Raven(create_dice(square()),"hquant.inv","vquant.inv"))
#draw(M1)

## ESEMPIO 10 #NO MORE
#M5<-apply(Raven(create_dice(ellipse()),
#                "hquant.x2","vquant.x2"))
#draw(M5)

M1<-apply(Raven(cof(ellipse(),square(),triangle()),"diff_shapes"))
m1<-numeric_progression(M1,"TL-LR-increasing")
draw(m1)

m2<-numeric_progression(M1,"LL-TR")
draw(m2)

m3<-numeric_progression(M1,"v.increasing")
draw(m3)

m4<-numeric_progression(M1,"h.increasing")
draw(m4)
