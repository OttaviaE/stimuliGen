source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods.R")
source("Class and Methods extension.R")
source("Rules_27102022.R")


# provo a formalizzare i distrattori ------
shapes = shapes_list("Shapes_list-10-11-Ottavia.R")
# gli archetti si possono togliere 

shapes = shapes[-grep("arc", shapes$name), ]
shapes$var = numeric(nrow(shapes))

m = apply(Raven(cof(e.hexagon(),square(),
                    pentagon()),
                hrule = c("diff_shapes", "rotation"),
                vrule = c("rotation", "rotation")))
draw(m)

M1<-apply(Raven(cof(s.lily(),square(s.x=3,s.y=3,rot = pi/2),
                    cross()),
                "diff_shapes","diff_shapes"))
draw(M1)

M2<-apply(Raven(cof(square()),vrule = "lty"))   
draw(M2, bg = "blue", n.cell = 4)

par(mfrow=c(1,1))
Canvas(xlim=16,mar=c(0.5,0.5,0.5,0.5))

draw(vline(pos.x = -5, s.x=55, lwd = 15))
draw(vline(pos.x =0, s.x=55, lwd = 15), canvas = F)


par(mfrow=c(2,1))
draw(hline(s.x=55, lwd = 15))
draw(hline(s.x=55, lwd = 15))


M <-com(M2,M1)
draw(M)
m1a = hide(M1$Sq9, 3)

ic.inc = cof(M2$Sq9, m1a)
draw(ic.inc)

# risposta corretta
Correct<-M$Sq9
# Matrice
risposte<-list()

M$Sq9<-hide(M$Sq9)
# wp- copy può essere una cella qualsiasi Sq1:sq3, sq4, sq7 -
# propongo di far selezionare a caso quale prenderer
index.copy = sample(c(1:4, 7))[1]
risposte[[1]] = (M[[index.copy]])
# wp matrix - unione di TUTTE le celle della matrice meno la corretta
risposte[[2]] = M$Sq1
for (i in 2:8) {
  risposte[[2]] = cof(risposte[[2]], M[[i]])
}

draw(risposte[[2]])
# mi piaceva l'idea di controllare se in almeno una delle celle la linea 
# lty è diversa da 1 e applicarla ad almeno un elemento

# d union
d.union = M[[1]]
for (i in 2:8) {
  d.union = cof(d.union, M[[i]])
}
# queste sono tutte le forme che NON sono in d.union
exclusion = shapes[!shapes$name %in% unlist(d.union$shape), ]
random<-sample(1:length(exclusion$name),3)
for(i in 1:length(random)) {
  f<-get(exclusion$name[random[i]])
  if(i==1){
    obj<-f()
  }else{
    obj<-cof(obj,f())
  }
}
draw(obj)
d.union = cof(d.union, obj)

draw(d.union)

risposte[[3]] = cof(d.union, Correct, 
                    square(s.x=3,s.y=3, shd="black"), 
                    X())

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
