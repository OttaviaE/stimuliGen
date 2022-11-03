current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cat("\014")
rm(list = ls())

source("Class and Methods.R")
source("Shapes_list.R")
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
xcros$lwd<-xcros$lwd[[1]]+4
xcros$size.x<-xcros$size.x-4
xcros$size.y<-xcros$size.y-4


# Esempio di svillupo di stimolo D4
# Cambio di forma verticale, addizione oggettuale e cambio di forma orizzontale

pdf("D4_v01.pdf")
M1<-apply(Raven(cof(s.lilth,square(s.x=3,s.y=3,rot = pi/2),cross()),
                "diff_shapes"))
M2<-apply(Raven(cof(square(),circle(),dice()),vrule = "diff_shapes"))          
draw(com(M2,M1))

dev.off()

##FUCK per motivi non chiari ma sicuramente relati a draw circle i seguenti due codici hanno plot diversi
#draw(Raven(cof( lily(),dot(),cross())))
#draw(Raven(cof(dot(),cross(), lily())))

pdf("D4_v02.pdf")
M1<-apply(Raven(cof(dot(),square(s.x=3,s.y=3,rot = pi/2, 
                                 shd="black"),xcros),
                "diff_shapes"))
M2<-apply(Raven(cof(square(),circle(),pentagon()),vrule = "diff_shapes"))        
draw(com(M2,M1))

dev.off()

# Esempio di svillupo di stimolo D6
# Cambio di forma LR_TL, addizione oggettuale e cambio di di contorno

pdf("D6_v01.pdf")
M1<-apply(Raven(cof(s.lily(),square(s.x=3,s.y=3,rot = pi/2),cross()),
                "diff_shapes","diff_shapes"))
M2<-apply(Raven(cof(square()),vrule = "lty"))          
draw(com(M2,M1))

dev.off()

pdf("D6_v02.pdf")
M1<-apply(Raven(cof(s.lily(),square(s.x=3,s.y=3,rot = pi/2),cross()),
                "diff_shapes","diff_shapes"))
M2<-apply(Raven(cof(square()),vrule = "lwd"))          
draw(com(M2,M1))

dev.off()

# Esempio di svillupo di stimolo E1
# Cambio di forma Superimposizione matriciale
pdf("E1_v01.pdf")
M<-logic_rules(Raven(lily()),"OR")
draw(M)

dev.off()

pdf("E1_v02.pdf")
M<-logic_rules(Raven(cof(hline(),square(s.x=3,s.y=3,rot = pi/2, shd="black"),dice(),vline())),"OR")
draw(M)
dev.off()

# Esempio di svillupo di stimolo E11
# Cambio di forma Superimposizione matriciale
pdf("E11_v01.pdf")
M1<-logic_rules(Raven(square4()),"AND")
M2<-logic_rules(Raven(lilth),"XOR")
draw(com(M1,M2))

dev.off()

