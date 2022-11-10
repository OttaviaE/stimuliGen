current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cat("\014")
rm(list = ls())

source("Shapes_list-07-11-Ottavia.R")
source("Class and Methods.R")
source("Class and Methods extension.R")

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

draw(com(M2,M1))

###ESEMPIO 4

M1<-logic_rules(Raven(square4()),"OR")
draw(M1)

M2<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"XOR")
draw(M2)

draw(com(M1,M2))

### ESEMPIO 5

M1<-apply(Raven(lily(),"rotation","rotation"))
draw(M1)


### ESEMPIO 6

M1<-apply(Raven(bow.tie(),"multifill","multifill"))
draw(M1)

