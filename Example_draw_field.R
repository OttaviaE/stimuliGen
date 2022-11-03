current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cat("\014")
rm(list = ls())

source("Class and Methods.R")
source("Class and Methods extension.R")
source("Shapes_list.R")
source("Rules_27102022.R")

## Forme modificate ad Hoc per questi esempi

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

#### Esempio E01 

M<-logic_rules(Raven(cof(hline(),square(s.x=3,s.y=3,rot = pi/2, shd="black"),dice(),vline())),"OR")
draw(M)

## Quadrato (3,3) tolgo la corretta dalla matrice
Correct<-M$Sq9
M1<-M
M1$Sq9<-hide(M1$Sq9)


##Funzione Draw estesa per il campo field
draw(Correct)
draw(M1)


##Rimpiazzo  il quadrato con il Dot
wrong1<-replace(Correct,2,dot())
draw(wrong1)


##Rimpiazzo  il quadrato con lilith
wrong2<-replace(Correct,2,s.lilth)
draw(wrong2)

##Rimuovo la croce
wrong3<-hide(wrong2,c(1,4))
draw(wrong3)

