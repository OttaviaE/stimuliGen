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

M<-logic_rules(Raven(cof(hline(),square(s.x=3,s.y=3,rot = pi/2, shd="black"),
                         dice(),vline())),"OR")
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
draw(replace(Correct,1,circle()))



# Generazione distrattori ----

# WP copy - copia di una cella della matrice non vicina allo spazio bianco
sel = sample(c(1:4, 7))
wp_copy = (M[[sel[1]]])
draw(wp_copy)
# WP flip
wp_flip = cof(diagline.inv(), square(s.x=3,s.y=3, shd="black"))
draw(wp_flip)

wp = list(wp_copy, wp_flip)

# in questo caso il WP matrix è molto "complesso", combinando assieme tutti gli elementi della 
# matrice si ottiene la risposta corretta




#d-blank non lo facio neanche perché dai. 

# d-union: distrattore con più elementi di qualsiai entrata dellla matrice, ma 
# già la risosta corretta esaurisce tutte le possibili robe da poter mettere nel distrattore
d.union = hide(Correct, c(1, 2,4))
d.union = cof(d.union, Correct, square(s.x=3,s.y=3, shd="black"), X())
draw(d.union)
# D-plus. enfatizzare alcuni aspetti di una cella della matrice per rendere il 
# distrattore più complesso 


# nel caso di questo stimolo, conviene il D-diff
d_diff = hide(Correct, 2)
draw(cof(d_diff, pentagon(s.x = 12, s.y = 12), 
         square(s.x = 20, s.y = 20), hexagon(s.x = 3, s.y = 3), 
         rot.hexagon(s.x = 3, s.y = 3), 
         X())
)
d_diff1 = cof(s.lily(), 
              dot(x = unlist(v.arc.left.down()$pos.x), 
                  y = unlist(v.arc.left.down()$pos.y), 
                  size.x = 1), 
              dot(x = unlist(s_vertical()$pos.x), 
                  y = unlist(s_vertical()$pos.y), 
                  size.x = 1), 
              dot(x = unlist(v.arc.right.up()$pos.x), 
                  y = unlist(v.arc.right.up()$pos.y), 
                  size.x = 1), 
              dot())
draw(d_diff1)

d = list(d.union, d_diff, d_diff1)
# repetition sono indubbiamente i più semplici 
r.left = M$Sq8
r.diag = M$Sq5
r.top = M$Sq6     

r = list(r.left, r.diag, r.top)

# Incomplete correlate 
# IC-neg 
ic.neg = cof(d1, square(s.x=3,s.y=3,rot = pi/2), cross())
draw(ic.neg)
# anche i pallozzi bianchi? Forse aumenta la distanza

# IC fill (cambiamento nella texture o stile della risposta corretta) faccio anche i pallozzi bianchi
# se si puoò
# a mano non riesco 

# IC-flip: ruoto la risposta corretta 
ic.flip = hide(Correct, c(1, 2,4))
draw(cof(ic.flip,square(s.x=3,s.y=3, shd="black"), X()))

# ic scale: tolgo il quadrato e lo metto più grosso
ic.scale = hide(Correct, 2)
draw(cof(ic.scale,square(s.x=8,s.y=8,rot = pi/2, shd="black")))

# ic-inc: versione incompleta della risposta correta 
# tolgo il quadrato 
ic.inc = hide(Correct, 2)
draw(cof(ic.scale))

ic = list(ic.neg, ic.flip, ic.scale, ic.inc)

distracters = list(d.union = d.union, 
                   ic.neg = ic.neg, 
                   ic.inc = ic.inc, 
                   wp.flip =wp_flip, 
                   r.left = r.left, 
                   correct = Correct)
responses = sample(distracters)
par(mfcol = c(3,2))
for (i in 1:6) {
  draw(responses[[i]], main = names(responses)[i])
}

# ALtro stimolo 

M2 = apply(Raven(st1=cof(pentagon(), triangle(),square()), 
                 hrule=c("diff_shapes"),
                 vrule=c("rotation")))
draw(M2)

M<-apply(Raven(st1=s.lily(),hrule=c("rotation"),vrule=c("size")))
draw(M)
