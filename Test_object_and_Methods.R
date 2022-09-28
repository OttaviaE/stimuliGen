current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cat("\014")
rm(list = ls())
source("Object_and_methods.R")

Stmolo1<-field(c("triangle","elipse"),c(17,10),c(15,10),c(pi/6,pi),c(1,1))
M1<-Raven(Stmolo1,c("rotation","size"),"rotation") 
draw(M1)
M1<-apply_rule(M1)
draw(M1)
M2<-Raven(Stmolo1,c("rotation","size"),c("rotation","size")) 
draw(M2)
M2<-apply_rule(M2)
draw(M2)

Stmolo2<-field(c("square","elipse"),c(15,10),c(15,10),c(pi/4,0),c(1,1))
M3<-Raven(Stmolo2,c("size"),"rotation") 
M3<-apply_rule(M3)
draw(M3)

Stimolo3<-field(c("triangle","elipse","square"),
                c(17,10,15),c(15,10,15),c(pi/6,0,pi/4),c(1,1,0))
M4<-Raven(Stimolo3,"identity","identity") 
draw(M4)
M5<-Raven(Stimolo3,"diff_shapes","diff_shapes") 
M5<-apply_rule(M5)
draw(M5)

M6<-Raven(Stimolo3,c("diff_shapes","rotation"),"diff_shapes") 
M6<-apply_rule(M6)
draw(M6)

Stimolo4<-field(c("triangle","square","elipse","elipse","cross"),
                c(17,15,10,10,10),c(15,15,10,7,10),c(pi/6,pi/4,0,0,pi),c(1,1,1,1,1))
M7<-Raven(Stimolo4,"AND","identity") 
M7<-apply_rule(M7)
draw(M7)
