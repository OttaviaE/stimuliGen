current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cat("\014")
rm(list = ls())

source("Basic_functions.R")
source("Complex_functions.R")
source("Rules.R")

Stimoli<-cof(square(),triangle(),elipse())
M<-Raven(Stimoli,c("diff_shapes","lty"),c("diff_shapes"))
M<-apply_rule(M)
draw(M)


Stimoli<-dice()
M2<-Raven(Stimoli)
M2<-apply_rule(M2)
Stimoli2<-cross()
M3<-Raven(Stimoli2,"rotation","rotation")
M3<-apply_rule(M3)
M4<-com(M3,M2)
draw(M4)

Stimoli2<-small_triangle()
M2<-Raven(Stimoli2,"mov_hlr","mov_vud") ##La concatenazione non funziona
M2<-apply_rule(M2)
draw(M2)

Stimoli3<-square()
Stimoli3$size.x<-36
Stimoli3$size.y<-36
M3<-Raven(Stimoli3)
M3<-apply_rule(M3)

M4<-com(M3,M2)
draw(M4)

