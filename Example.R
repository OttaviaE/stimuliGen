current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cat("\014")
rm(list = ls())

source("Basic_functions.R")
source("Complex_functions.R")
source("Rules.R")

Stimoli<-cof(square(),triangle(),cross(),dice())
M<-Raven(Stimoli,"XOR")
M<-apply_rule(M)
draw(M)

Stimoli2<-dice()
M2<-Raven(Stimoli2,"size")
M2<-apply_rule(M2)

M3<-com(M,M2)
draw(M3)

