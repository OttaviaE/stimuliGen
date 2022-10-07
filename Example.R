current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cat("\014")
rm(list = ls())

source("Basic_functions.R")
source("Complex_functions.R")
source("Rules.R")

Stimoli<-small_triangle()
M<-Raven(Stimoli)
draw(apply_rule(M))
 