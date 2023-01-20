## ----r------------------------------------------------------------------------
set.seed(999)
knitr::opts_chunk$set(echo=FALSE, 
                      eval=TRUE, 
                      warning = FALSE, 
                      message = FALSE, 
                      fig.align = "center")
knitr::knit_hooks$set(purl = knitr::hook_purl)

rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")


## ----r------------------------------------------------------------------------
m1 = apply(Raven(
  st1 = pacman(), 
  hrule = "fill", 
  vrule = "identity"
))

draw(m1)


## ----r out.width="80%"--------------------------------------------------------
dist.m1 = responses(m1)

draw.dist(dist.m1)


## ----r------------------------------------------------------------------------
m2 = apply(Raven(
  st1 = pacman(), 
  vrule = "fill", 
  hrule = "identity"
))

draw(m2)


## ----r out.width="80%"--------------------------------------------------------
dist.m2 = responses(m2)

draw.dist(dist.m2)


## ----r------------------------------------------------------------------------
m3 = apply(Raven(
  st1 = pacman(), 
  vrule = "fill", 
  hrule = "fill"
))

draw(m3)


## ----r out.width="80%"--------------------------------------------------------
dist.m3 = responses(m3)

draw.dist(dist.m3)


## ----r------------------------------------------------------------------------
m3a = apply(Raven(
  st1 = cof(pacman(), square(), ellipse()), 
hrule="diff_shapes.inv",
vrule="diff_shapes"
))

draw(m3a)

## ----r out.width="80%"--------------------------------------------------------
dist.m3a = responses(m3a)

draw.dist(dist.m3a)


## ----r------------------------------------------------------------------------
m4 = apply(Raven(
  st1 = pacman(), 
  hrule = c("fill", "size"), 
  vrule = "identity"
))

draw(m4)

## ----r out.width="80%"--------------------------------------------------------
dist.m4 = responses(m4)

draw.dist(dist.m4)


## ----r------------------------------------------------------------------------
m5 = apply(Raven(
  st1 = pacman(), 
  vrule = c("fill", "size"), 
  hrule = "identity"
))

draw(m5)

## ----r out.width="80%"--------------------------------------------------------
dist.m5 = responses(m5)

draw.dist(dist.m5)


## ----r------------------------------------------------------------------------
m6 = apply(Raven(
  st1 = pacman(), 
  vrule = c("fill"), 
  hrule = "size"
))

draw(m6)

## ----r out.width="80%"--------------------------------------------------------
dist.m6 = responses(m6)

draw.dist(dist.m6)


## ----r------------------------------------------------------------------------
m7 = apply(Raven(
  st1 = pacman(), 
  vrule = c("size"), 
  hrule = "fill"
))

draw(m7)

## ----r out.width="80%"--------------------------------------------------------
dist.m7 = responses(m7)

draw.dist(dist.m7)


## ----r------------------------------------------------------------------------
m8 = apply(Raven(
  st1 = pacman(), 
  hrule = c("size", "fill"), 
  vrule = c("size", "fill")
))

draw(m8)

## ----r------------------------------------------------------------------------
dist.m8 = responses(m8)

draw.dist(dist.m8)


## ----r------------------------------------------------------------------------
m8a = apply(Raven(
  st1 = cof(pacman(), square(), ellipse()), 
hrule=c("diff_shapes.inv", "fill"),
vrule=c("diff_shapes", "fill")
))

draw(m8a)

## ----r out.width="80%"--------------------------------------------------------
dist.m8a = responses(m8a)

draw.dist(dist.m8a)


## ----r------------------------------------------------------------------------
m9 = apply(Raven(
  st1 = pacman(), 
    hrule = c("size", "fill", "rotation"), 
  vrule = "identity"
))

draw(m9)

## ----r out.width="80%"--------------------------------------------------------
dist.m9 = responses(m9)

draw.dist(dist.m9)

## ----r------------------------------------------------------------------------
m10 = apply(Raven(
  st1 = pacman(), 
    hrule = c("size", "fill"), 
  vrule = "rotation"
))

draw(m10)

## ----r out.width="80%"--------------------------------------------------------
dist.m10 = responses(m10)

draw.dist(dist.m10)

## ----r------------------------------------------------------------------------
m11 = apply(Raven(
  st1 = pacman(), 
    hrule = c("size"), 
  vrule = c("rotation", "fill")
))

draw(m11)

## ----r out.width="80%"--------------------------------------------------------
dist.m11 = responses(m11)

draw.dist(dist.m11)

## ----r------------------------------------------------------------------------
m12 = apply(Raven(
  st1 = pacman(), 
    hrule = c("identity"), 
  vrule = c("rotation", "fill", "size")
))

draw(m12)

## ----r out.width="80%"--------------------------------------------------------
dist.m12 = responses(m12)

draw.dist(dist.m12)

## ----r------------------------------------------------------------------------
m13 = apply(Raven(
  st1 = pacman(), 
    hrule = c("size"), 
  vrule = c("rotation", "fill")
))

draw(m13)

## ----r out.width="80%"--------------------------------------------------------
dist.m12 = responses(m12)

draw.dist(dist.m12)

## ----r------------------------------------------------------------------------

m12a = apply(Raven(
  st1 = cof(pacman(), square(), ellipse()), 
hrule=c("diff_shapes.inv", "lty.inv", "rotation3.inv"),
vrule=c("diff_shapes", "lty", "rotation3")
))

draw(m12a)

## ----r out.width="80%"--------------------------------------------------------
dist.m12a = responses(m12a)

draw.dist(dist.m12a)


