source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")
set.seed(999)

pratica001 = apply(Raven(st1=pie.4(),
                         hrule=c("identity"),
                         vrule=c("size")))


pratica001_dist = responses(pratica001)


# cambiare lo spessore del cerchio ------

resp_pratica001 = list(correct = correct(pratica001), 
                       dist1 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist2 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist3 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist4 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist5 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist6 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist7 = circle(s.x = 15, s.y = 15, lwd = 3)) 



## -----------------------------------------------------------------------------
pratica002 <-apply(Raven(st1=cof(e.hexagon()),
                         hrule=c("fill"),vrule=c("identity")))


pratica002_dist = responses(pratica002, choose.copy = 1)



# distrattori manuali m pratica 1 

resp_pratica002 = within(pratica002_dist, 
                         rm(r.left, r.top, ic.scale, ic.inc))

resp_pratica002[["ic.neg"]] = ic.neg(pratica002)
resp_pratica002[["wp.copy"]] = rotation(resp_pratica002$wp.copy, 2)
resp_pratica002[["d.difference"]] = square()



## -----------------------------------------------------------------------------
pratica003 = apply(Raven(st1=u.pie.2(),
                         hrule=c("lty"),
                         vrule=c("identity")))


pratica003_dist = responses(pratica003, choose.copy = 1, choose.start = 3)

resp_pratica003 = pratica003_dist

resp_pratica003 = within(resp_pratica003, 
                         rm(r.left, r.top, ic.scale, ic.inc))


correct_pratic003 = correct(pratica003)
correct_pratic003$shade[[1]] = rep("grey", 2)
resp_pratica003[["ic.neg"]] = (correct_pratic003)
resp_pratica003[["difference"]] = ellipse(shd = "black")

## ----out.width="80%"----------------------------------------------------------



## -----------------------------------------------------------------------------
pratica4a = apply(Raven(st1 = cof(dot(), 
                                    s.lily(), 
                                    square(s.x = 5, s.y = 5, 
                                           shd = "black", rot = pi/2)), 
                          hrule = "diff_shapes"))

pratica4b = apply(Raven(st1=pentagon(),
                          hrule=c("identity"),
                          vrule=c("identity")))

pratica004 = (com(pratica4a, pratica4b))


pratica004_dist = responses(pratica004, choose.copy = 1)


## ----out.width="80%"----------------------------------------------------------

resp_pratica004 = within(pratica004_dist, 
                         rm(r.left, r.top, ic.scale))

s_p4 = split.mat(pratica004)

s_p4[[2]]$shade[[1]] = "black"
s_p4[[1]]$shade[[1]] = "white"

resp_pratica004[["ic.neg"]] = cof(s_p4[[2]], s_p4[[1]])


