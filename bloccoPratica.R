
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("distrattoriBrutti.R")


rot_h <-apply(Raven(st1=cof(e.hexagon()),
                    hrule=c("fill"),vrule=c("identity")))
draw(rot_h, hide = TRUE)

resp_rot_h = list(repetition(rot_h)$r.left,
                wp(rot_h)$wp.copy, 
                  correct(rot_h), 
                  d.union(rot_h, shapes.in = c("dice", "X", "cross")), 
                  ic(rot_h)$ic.flip)


draw.distr(resp_rot_h)


s_v = apply(Raven(st1=star(),
                  hrule=c("identity"),
                  vrule=c("size")))
draw(s_v, hide = T)

resp_sv_v = list(
  repetition(s_v)$r.top,
  wp(s_v)$wp.copy, 
  correct(s_v), 
  d.union(s_v, shapes.in = c("dice", "X", "cross")), 
  ic(s_v)$ic.size
)

draw.distr(resp_sv_v)

lwd_hv = apply(Raven(st1=lily(),
                     hrule=c("lty"),
                     vrule=c("identity")))
draw(lwd_hv, hide = T)


m1 = apply(Raven(st1 = cof(dot(), 
                            s.lily(), 
                            square(s.x = 5, s.y = 5, 
                                   shd = "black", rot = pi/2)), 
                 hrule = "diff_shapes"))
draw(m1)           
m2 = apply(Raven(st1=pentagon(),
                 hrule=c("identity"),
                 vrule=c("identity")))
draw(m2)
mix = (com(m2, m1))
draw(mix, hide = T)


