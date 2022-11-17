
rot_h <-apply(Raven(st1=cof(e.hexagon()),
                    hrule=c("rotation"),vrule=c("identity")))
rot_h$Sq9 = hide(rot_h$Sq9)
draw(rot_h)


s_v = apply(Raven(st1=star(),
                  hrule=c("identity"),
                  vrule=c("size")))
s_v$Sq9 = hide(s_v$Sq9)
draw(s_v)

lwd_hv = apply(Raven(st1=lily(),
                     hrule=c("lty"),
                     vrule=c("identity")))
draw(lwd_hv)
lwd_hv$Sq9 = hide(lwd_hv$Sq9)
draw(lwd_hv)


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
mix$Sq9 = hide(mix$Sq9)
draw(mix)
