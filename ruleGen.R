library(DescTools)
# 19/09/2022 ----- 
# prova generazione di regole con liste e array e tante speranze ---- 
# RELATION: UNARY #
# The same pattern is repeated on each row (the first element determines the second
# and the third)
# rotation

list(first = c(0.10, 0.50, 0.00), 
     second = c(0.50, 0.00, 0.10), 
     third = c(0.00, 0.10))

rm(rule_relation)
rule_relation = function(rotation = c("htv", "vth", 
                                      "dtv", "dth"), 
                         shade = NULL, 
                         size = NULL) {
  if (is.null(rotation) == F & is.null(shade) == T & is.null(size) == T) {
    h = 0; d= pi/3; v = pi/2
    rot_rule = list()
    shade_ruleG = c(0, 0.10, 0.50)
    if (rotation == "htv") {
      for (i in 1:3) {
        rot_ruleG = c(h, d, v)
        rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
      }
    } else if (rotation == "vth") {
      for (i in 1:3) {
        rot_ruleG = c(v,d,h)
        rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
      }
    } else if (rotation == "dth") {
      for (i in 1:3) {
        rot_ruleG = c(d,h,v)
        rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
      }
    } else if (rotation == "dtv") {
      for (i in 1:3) {
        rot_ruleG = c(d,v,h)
        rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
      }
    }
    the_rule = rot_rule
  } 
  return(the_rule)
}


my_rot = rule_relation(rotation = "dth")
par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
Canvas()
stimElli(rotation = my_rot)
DrawEllipse(x= 0, rot = my_rot[[2]][1])


Canvas()
stimTri(rotation = my_rot)

plot(c(0,1),c(0,1), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
Canvas()
DrawRegPolygon(rot = my_rot[[2]][1], nv=4)
DrawRegPolygon(x = 0.5, y = 0.5, rot =  c(my_rot[[1]][1], 
                                          my_rot[[2]][1], 
                                          my_rot[[3]][1]), radius.x = 0.5, nv = 3,
               col = SetAlpha(c("yellow", "blue", "pink"),0.5))

par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
for (i in 1:length(my_rot)) {
  for (j in 1:length(my_rot[[i]])) {
    temp = NULL
    Canvas(15, 15)
    DrawEllipse(x = 0,
                radius.x = 10, 
                radius.y = 15, 
                lwd = 2, 
                rot = my_rot[[i]][[j]], plot = T)
  }
}

par(mfrow=c(1,2))
Canvas(15, 15)
DrawEllipse(x = 0,
            radius.x = 10, 
            radius.y = 15, rot = 0)




Canvas()
DrawEllipse(rot = c(1:3) * pi/3, col=SetAlpha(c("blue","red","green"), 0.5) )
