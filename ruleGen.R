library(DescTools)
# 19/09/2022 ----- 
# prova generazione di regole con liste e array e tante speranze ---- 
# RELATION: UNARY #
# The same pattern is repeated on each row (the first element determines the second
# and the third)
# which_rot

list(first = c(0.10, 0.50, 0.00), 
     second = c(0.50, 0.00, 0.10), 
     third = c(0.00, 0.10))

rm(rule_relation)
rule_relation = function(rotation = NULL,
    which_rot = c("htv", "vth", 
                                      "dtv", "dth"), shade = NULL, 
                         which_shade = c("wtg", "wtb", "btw", "btg"),
    line = NULL,
                         which_line = c("sdad", "dads", "sdda", "ddas")) {
  the_rule = list()
  # which_rots
    h = 0; d= pi/3; v = pi/2
    rot_rule = list()
    # colors 
    sw = 0; sg = 0.10; sb = 0.6
    shade_rule = list()
    # which_line 
    s = 1; da = 5; do = 3
    line_rule = list()
    if (!is.null(rotation) && is.null(shade) && is.null(line)) {
      # htv ----
      if (which_rot == "htv") {
        for (i in 1:3) {
          rot_ruleG = c(h, d, v)
          rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
        }
      }  # vth ----
      else if (which_rot == "vth") {
        for (i in 1:3) {
          rot_ruleG = c(v,d,h)
          rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
        }
      } # dth ----
      else if (which_rot == "dth") {
        for (i in 1:3) {
          rot_ruleG = c(d,h,v)
          rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
        }
      } # dtv -----
      else if (which_rot == "dtv") {
        for (i in 1:3) {
          rot_ruleG = c(d,v,h)
          rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
        }
      }
      
      the_rule[["rotation"]] = rot_rule
      
    } else if (!is.null(shade) && is.null(rotation) && is.null(line)) {
      if (which_shade == "wtg") {
        for (i in 1:3) {
          shade_ruleG = c(sw, sb, sg)
          shade_rule[[i]] = shade_ruleG
        }
      } else if (which_shade == "wtb") {
        for (i in 1:3) {
          shade_ruleG = c(sw, sg, sb)
          shade_rule[[i]] = shade_ruleG
        }
      } else if (which_shade == "btw") {
        for (i in 1:3) {
          shade_ruleG = c(sb, sg, sw)
          shade_rule[[i]] = shade_ruleG
        }
      }  else if (which_shade == "btg") {
        for (i in 1:3) {
          shade_ruleG = c(sb, sw, sg)
          shade_rule[[i]] = shade_ruleG
        }
      }
      the_rule[["shade"]] = shade_rule
      
    } else if(!is.null(line) && is.null(shade) && is.null(rotation)) {
      if (which_line == "sdad") {
        for (i in 1:3) {
          line_ruleG = c(s, da, do) 
          line_rule[[i]] = line_ruleG
        } 
      } else if (which_line == "dads") {
        for (i in 1:3) {
          line_ruleG = c(da, do, s) 
          line_rule[[i]] = line_ruleG
        }
      } else if (which_line == "sdda") {
        for (i in 1:3) {
          line_ruleG = c(s, do, da) 
          line_rule[[i]] = line_ruleG
        }
      } else if (which_line == "ddas") {
        for (i in 1:3) {
          line_ruleG = c(do, da, s) 
          line_rule[[i]] = line_ruleG
        }
      }
      the_rule[["line"]] = line_rule
    } else if (!is.null(line) && !is.null(shade) && !is.null(rotation) ) {
      # htv ----
      if (which_rot == "htv") {
        for (i in 1:3) {
          rot_ruleG = c(h, d, v)
          rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
        }
      }  # vth ----
      else if (which_rot == "vth") {
        for (i in 1:3) {
          rot_ruleG = c(v,d,h)
          rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
        }
      } # dth ----
      else if (which_rot == "dth") {
        for (i in 1:3) {
          rot_ruleG = c(d,h,v)
          rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
        }
      } # dtv -----
      else if (which_rot == "dtv") {
        for (i in 1:3) {
          rot_ruleG = c(d,v,h)
          rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
        }
      }
      
      the_rule[["rotation"]] = rot_rule
      if (which_shade == "wtg") {
        for (i in 1:3) {
          shade_ruleG = c(sw, sb, sg)
          shade_rule[[i]] = shade_ruleG
        }
      } else if (which_shade == "wtb") {
        for (i in 1:3) {
          shade_ruleG = c(sw, sg, sb)
          shade_rule[[i]] = shade_ruleG
        }
      } else if (which_shade == "btw") {
        for (i in 1:3) {
          shade_ruleG = c(sb, sg, sw)
          shade_rule[[i]] = shade_ruleG
        }
      }  else if (which_shade == "btg") {
        for (i in 1:3) {
          shade_ruleG = c(sb, sw, sg)
          shade_rule[[i]] = shade_ruleG
        }
      }
      the_rule[["shade"]] = shade_rule
      
      if (which_line == "sdad") {
        for (i in 1:3) {
          line_ruleG = c(s, da, do) 
          line_rule[[i]] = line_ruleG
        } 
      } else if (which_line == "dads") {
        for (i in 1:3) {
          line_ruleG = c(da, do, s) 
          line_rule[[i]] = line_ruleG
        }
      } else if (which_line == "sdda") {
        for (i in 1:3) {
          line_ruleG = c(s, do, da) 
          line_rule[[i]] = line_ruleG
        }
      } else if (which_line == "ddas") {
        for (i in 1:3) {
          line_ruleG = c(do, da, s) 
          line_rule[[i]] = line_ruleG
        }
      }
      the_rule[["line"]] = line_rule
    }
    return(the_rule)
}

# colors
(my_rule = rule_relation(rotation = T, which_rot = "htv"))
(my_rule = rule_relation(shade = T, which_shade = "wtb"))
(my_rule = rule_relation(line = T, which_line = "dads"))

(my_rule = rule_relation(rotation = NULL,  
              shade = T, which_shade = "wtb")
)
(my_rule = rule_relation(rotation = T,  which_rot = "vth", 
                         shade = T, which_shade = "wtg",
                         line = T, which_line = "sdad")
)

par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
Canvas()
stimElli(my_rule, line = T)

stimTri(my_rule)

# which_lines 
line_ruleG = c(s,da,do)
line_rule[[i]] = line_ruleG

(my_rot = rule_relation(which_rot = "htv"))
(my_which_shade = rule_relation(which_rot = "htv", which_shade = "wtg"))
my_rules = rule_relation(which_rot = "htv", which_shade = "wtg", which_line = "sdad")
par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
Canvas()
stimElli(which_rot = my_rot)
stimElli(which_rot = my_rot, color = my_which_shade, which_line = my_rules)
stimTri(which_rot = my_rules, 
        color = my_rules, 
        which_line = my_rules)



Canvas()
stimTri(which_rot = my_rot)

par(mfrow = c(1,1))
plot(c(0,1),c(0,1), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
Canvas(15,15)
DrawEllipse(x = 0,
            radius.x = 10, 
            radius.y = 15, 
            lwd = 2, 
            rot = pi/3, plot = T)

DrawRegPolygon(rot = my_rot[[2]][1], nv=3)

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
