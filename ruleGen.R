library(DescTools)
# 19/09/2022 ----- 
# prova generazione di regole con liste e array e tante speranze ---- 
# RELATION: UNARY #
# The same pattern is repeated on each row (the first element determines the second
# and the third)
# which_rot

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

