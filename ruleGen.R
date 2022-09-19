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
    rot_ruleG = c(0, pi/3, pi)
    rot_rule = list()
    if (rotation == "htv") {
      for (i in 1:length(rot_ruleG)) {
        rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
      }
    } else if (rotation == "vth") {
      for (i in length(rot_ruleG):1) {
        rot_rule[[i]] = rep(rot_ruleG[i], length(rot_ruleG))
      }
    }
    the_rule = rot_rule
  } 
  return(the_rule)
}


par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)

my_rot = rule_relation(rotation = "htv")

Canvas()
stimElli(rotation = my_rot)
