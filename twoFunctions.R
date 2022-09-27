# 27/09 ----
# provo a creare le regole gerarchiche con integrate le permutazioni

highRule = function(rotation = NULL, shade = NULL, line = NULL) {
  if (is.null(rotation) & is.null(shade) & is.null(line)) {
    stop("Please specificy an argument") 
  } else if (!is.null(rotation) & is.null(shade) & is.null(line)){
    ruleG = "rotation"
  } else if (!is.null(rotation) & !is.null(shade) & is.null(line)) {
    ruleG = "rot_shade"
  } else if (!is.null(rotation) & is.null(shade) & !is.null(line)) {
    ruleG = "rot_line" 
  } else if (is.null(rotation) & !is.null(shade) & !is.null(line)) {
    ruleG = "shade_line"
  } else if (!is.null(rotation) & !is.null(shade) & !is.null(line)) {
    ruleG = "all"
  } else if (is.null(rotation) & !is.null(shade) & is.null(line)) {
    ruleG = "shade"
  } else if(is.null(rotation) & is.null(shade) & !is.null(line)) {
    ruleG = "line"
  }
  return(ruleG)
}


highRule(shade = T)
  
getDone = function(object, 
                   rotation = c("htv", "vth", "dtv", "dth"), 
                   shade = c("wtg", "wtb", "btw", "btg"), 
                   line = c("sdad", "dads", "sdda", "ddas")){
  the_rule = list()
  # which_rots
  h = pi/2; d= pi/3; v = 0
  rot_rule = list()
  # colors 
  sw = 0; sg = 0.10; sb = 0.6
  shade_rule = list()
  # which_line 
  s = 1; da = 5; do = 3
  line_rule = list()
  
  if (object == "rotation") {
    if (rotation == "htv") {
      htv = c(h = pi/2, d= pi/3, v = 0)
      start = list(rot = htv, 
                   shade = c(rep(0,3)), 
                   line = c(rep(1, 3)))
      
    } else if (rotation == "vth") {
      vth = c(v=0, d=pi/3, h=pi/2)
      start = list(rot = htv, 
                   shade = c(rep(0,3)), 
                   line = c(rep(1, 3)))
    }
    rest = list(rotation = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                shade =matrix(start[["shade"]], nrow = 3, ncol = 3), 
                line = matrix(start[["line"]], nrow = 3, ncol = 3))
  } else if (object == "shade") {
    if (shade == "wtb") {
      wtg = c(sw = 0, sg = 0.10, sb = 0.6)
      start = list(rot = c(rep(0,3)), 
                   shade = wtg, 
                   line = c(rep(1, 3)))
    } else if (shade == "wtg") {
      wtg = c(sw = 0,  sb = 0.6, sg = 0.10)
      start = list(rot = c(rep(0,3)), 
                   shade = wtg, 
                   line = c(rep(1, 3)))
    }
    rest = list(rotation = matrix(start[["rot"]], nrow = 3, ncol = 3), 
                shade =Permn(start[["shade"]])[c(seq(1,5,by=2)), ], 
                line = matrix(start[["line"]], nrow = 3, ncol = 3))
  } else if (object == "line") {
    if (line == "sdad") {
      sdad = c(s = 1, da = 5, do = 3)
      start = list(rot = c(rep(0,3)), 
                   shade = c(rep(0,3)), 
                   line = sdad)
    } else if (line == "dads") {
      dads = c(da = 5, do = 3, s = 1)
      start = list(rot = c(rep(0,3)), 
                   shade = c(rep(0,3)), 
                   line = dads)
    }
    rest = list(rotation = matrix(start[["rot"]], nrow = 3, ncol = 3), 
                shade =matrix(start[["shade"]], nrow = 3, ncol = 3), 
                line =Permn(start[["line"]])[c(seq(1,5,by=2)), ])
  }   
  the_rule = rest
  return(the_rule)
  }

stimElli(getDone(highRule(rotation = T), rotation = "htv"))
stimElli(getDone(highRule(shade = T), shade = "wtb"))
stimElli(getDone(highRule(line = T), line = "sdad"))
