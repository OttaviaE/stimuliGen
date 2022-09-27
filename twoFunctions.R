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
  # empty rotation 
  empty_rot = matrix(start[["rot"]], nrow = 3, ncol = 3)
  # empty shade 
  empty_shade = matrix(start[["shade"]], nrow = 3, ncol = 3)
  # empty line 
  empty_line = matrix(start[["rot"]], nrow = 3, ncol = 3)
  # base rotation 
  base_rot = c(rep(0, 3))
  # base shade 
  base_shade = c(rep(0, 3))
  # base line 
  base_line = c(rep(1, 3))
  
  # htv 
  htv = c(h = pi/2, d= pi/3, v = 0)
  # vth 
  vth = c(v=0, d=pi/3, h=pi/2)
 
  # wtb
  wtb = c(sw = 0, sg = 0.10, sb = 0.6)
  # wtg 
  wtg = c(sw = 0,  sb = 0.6, sg = 0.10)
  
  # sdad 
  sdad = c(s = 1, da = 5, do = 3)
  # dads
  dads = c(da = 5, do = 3, s = 1)
  
  if (object == "rotation") {
    if (rotation == "htv") {
      start = list(rot = htv, 
                   shade = base_shade, 
                   line = base_line)
      
    } else if (rotation == "vth") {
      start = list(rot = htv, 
                   shade = base_shade, 
                   line = base_line)
    }
    rest = list(rotation = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                shade = empty_shade, 
                line = empty_line)
  } else if (object == "shade") {
    if (shade == "wtb") {
      start = list(rot = base_rot, 
                   shade = wtb, 
                   line = base_line)
    } else if (shade == "wtg") {
      start = list(rot = base_rot, 
                   shade = wtg, 
                   line = base_line)
    }
    rest = list(rotation = empty_rot, 
                shade =Permn(start[["shade"]])[c(seq(1,5,by=2)), ], 
                line = empty_line)
  } else if (object == "line") {
    if (line == "sdad") {
      
      start = list(rot = c(rep(0,3)), 
                   shade = c(rep(0,3)), 
                   line = sdad)
    } else if (line == "dads") {
      start = list(rot = c(rep(0,3)), 
                   shade = c(rep(0,3)), 
                   line = dads)
    }
    rest = list(rotation = matrix(start[["rot"]], nrow = 3, ncol = 3), 
                shade =matrix(start[["shade"]], nrow = 3, ncol = 3), 
                line =Permn(start[["line"]])[c(seq(1,5,by=2)), ])
  } else if (object == "rot_shade") {
    if (rotation == "htv" & shade == "wtg") {
      start = list(rot = htv, 
                   shade = wtg, 
                   line = base_line)
    } else if (rotation == "htv" & shade == "wtb") {
      start = list(rot = htv, 
                   shade = wtb, 
                   line = base_line)
    }
    rest = list(rotation = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                shade = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                line= empty_line)
  }  
  the_rule = rest
  return(the_rule)
  }

stimElli(getDone(highRule(rotation = T), rotation = "htv"))
stimElli(getDone(highRule(shade = T), shade = "wtb"))
stimElli(getDone(highRule(line = T), line = "sdad"))
