# 27/09 ----
# provo a creare le regole gerarchiche con integrate le permutazioni
rm(list = ls()[!ls() %in% "stimElli"])
highRule = function(rotation = NULL, shade = NULL, line = NULL, 
                    multi = F) {
  if (is.null(rotation) & is.null(shade) & is.null(line) & multi == F) {
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
  } else if (multi == T) {
    ruleG = "multi"
  }
  return(ruleG)
}


highRule(shade = T)
  
getDone = function(object, 
                   rotation = c("htv", "vth", "dtv", "dth"), 
                   shade = c("wtg", "wtb", "btw", "btg"), 
                   line = c("sdad", "dads", "sdda", "ddas"), 
                   multi = c("increasing", "decreasing")){
  the_rule = list()
  # empty rotation 
  empty_rot = matrix(0, nrow = 3, ncol = 3)
  # empty shade 
  empty_shade = matrix(0, nrow = 3, ncol = 3)
  # empty line 
  empty_line = matrix(1, nrow = 3, ncol = 3)
  
  
  # htv 
  htv = c(h = pi/2, d= pi/3, v = 0)
  # vth 
  vth = c(v=0, d=pi/3, h=pi/2)
 
  # wtb
  wtb = c(sw = 0, sg = 0.20, sb = 0.8)
  # wtg 
  wtg = c(sw = 0,  sb = 0.8, sg = 0.20)
  
  # sdad 
  sdad = c(s = 1, da = 5, do = 3)
  # dads
  dads = c(da = 5, do = 3, s = 1)
  
  if (object == "rotation") {
    if (rotation == "htv") {
      start = list(rot = htv)
      
    } else if (rotation == "vth") {
      start = list(rot = htv)
    }
    rest = list(rotation = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                shade = empty_shade, 
                line = empty_line)
  } else if (object == "shade") {
    if (shade == "wtb") {
      start = list(shade = wtb)
    } else if (shade == "wtg") {
      start = list(shade = wtg)
    }
    rest = list(rotation = empty_rot, 
                shade =Permn(start[["shade"]])[c(seq(1,5,by=2)), ], 
                line = empty_line)
  } else if (object == "line") {
    if (line == "sdad") {
      
      start = list(line = sdad)
    } else if (line == "dads") {
      start = list(line = dads)
    }
    rest = list(rotation = empty_rot, 
                shade = empty_shade, 
                line =Permn(start[["line"]])[c(seq(1,5,by=2)), ])
  } else if (object == "rot_shade") { # rot_shade -----
    if (rotation == "htv" & shade == "wtg") { # htv and wtg ----
      start = list(rot = htv, 
                   shade = wtg)
    } else if (rotation == "htv" & shade == "wtb") { # htv and wtg -----
      start = list(rot = htv, 
                   shade = wtb)
    }
    rest = list(rotation = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                shade = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                line= empty_line)
  } else if (object == "rot_line") {
    if (rotation == "htv" & line == "sdad") { # htv and wtg ----
      start = list(rot = htv, 
                   line = sdad)
    } else if (rotation == "htv" & line == "dads") { # htv and wtg -----
      start = list(rot = htv, 
                   line = dads)
    }
    rest = list(rotation = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                shade = empty_shade, 
                line= Permn(start[["line"]])[c(seq(1,5,by=2)), ])
  } else if (object == "shade_line") {
    if (shade == "wtb" & line == "sdad") { # htv and wtg ----
      start = list(shade = wtb, 
                   line = sdad)
    } else if (shade == "wtg" & line == "sdad") { # htv and wtg -----
      start = list( shade = wtg, 
                   line = sdad)
    }
    rest = list(rotation = empty_rot, 
                shade = Permn(start[["shade"]])[c(seq(1,5,by=2)), ], 
                line= Permn(start[["line"]])[c(seq(1,5,by=2)), ])
  } else if (object == "all") {
    if (rotation == "htv" & shade == "wtb" & line == "sdad") {
      start = list(rot = htv, 
                   shade = wtb, 
                   line = sdad)
    } else if (rotation == "vth" & shade == "wtb" & line == "sdad") {
      start = list(rot = vth, 
                   shade = wtb, 
                   line = sdad)
    }
    rest = list(rotation = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                shade = Permn(start[["shade"]])[c(seq(1,5,by=2)), ], 
                line= Permn(start[["line"]])[c(seq(1,5,by=2)), ])
  } else if (multi == "increasing") {
    rest = matrix(c(c(1:3), c(2,1,3), c(3:1)), nrow=3, ncol= 3, byrow = T)
  } else if (multi == "decreasing") {
    rest = matrix(c(c(3:1), c(2,1,3), c(1:3),), nrow=3, ncol= 3, byrow = T)
  }
  the_rule = rest
  return(the_rule)
  }
matrix(c(c(1:3), c(3:1), c(2,1,3)), nrow=3, ncol= 3, byrow = T)

stimElli(getDone(highRule(multi = T), multi = "increasing"))

stimElli(getDone(highRule(multi = T), multi = "increasing"))
getDone(highRule(rotation = T), rotation = "htv")
cont = prova[which(is.null(prova)) == F]

prova = (getDone(highRule(shade = T), shade = "wtb"))

stimElli(getDone(highRule(line = T), line = "sdad"))
stimElli(getDone(highRule(line = T, rotation = T), line = "sdad", 
                 rotation = "htv"))
stimElli(getDone(highRule(line = T, shade = T), line = "sdad", 
                 shade = "wtg"))

prova = (getDone(highRule(line = T, shade = T, rotation = T), 
                 line = "sdad", 
                 shade = "wtb", 
                 rotation = "htv"))
p = getDone(highRule(multi = T), multi = "increasing")

stimElli(getDone(highRule(multi = T), multi = "increasing"))
highRule(rotation  = T)
