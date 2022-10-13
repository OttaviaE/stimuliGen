
highRule = function(rotation = NULL, shade = NULL, line = NULL, shape = NULL, size = NULL) {
  if (is.null(rotation) & is.null(shade) & is.null(line) ) {
    stop("Please specificy an argument") 
  } else if (!is.null(rotation) & !is.null(shape)  & is.null(shade) & is.null(line) & is.null(size)){
    ruleG = "rotation"
  } else if (!is.null(rotation) & !is.null(shape) & !is.null(shade) & is.null(line)& is.null(size)) {
    ruleG = "rot_shade"
  } else if (!is.null(rotation) & !is.null(shape) & is.null(shade) & !is.null(line) & is.null(size)) {
    ruleG = "rot_line" 
  } else if (is.null(rotation) & !is.null(shape) &  !is.null(shade) & !is.null(line) & is.null(size)) {
    ruleG = "shade_line"
  } else if (!is.null(rotation)& !is.null(shape) & !is.null(shade) & !is.null(line) & is.null(size)) {
    ruleG = "rot_shade_line"
  } else if (is.null(rotation) & !is.null(shape) & !is.null(shade) & is.null(line) & is.null(size)) {
    ruleG = "shade"
  } else if(is.null(rotation) & !is.null(shape) & is.null(shade) & !is.null(line) & is.null(size)) {
    ruleG = "line"
  }  else if (!is.null(rotation) & !is.null(shade) & !is.null(line) & !is.null(shape) & !is.null(size)) {
    ruleG = "rot_shade_line_shape_size"
  } else if (is.null(rotation) & !is.null(shade) & is.null(line) & !is.null(shape) & !is.null(size)) {
    ruleG = "shade_shape_size"
  }
  return(ruleG)
}




getDone = function(object, 
                   rotation = c("htv", "vth", "dtv", "dth"), 
                   shade = c("wtg", "wtb", "btw", "btg"), 
                   line = c("sdad", "dads", "sdda", "ddas"), 
                   shape = c("ellipse", "triangle", "pentagon", "all"),
                   size = c("increasing", "decreasing")){
  the_rule = list()
  # empty rotation 
  empty_rot = matrix(pi/2, nrow = 3, ncol = 3)
  # empty shade 
  empty_shade = matrix(0, nrow = 3, ncol = 3)
  # empty line 
  empty_line = matrix(1, nrow = 3, ncol = 3)
  # empty size 
  empty_x_ell = matrix(10, nrow = 3, ncol = 3)
  empty_y_ell = matrix(15, nrow = 3, ncol = 3)
  empty_x = matrix(15, nrow = 3, ncol = 3)
  empty_y = matrix(15, nrow = 3, ncol = 3)
  
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
  
  # increasing size x elli 
  plus_x_elli = c(10, 15, 20)
  plus_y_elli = plus_x_elli/2
  
  # increasing other shapes 
  plus_x = c(10, 15, 20)
  plus_y = plus_x
  
  if (object == "rotation") {
    if (rotation == "htv") {
      start = list(rot = htv)
      
    } else if (rotation == "vth") {
      start = list(rot = htv)
    }
    rest = list(rotation = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                shade = empty_shade, 
                line = empty_line, 
                size.x.elli = empty_x_ell, 
                size.y.elli = empty_y_ell, 
                size.x = empty_x, 
                size.y = empty_y 
                )
  } else if (object == "shade") {
    if (shade == "wtb") {
      start = list(shade = wtb)
    } else if (shade == "wtg") {
      start = list(shade = wtg)
    }
    rest = list(rotation = empty_rot, 
                shade =Permn(start[["shade"]])[c(seq(1,5,by=2)), ], 
                line = empty_line, 
                size.x.elli = empty_x_ell, 
                size.y.elli = empty_y_ell, 
                size.x = empty_x, 
                size.y = empty_y )
  } else if (object == "line") {
    if (line == "sdad") {
      
      start = list(line = sdad)
    } else if (line == "dads") {
      start = list(line = dads)
    }
    rest = list(rotation = empty_rot, 
                shade = empty_shade, 
                line =Permn(start[["line"]])[c(seq(1,5,by=2)), ], 
                size.x.elli = empty_x_ell, 
                size.y.elli = empty_y_ell, 
                size.x = empty_x, 
                size.y = empty_y )
  }  else if (object == "rot_line") {
    if (rotation == "htv" & line == "sdad") { # htv and wtg ----
      start = list(rot = htv, 
                   line = sdad)
    } else if (rotation == "htv" & line == "dads") { # htv and wtg -----
      start = list(rot = htv, 
                   line = dads)
    }
    rest = list(rotation = Permn(start[["rot"]])[c(seq(1,5,by=2)), ], 
                shade = empty_shade, 
                line= Permn(start[["line"]])[c(seq(1,5,by=2)), ], 
                size.x.elli = empty_x_ell, 
                size.y.elli = empty_y_ell, 
                size.x = empty_x, 
                size.y = empty_y )
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
                line= Permn(start[["line"]])[c(seq(1,5,by=2)), ], 
                size.x.elli = empty_x_ell, 
                size.y.elli = empty_y_ell, 
                size.x = empty_x, 
                size.y = empty_y )
  } else if (object == "rot_shade_line") {
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
                line= Permn(start[["line"]])[c(seq(1,5,by=2)), ], 
                size.x.elli = empty_x_ell, 
                size.y.elli = empty_y_ell, 
                size.x = empty_x, 
                size.y = empty_y )
  }  else if (object == "shade_shape_size") {
    if (shade == "wtb" & size == "increasing") {
      start = list(rot = htv, 
                   shade = wtb, 
                   size.x.elli = plus_x_elli, 
                   size.y.elli = plus_y_elli, 
                   size.x = plus_x, 
                   size.y = plus_y)
    }
    rest = list(rotation = empty_rot, 
                shade = matrix(start[["shade"]],  ncol = 3, nrow = 3, byrow = T), 
                line = empty_line, 
                size.x.elli = matrix(plus_x_elli, ncol = 3, nrow = 3, byrow = T), 
                size.y.elli = matrix(plus_y_elli, ncol = 3, nrow = 3, byrow = T), 
                size.x = matrix(plus_x, ncol = 3, nrow = 3, byrow = T), 
                size.y = matrix(plus_y, ncol = 3, nrow = 3, byrow = T))
  } else if (object == "rot_shade") {
    if (rotation == "htv" & shade == "wtg") { # htv and wtg ----
      start = list(rot = htv,
                   shade = wtg, 
                   size.x.elli = empty_x_ell, 
                   size.y.elli = empty_y_ell)
    } else if (rotation == "htv" & shade == "wtb") { # htv and wtg -----
      start = list(rot = htv,
                   shade = wtb, 
                   size.x.elli = empty_x_ell, 
                   size.y.elli = empty_y_ell)
    }
    rest = list(rotation = Permn(start[["rot"]])[c(seq(1,5,by=2)), ],
                shade = Permn(start[["shade"]])[c(seq(1,5,by=2)), ],
                line= empty_line, 
                size.x.elli = empty_x_ell, 
                size.y.elli = empty_y_ell)
  }
  if (shape == "ellipse") {
    rest$shape = "ellipse"
  } else if (shape == "triangle") {
    rest$shape = "triangle" 
  } else if (shape == "pentagon") {
    rest$shape = "pentagon"
  } else if (shape == "all") {
    rest$shape = "all"
  }
  the_rule = rest
  return(the_rule)
}

getDone(highRule(shade = T, shape = T, size = T), 
        shade = "wtb", size = "increasing", shape = "all")

getDone(highRule(rotation = T, shape = T), rotation = "htv", shape = "ellipse")

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