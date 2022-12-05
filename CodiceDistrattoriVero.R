# codice distrattori questa volta sul serio -----

# decompone la matrice -----

split.mat = function(m) {
  m.correct = correct(m)
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1))) 
  split.m <- vector("list", length(index_elements))
  
  for (i in 1:length(split.m)) {
    split.m[[i]] <- vector("list", length(m.correct))
    for (j in 1:length(split.m[[i]])) {
      names(split.m)[i] = m.correct$shape[index_elements[i]]
      attr(split.m[[i]], "class") = "field"
      split.m[[i]][[j]] = m.correct[[j]][index_elements[i]]
      names(split.m[[i]])[j] = names(m.correct)[j]
    }
  }
  return(split.m)
}


# risposta corretta ---
correct = function(m) {
  correct = m$Sq9 
  return(correct)
}

# Repeteion -----

repetition = function(m) {
  m.correct = correct(m)
  distr.repetition = list(  r.top = m$Sq6,
                            r.diag = m$Sq5,
                            r.left = m$Sq8)
  if (any(unlist(distr.repetition$r.top) != unlist(m.correct),
          na.rm = T) == F) {
    warning("R-Top is equal to the correct response")
    flag = "r.top"
  } 
  if (any(unlist(distr.repetition$r.left) != unlist(m.correct),
          na.rm = T) == F) {
    warning("R-left is equal to the correct response")
    flag = "r.left"
  }
  if (any(unlist(distr.repetition$r.diag) != unlist(m.correct),
          na.rm = T) == F) {
    warning("R-diag is equal to the correct response")
    # distr.repetition = distr.repetition[[sample.index]]
    flag = "r.diag"
  }
  return(distr.repetition)
}

# Wrong principle -----

wp = function(m, choose.matrix = 1, choose.copy = NULL) {
  m.correct = correct(m)
  check.rep = repetition(m)
  if (is.null(choose.copy) == F) {
    distr.wp.copy = m[[choose.copy]]
  } else {
    sample.index = sample(c(2:4, 7)) # non è random
    s = sample(sample.index,1)
    distr.wp.copy = m[[s]]
  }
  
  if (is.null(choose.matrix) == F) {
    distr.wp.matrix = m[[choose.matrix]]
  } else {
    distr.wp.matrix = m[[1]]
  }
  which.vis = unlist(distr.wp.matrix$visible)
  p = which(which.vis == 0)
  
  if (length(p) == 0) {
    distr.wp.matrix = cof(distr.wp.matrix, 
                          size(m$Sq1, 3))
  } else {
    distr.wp.matrix = show(distr.wp.matrix, p[1])
  }
  
  
  if (any(unlist(distr.wp.copy) != unlist(m.correct),
          na.rm = T) == F) {
    warning("WP-Copy is equal to the correct response!")
    # if (s < 3) {
    #   distr.wp.copy = m[[s + 1]]
    # } else if ( s == 4) {
    #   distr.wp.copy = m[[s - 1]]
    # } else {
    #   distr.wp.copy = m[[s - 3]]
    # }
  }
  
  if (any(unlist(distr.wp.copy) != unlist(check.rep$r.top), 
          na.rm = T) == F) {
    warning("WP-Copy equal to r-top")
  }
  
  if (any(unlist(distr.wp.copy) != unlist(check.rep$r.diag), 
          na.rm = T) == F) {
    warning("WP-Copy equal to r-diag")
  }
  
  if (any(unlist(distr.wp.copy) != unlist(check.rep$r.left), 
          na.rm = T) == F) {
    warning("WP-Copy equal to r-left")
  }
  distr.wp = list(wp.copy = distr.wp.copy,
                  wp.matrix = distr.wp.matrix)
  
}

# difference -----
d.union = function(m,
                   choose.start = 1) {
  d.union = m[[choose.start]]
  
  shapes.l = shapes_list("Shapes_list-10-11-Ottavia.R")
  shapes.l = shapes.l[-c(grep("arc", shapes.l$name), 
                         grep("pie", shapes.l$name), 
                         grep("pacman", shapes.l$name), 
                         grep("semi.circle", shapes.l$name)), ]
  
  if (any(grep("semi.circle", (d.union$shape))) == T) {
    shapes.in = "pie.2"
  } else if (any(grep("pie.4", (d.union$shape))) == T) {
    shapes.in = "pacman"
  } else if (any(grep("pie.2", (d.union$shape))) == T | 
             any(grep("slice", (d.union$shape))) == T | 
             any(grep("pacman", (d.union$shape))) == T) {
    shapes.in = "pie.4"
  } else {
    random<-sample(1:length(shapes.l$name),1)
    shapes.in = shapes.l$name[random]
  }
  f = get(shapes.in)
  d.un = cof(d.union, f())
  return(d.un)
  
}


responses = function(m, n.rule = 1, 
                     choose.matrix = 1, 
                     choose.copy = 2, 
                     choose.start = 1, 
                     which.element = NULL) {
  m.correct = correct(m)
  resp = list(correct = m.correct, 
              r.top = m$Sq6,
              r.diag = m$Sq5,
              r.left = m$Sq8, 
              wp.copy = wp(m, choose.copy = choose.copy)$wp.copy, 
              wp.matrix = wp(m, choose.matrix = choose.matrix)$wp.matrix, 
              d.union = d.union(m, choose.start = choose.start), 
              ic.scale = ic(m, which.element = which.element)$ic.scale, 
              ic.flip = ic(m, which.element = which.element)$ic.flip, 
              ic.inc = ic(m, which.element = which.element)$ic.inc)
  
  # if (any(unlist(resp$r.top) != unlist(m.correct), 
  #         na.rm = T) == F) {
  #   resp$r.top$attention = "R-top equal correct"
  # } if (any(unlist(resp$r.left) != unlist(m.correct), 
  #                na.rm = T) == F) {
  #   resp$r.left$attention = "R-left equal correct"
  # } else if (any(unlist(resp$r.diag) != unlist(m.correct), 
  #                na.rm = T) == F) {
  #   resp$r.diag$attention = "R-diag equal correct"
  # }
  # 
  # 
  # if (any(unlist(resp$wp.copy) != unlist(m.correct), 
  #         na.rm = T) == F) {
  #   resp$wp.copy$attention = "WP-Copy equal correct"
  # }
  # 
  return(resp)
} 

# draw distractors -----
draw.dist = function(dist.list, n.resp = 8,
                     main = NULL) {
  if (n.resp ==8) {
    par(mfrow = c(2, 4)) 
    
  } else if (n.resp == 10) {
    par(mfrow = c(2, 5))
  } else if (n.resp == 5) {
    par(mfrow =c(1, 5))
  } else if (n.resp == 1) {
    par(mfrow =c(1, 5))
  }
  
  if (is.null(main) == F) {
    for (i in 1:length(dist.list)) {
      draw(dist.list[[i]], main = names(dist.list)[i])
    }
  } else {
    for (i in 1:length(dist.list)) {
      draw(dist.list[[i]])
    }
  }
}