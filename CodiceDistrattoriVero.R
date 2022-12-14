# codice distrattori questa volta sul serio -----

# decompone la matrice -----

split.mat = function(m, cell = NULL) {
  if (is.null(cell) == T) {
    m.start = correct(m)
  } else {
    cell = paste0("Sq", cell)
    m.start = m[[cell]]
  }
  
  
  index_elements<-which(m.start$visible==1 & unlist(lapply(m.start$num, all, 1))) 
  split.m <- vector("list", length(index_elements))
  
  if (length(index_elements) == 1) {
    split.m = m.start
  } else {
    for (i in 1:length(split.m)) {
      split.m[[i]] <- vector("list", length(m.start))
      for (j in 1:length(split.m[[i]])) {
        names(split.m)[i] = m.start$shape[index_elements[i]]
        attr(split.m[[i]], "class") = "field"
        split.m[[i]][[j]] = m.start[[j]][index_elements[i]]
        names(split.m[[i]])[j] = names(m.start)[j]
      }
    }
  }
  
  
  return(split.m)
}


# risposta corretta ---
correct = function(m) {
  correct = m$Sq9 
  return(correct)
}

# incomplete correlate -----

ic = function(m, 
              n.rule = 1, 
              which.element = NULL) {
  m.correct = correct(m)
  
  ##Inizio aggiunta brutta
  elements<-decof(m.correct)##il controllo con i tag è sbagliato $EVVIVA$!
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
  #unlist(lapply(m.correct$tag,function(x) any(x== "rotate"))) )
  
  if (length(index_elements) == 1) {
    ic.scale = size(m.correct, 3)
    ic.flip = rotation(m.correct, 3)
    
    if (any(grep("pie.4", m.correct$shape)) == T)  {
      random_shape = list(pie.2(), pie.2.inv())
      random_index = sample(1:2, 1)
      ic.inc = random_shape[[random_index]]
    } else if (any(grep("pie.2", m.correct$shape)) == T) {
      ic.inc = circle()
    } else {
      ic.inc = m.correct
    }
    
  } else {
    split.m = split.mat(m)
    
    if (is.null(which.element) == T) {
      ic.flip = rotation(split.m[[1]], 2)
      
      ic.scale = size(split.m[[1]], 2)
      
      new_index = sample(index_elements,1)
      ic.inc = hide(m.correct, new_index)
      for (i in 2:length(split.m)) {
        ic.flip = cof(ic.flip, split.m[[i]])
        ic.scale = cof(ic.scale, split.m[[i]])
      }
      
      
    } else {
      ic.flip = rotation(split.m[[which.element]], 2) 
      ic.scale = size(split.m[[which.element]], 2) 
      for (i in 1:length(which(names(split.m) != which.element))) {
        ic.flip = cof(split.m[[which(names(split.m) != which.element)[i]]], 
                      ic.flip)
        ic.scale = cof(split.m[[which(names(split.m) != which.element)[i]]], 
                       ic.scale)
      }
      
      ic.inc = hide(m.correct, 
                    index_elements[which(names(split.m) == which.element)])
    } 
    
  }
  # if(length(index_elements)==0)
  # {
  #   ic.inc$attention = "No object can rotate here!"
  # }else{
  #   new_index <-sample(index_elements,1)
  #   # new_obj <- rotation( elements[[new_index]], 2) # il numero sulla rotazione dipende dalla figura
  #   # ic.flip <- replace(ic.flip,new_index,new_obj)
  #   
  #}
  ##Fine aggiunta brutta
  
  ic.dist = list(ic.scale = ic.scale, 
                 ic.flip = ic.flip, 
                 ic.inc = ic.inc)
  return(ic.dist)
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
                   choose.start = 1, 
                   choose.fig = NULL) {
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
  
  if (is.null(choose.fig) == F) {
    shapes.in = choose.fig
  } else {
    shapes.in = shapes.in
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
                     main = NULL, single.print = F) {
  # dist.list = sample(dist.list)
  if (single.print == F) {
    if (n.resp ==8) {
      par(mfrow = c(2, 4), mar = c(0.5, 6, 0.5, 2) + .1, 
          mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) ) 
      
    } else if (n.resp == 10) {
      par(mfrow = c(2, 5), mar = c(0.5, 6, 0.5, 2) + .1, 
          mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )
    } else if (n.resp == 5) {
      par(mfrow =c(1, 5), mar = c(0.5, 6, 0.5, 2) + .1, 
          mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )
    } else if (n.resp == 1) {
      par(mfrow =c(1, 5), mar = c(0.5, 6, 0.5, 2) + .1, 
          mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )
    }
    
  } else if (single.print == T) {
    par(mfrow = c(1, 1), mar = c(0.5, 6, 0.5, 2) + .1, 
        mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )
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

