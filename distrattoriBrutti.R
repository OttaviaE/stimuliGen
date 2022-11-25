# distrattori ------

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")

# disttattori -----

correct = function(m) {
  correct = m$Sq9 
  return(correct)
}

repetition = function(m,which = "all") {
  m.correct = correct(m)
  distr.repetition = list(  r.top = m$Sq6,
                            r.diag = m$Sq5,
                            r.left = m$Sq8)
  if (any(unlist(distr.repetition$r.top) != unlist(m.correct), 
          na.rm = T) == F) {
    warning("R-Top is equal to the correct response")
    flag = "r.top"
  } else if (any(unlist(distr.repetition$r.left) != unlist(m.correct), na.rm = T) == F) {
    warning("R-left is equal to the correct response")
    flag = "r.left"
  } else if (any(unlist(distr.repetition$r.diag) != unlist(m.correct), 
                 na.rm = T) == F) {
    warning("R-diag is equal to the correct response")
    # distr.repetition = distr.repetition[[sample.index]]
    flag = "r.diag"
  }
  return(distr.repetition)
}


wp = function(m, which = "all") {
  sample.index = sample(c(1:4, 7)) # non è random
  s = sample.index[1]
  distr.wp.copy = m[[s]]
  distr.wp.matrix = m$Sq1
  for (i in 2:8) {
    distr.wp.matrix = cof(distr.wp.matrix, 
                          m[[i]])
  }
  distr.wp = list(wp.copy = distr.wp.copy, 
                  wp.matrix = distr.wp.matrix)
  
}


# difference -----
d.union = function(m, n = 1, 
                   shapes.out = NULL, 
                   shapes.in = NULL) {
  n = n
  d.union = m$Sq1
  for (i in 2:8) {
    d.union = cof(d.union, m[[i]])
  }
  shapes.l = shapes_list("Shapes_list-10-11-Ottavia.R")
  shapes.l = shapes.l[-grep("arc", shapes.l$name), ]
  if (is.null(shapes.out) == F) {
    exclusion = shapes.l[!shapes.l$name %in% c(unlist(d.union$shape), 
                                               shapes.out), ]  
  } else {
    exclusion = shapes.l[!shapes.l$name %in% unlist(d.union$shape), ]  
  } 
  if (is.null(shapes.in) == F) {
    selection = exclusion[exclusion$name %in% shapes.in, ]
    for (i in 1:nrow(selection)) {
      f = get(selection$name[i])
      if (i==1) {
        obj = f()
      } else {
        obj = cof(obj, f())
      }
    }
  } else {
    random<-sample(1:length(exclusion$name),n)
    for(i in 1:length(random)) {
      f<-get(exclusion$name[random[i]])
      if(i==1){
        obj<-f()
      }else{
        obj<-cof(obj,f())
      }
    }
  }
  
  d.union = cof(d.union, obj)
  return(d.union)
}
