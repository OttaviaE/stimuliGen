# codice distrattori questa volta sul serio -----
# mi serve per fare i distrattori dei cosi quantitativi
temp.dice = function(object){
  object<-movement(object,2,"pos",-9,9) 
  object2<-object
  for(row in 1:3) {
    obj<-movement(object,row,"y")
    if(row>1)
    {
      object2<-cof(object2,obj)
    }
    
    for( col in 2:3 )
    {
      obj<-movement(obj,2,"x")
      object2<-cof(object2,obj)
      
    }
  }
  return(object2)
}
# decompone la matrice -----

# non lo so vorrei morire il codice non funziona con le matrici di massimiliano dc
split.mat = function(m, cell = NULL, vis = NULL, mat.type = 9) {
  if (is.null(cell) == T) {
    m.start = correct(m, mat.type = mat.type)
  } else {
    cell = paste0("Sq", cell)
    m.start = m[[cell]]
  }
  
  if(is.null(vis) == T) {
    index_elements<-which(m.start$visible==1 & unlist(lapply(m.start$num, all, 1))) 
  } else {
    index_elements = 1:length(m.start$shape)
  }
  
  split.m <- vector("list", length(index_elements))
  for (i in 1:length(split.m)) {
    split.m[[i]] <- vector("list", length(m.start))
    for (j in 1:length(split.m[[i]])) {
      names(split.m)[i] = m.start$shape[index_elements[i]]
      attr(split.m[[i]], "class") = "field"
      split.m[[i]][[j]] = m.start[[j]][index_elements[i]]
      names(split.m[[i]])[j] = names(m.start)[j]
      split.m[[i]][j]$visible = 1
    }
  }
  
  
  return(split.m)
}

# split.mat = function(m, cell = NULL, vis = NULL, mat.type = 9) {
#   if (is.null(cell) == T) {
#     m.start = correct(m, mat.type = 9)
#   } else {
#     cell = paste0("Sq", cell)
#     m.start = m[[cell]]
#   }
# 
#   if(is.null(vis) == T) {
#     index_elements<-which(m.start$visible==1 & unlist(lapply(m.start$num, all, 1)))
#   } else {
#     index_elements = 1:length(m.start$shape)
#   }
# 
#   if (length(index_elements) > 1 ) {
#     split.m <- vector("list", length(index_elements))
#     for (i in 1:length(split.m)) {
#       split.m[[i]] <- vector("list", length(m.start))
#       for (j in 1:length(split.m[[i]])) {
#         names(split.m)[i] = m.start$shape[index_elements[i]]
#         attr(split.m[[i]], "class") = "field"
#         split.m[[i]][[j]] = m.start[[j]][index_elements[i]]
#         names(split.m[[i]])[j] = names(m.start)[j]
#         split.m[[i]][j]$visible = 1
#       }
#     }
#   }
# 
# 
#   return(split.m)
# }

# split.mat = function(m, cell = NULL, vis = NULL) {
#   if (is.null(cell) == T) {
#     m.start = correct(m)
#   } else {
#     cell = paste0("Sq", cell)
#     m.start = m[[cell]]
#   }
#   
#   if(is.null(vis) == T) {
#     index_elements<-which(m.start$visible==1 & unlist(lapply(m.start$num, all, 1))) 
#   } else {
#     index_elements = 1:length(m.start$shape)
#   }
#   
#   if (length(index_elements) > 1 ) {
#     split.m <- vector("list", length(index_elements))
#     for (i in 1:length(split.m)) {
#       split.m[[i]] <- vector("list", length(m.start))
#       for (j in 1:length(split.m[[i]])) {
#         names(split.m)[i] = m.start$shape[index_elements[i]]
#         attr(split.m[[i]], "class") = "field"
#         split.m[[i]][[j]] = m.start[[j]][index_elements[i]]
#         names(split.m[[i]])[j] = names(m.start)[j]
#         split.m[[i]][j]$visible = 1
#       }
#     }
#   } else {
#     split.m = m.start
#   }
#   
#   
#   return(split.m)
# }

# risposta corretta ---
correct = function(m, mat.type = 9) {
  if (mat.type == 9) {
    correct = m$Sq9 
  } else {
    correct = m$Sq5
  }
  
  return(correct)
}

# incomplete correlate -----

# ic = function(m, 
#               n.rule = 1, 
#               which.element = NULL, mat.type = 9) {
#   m.correct = correct(m, mat.type = mat.type)
#   
#   ##Inizio aggiunta brutta
#   elements<-decof(m.correct)##il controllo con i tag è sbagliato $EVVIVA$!
#    # bisogna trovare un modo alternativo per trovare l'indice degli elementi 
#   # perché non possiamo avere una funzione che sputa fuori 50 wanrings
#   index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
#   #unlist(lapply(m.correct$tag,function(x) any(x== "rotate"))) )
#   
#   if (length(index_elements) == 1) {
#     ic.scale = size(m.correct, 3)
#     ic.flip = rotation(m.correct, 2)
#     
#     if (any(grep("pie.4", m.correct$shape)) == T)  {
#       random_shape = list(pie.2(), pie.2.inv())
#       random_index = sample(1:2, 1)
#       ic.inc = random_shape[[random_index]]
#     } else if (any(grep("pie.2", m.correct$shape)) == T) {
#       ic.inc = circle()
#     } else if (any(grep("square", m.correct$shape)) == T) {
#       ic.inc = cof(vline(pos.x =-m.correct$size.x[[1]], s.x = m.correct$size.x[[1]]), 
#                    hline(pos.y = -m.correct$size.x[[1]], s.x = m.correct$size.x[[1]]), 
#                    vline(pos.x =m.correct$size.x[[1]], s.x = m.correct$size.x[[1]]))
#     } else {
#       ic.inc = m.correct
#     }
#     m.c = m.correct
#     if (any(unlist(m.c$shade == "black"), na.rm = T) | any(grep("line", unlist(m.c$shade)), na.rm = T) == T) {
#       m.c$shade[[1]] = rep("white", 
#                                  length(any(unlist(m.c$shade == "black"))))
#     } else if (any(unlist(m.c$shade == "white")) == T) {
#       m.c$shade[[1]] = rep("black", 
#                                  length(any(unlist(m.c$shade == "white"))))
#     } else if(is.na(any(unlist(m.c$shade))) == T) {
#       m.c$shade[[1]] = rep("black", 
#                                  length(is.na(any(unlist(m.c$shade)))))
#     } else if (any(grep("line", unlist(m5$Sq9$shade)) == T) == T) {
#       m.c$shade[[1]] = rep("white", 
#                            length(is.na(any(unlist(m.c$shade)))))
#     }
#     ic.col = m.c
#     
#   } else {
#     split.m = split.mat(m)
#     
#     
#     
#     if (is.null(which.element) == T) {
#       ic.flip = rotation(split.m[[1]], 2)
#       
#       ic.scale = size(split.m[[1]], 2)
#       
#       new_index = sample(index_elements,1)
#       ic.inc = hide(m.correct, new_index)
#       for (i in 2:length(split.m)) {
#         ic.flip = cof(ic.flip, split.m[[i]])
#         ic.scale = cof(ic.scale, split.m[[i]])
#       }
#       
#     } else {
#       ic.flip = rotation(split.m[[which.element]], 2) 
#       ic.scale = size(split.m[[which.element]], 2) 
#       for (i in 1:length(which(names(split.m) != which.element))) {
#         ic.flip = cof(split.m[[which(names(split.m) != which.element)[i]]], 
#                       ic.flip)
#         ic.scale = cof(split.m[[which(names(split.m) != which.element)[i]]], 
#                        ic.scale)
#       }
#       
#       ic.inc = hide(m.correct, 
#                     index_elements[which(names(split.m) == which.element)])
#       
#     
#     } 
#       for (i in 1:length(split.m)) {
#     if (is.na(split.m[[i]]$shade[[1]]) == T) {
#       split.m[[i]]$shade[[1]] = "black"
#     } else if (split.m[[i]]$shade[[1]] == "grey") {
#       split.m[[i]]$shade[[1]] = "white"
#     } else if(split.m[[i]]$shade[[1]] == "white") {
#       split.m[[i]]$shade[[1]] = "black"
#     }  else if(split.m[[i]]$shade[[1]] == "black") {
#       split.m[[i]]$shade[[1]] = "white"
#     }
#   }
#   ic.col = split.m[[1]]
#   for (i in 2:length(split.m)) {
#     ic.col = cof(split.m[[i]],ic.col)
#   }
#   }
#   
# 
#   # if(length(index_elements)==0)
#   # {
#   #   ic.inc$attention = "No object can rotate here!"
#   # }else{
#   #   new_index <-sample(index_elements,1)
#   #   # new_obj <- rotation( elements[[new_index]], 2) # il numero sulla rotazione dipende dalla figura
#   #   # ic.flip <- replace(ic.flip,new_index,new_obj)
#   #   
#   #}
#   ##Fine aggiunta brutta
#   
#   ic.dist = list(ic.scale = ic.scale, 
#                  ic.flip = ic.flip, 
#                  ic.inc = ic.inc, 
#                  ic.neg = ic.col)
#   return(ic.dist)
# }

# ic size -----
ic.scale = function(m, 
                    which.element = NULL, 
                    mat.type = 9, 
                    how.small = 2) {
  m.correct = correct(m, mat.type = mat.type)
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
  
  if (length(index_elements) == 1) {
    ic.scale = size(m.correct, how.small)
  } else {
    split.m = split.mat(m, mat.type = mat.type)
    
    if (is.null(which.element) == T) {
      ic.temp = size(split.m[[length(split.m)]], how.small) # perché funzioni l'utlimo riempimento va messo per ultimo
      ic.scale = replace(m.correct, index_elements[length(split.m)], ic.temp)
      
    } else {
      ic.scale = size(split.m[[which.element]], how.small) 
      for (i in 1:length(which(names(split.m) != which.element))) {
        ic.scale = cof(split.m[[which(names(split.m) != which.element)[i]]], 
                       ic.scale)
      }
    }
    rule.mat = c(m$hrule, m$vrule)
    if ((any(rule.mat == "AND") || any(rule.mat == "OR") || any(rule.mat == "XOR"))) {
      if (any(grep("lily", names(split.m))) | any(grep("s.", names(split.m))) | any(grep("bow.tie", names(split.m)))) {
        ic.scale = size(split.m[[1]], how.small)
        
        new_index = sample(index_elements,1)
        ic.inc = hide(m.correct, new_index)
        for (i in 2:length(split.m)) {
          ic.scale = cof(ic.scale, split.m[[i]])
        }
      } else {
        ic.temp = size(split.m[[length(split.m)]], how.small) # perché funzioni l'utlimo riempimento va messo per ultimo
        ic.scale = replace(m.correct, index_elements[length(split.m)], ic.temp)
      }
      
    }
    if(any(grepl("quant", rule.mat)) == T) {
      split.m = split.mat(m)
      ic.scale = size(split.m[[1]], how.small)
      for (i in 2:length(split.m)) {
        ic.scale = cof(ic.scale, size(split.m[[i]], how.small))
      }
    }
  }
  ic.size = ic.scale 
  return(ic.size)
}


# ic flip ----- 
ic.flip = function(m, 
                   which.element = NULL, 
                   mat.type = 9, 
                   how.rot = 2) {
  m.correct = correct(m, mat.type = mat.type)
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
  
  if (length(index_elements) == 1) {
    ic.rotation = rotation(m.correct, how.rot)
  } else {
    split.m = split.mat(m, mat.type = mat.type)
    
    if (is.null(which.element) == T) {
      
      ic.temp = rotation(split.m[[length(split.m)]], how.rot) # perché funzioni l'utlimo riempimento va messo per ultimo
      ic.rotation = replace(m.correct, index_elements[length(split.m)], ic.temp)
      
      if (any(grepl("circle", names(split.m))) & length(index_elements) == 2) {
        ic.rotation = rotation(split.m[[names(split.m)[names(split.m) != "circle"]]], 
                               how.rot)
        temp = ic.rotation$shape
      
          ic.rotation = cof(ic.rotation, 
                            split.m[[names(split.m)[names(split.m) != temp]]])
        
      } else if (any(grepl("circle", names(split.m))) & length(index_elements) > 2) {
        
        temp = split.m[[1]]
        for(i in 2:length(split.m)) {
        temp = cof(temp, split.m[[i]])
        }
        
        ic.rotation = rotation(temp, how.rot)
      }
      
    } else {
      ic.rotation = rotation(split.m[[which.element]], how.rot) 
      for (i in 1:length(which(names(split.m) != which.element))) {
        ic.rotation = cof(split.m[[which(names(split.m) != which.element)[i]]], 
                          ic.rotation)
      }
    }
    rule.mat = c(m$hrule, m$vrule)
    if ((any(rule.mat == "AND") || any(rule.mat == "OR") || any(rule.mat == "XOR"))) {
      if (any(grep("lily", names(split.m))) | any(grep("s.", names(split.m))) | any(grep("bow.tie", names(split.m)))) {
        ic.rotation = rotation(split.m[[1]], how.rot)
        
        new_index = sample(index_elements,1)
        ic.inc = hide(m.correct, new_index)
        for (i in 2:length(split.m)) {
          ic.rotation = cof(ic.rotation, split.m[[i]])
        }
      } else {
        ap = split.m[[1]]
        
        #for (i in 2:length(p)) { ### DA DOVE SALTA FUORI P????
         for (i in 2:length(split.m)) {  
          ap = cof(ap, split.m[[i]])
        }
        ic.rotation = rotation(ap, how.rot)
      }
      
    }
    if(any(grepl("quant", rule.mat)) == T) {
      split.m = split.mat(m)
      ic.rotation = rotation(split.m[[1]], how.rot)
      for (i in 2:length(split.m)) {
        ic.rotation = cof(ic.rotation, rotation(split.m[[i]], how.rot))
      }
    }
  }
  ic.rotate = ic.rotation 
  return(ic.rotate)
}

# ic inc ----- 
ic.inc = function(m, which.element = NULL, 
                  mat.type = 9) {
  m.correct = correct(m, mat.type = mat.type)
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
  
  if (length(index_elements) == 1) {
    if (any(grep("pie.4", m.correct$shape)) == T)  {
      random_shape = list(pie.2(), pie.2.inv())
      random_index = sample(1:2, 1)
      ic.inc = random_shape[[random_index]]
    } else if (any(grep("pie.2", m.correct$shape)) == T | m.correct$shape[index_elements] == "pacman") {
      ic.inc = circle()
    } else if (m.correct$shape[index_elements] == "square") {
      ic.inc = cof(vline(pos.x =-m.correct$size.x[[1]], s.x = m.correct$size.x[[1]]), 
                   hline(pos.y = -m.correct$size.x[[1]], s.x = m.correct$size.x[[1]]), 
                   vline(pos.x =m.correct$size.x[[1]], s.x = m.correct$size.x[[1]]))
    } else if(any(grepl("line", unlist(m.correct$shade))) == T) {
      ic.inc = m.correct
      ic.inc$shade[[1]] = "white"
      ic.inc$lty[[1]] = m.correct$lty[[1]]  
    } else if(grepl("bow",  m.correct$shape[index_elements] ) == T) {
      ic.inc = triangle(rot = pi/2)
      ic.inc$pos.y[[1]] = -10 
      ic.inc$size.x[[1]] = bow.tie()$size.x[[1]]
      ic.inc$size.y[[1]] = bow.tie()$size.y[[1]]
    }else {
      ic.inc = m.correct
    }
  } else {
    split.m = split.mat(m, mat.type = mat.type)
    if (is.null(which.element) == T) {
      ic.inc = hide(m.correct, index_elements[length(split.m)])
    } else {
      ic.inc = hide(m.correct, 
                    index_elements[which(names(split.m) == which.element)])
      
    }
  }
  
  rule.mat = c(m$vrule, m$hrule) 
  if(any(grepl("quant", rule.mat)) == T) {
    ic.inc = (split.m[[2]])
    for (i in 2:length(split.m)) {
      ic.inc = cof(ic.inc, (split.m[[i]]))
    }
  } 
  return(ic.inc)
  
}

# ic neg -----

# ic.neg = function(m, which.element = NULL, 
#                   mat.type = 9) {
#   m.correct = correct(m, mat.type = mat.type)
#   index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
# 
#   split.m = split.mat(m, mat.type = mat.type)
# 
#   if (length(index_elements) == 1 & length(split.m) != 1) {
#     m.c = m.correct
#     if (any(unlist(m.c$shade == "black"), na.rm = T) | any(grepl("line", unlist(m.c$shade)), na.rm = T) == T) {
#       m.c$shade[[1]] = "white"
#     } else if (any(unlist(m.c$shade == "white")) == T) {
#       m.c$shade[[1]] = rep("black", 
#                                   length(any(unlist(m.c$shade == "white"))))
#     } else if(is.na(any(unlist(m.c$shade))) == T) {
#       m.c$shade[[1]] = rep("black", 
#                                   length(is.na(any(unlist(m.c$shade)))))
#     } else if (any(grep("line", unlist(m5$Sq9$shade)) == T) == T) {
#       m.c$shade[[1]] = rep("white", 
#                                   length(is.na(any(unlist(m.c$shade)))))
#     }
#     ic.col = m.c
#   } else if (length(index_elements) == 1 & length(split.m) == 1) {
#     
#     ic.col = split.m[[1]]
#     if (is.na(ic.col$shade[[1]][1]) == T ) {
#       ic.col$shade[[1]] = "black"
#     } else if (ic.col$shade[[1]] == "white") {
#       ic.col$shade[[1]] = "black"
#     } else if (ic.col$shade[[1]] == "black") {
#       ic.col$shade[[1]] = "white"
#     } else if (ic.col$shade[[1]] == "grey") {
#       ic.col$shade[[1]] = "white"
#     } else if ( any(grepl("line", unlist(ic.col$shade)), na.rm = T) == T) {
#       ic.col$shade[[1]] = "black"
#     }
#   } else {
#     if (is.null(which.element) == T & length(split.m) != 1) {
#       new_index = sample(names(split.m),1)
#      ic.temp = hide(m.correct, which(m.correct$shape == new_index))
#       
#       if (is.na(split.m[[new_index]]$shade[[1]][1]) == T ) {
#         split.m[[new_index]]$shade[[1]] = "black"
#       } else if (split.m[[new_index]]$shade[[1]][1] == "white") {
#         split.m[[new_index]]$shade[[1]] = "black"
#       } else if (split.m[[new_index]]$shade[[1]][1] == "black") {
#         split.m[[new_index]]$shade[[1]] = "white"
#       } else if (split.m[[new_index]]$shade[[1]][1] == "grey") {
#         split.m[[new_index]]$shade[[1]] = "white"
#       }
#       ic.col = cof(ic.temp, split.m[[new_index]])
#     } else if (is.null(which.element) == F & length(split.m) != 1){
#       if (is.na(split.m[[which.element]]$shade[[1]]) == T ) {
#         split.m[[which.element]]$shade[[1]] = "black"
#       } else if (split.m[[which.element]]$shade[[1]] == "white") {
#         split.m[[which.element]]$shade[[1]] = "black"
#       } else if (split.m[[which.element]]$shade[[1]] == "black") {
#         split.m[[which.element]]$shade[[1]] = "white"
#       } else if (split.m[[which.element]]$shade[[1]] == "grey") {
#         split.m[[which.element]]$shade[[1]] = "white"
#       }
#       ic.temp = split.m[[which.element]]
#       for (i in 1:length(which(names(split.m) != which.element))) {
#         ic.temp = cof(split.m[[which(names(split.m) != which.element)[i]]],
#                       ic.temp)
#       }
#       ic.col = ic.temp
#     }
#     
#     
#   }
#   return(ic.col)
# }
## Ic neg prova 1 ----
# ic.neg = function(m, mat.type = 9) {
#   m.correct = correct(m, mat.type = mat.type)
#   index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
#   
#   split.m = split.mat(m, mat.type = mat.type)
#   
#   ic.temp = split.m[[length(split.m)]]
#   
#   for (i in 1:length(ic.temp$shade)) {
#     if(is.na(ic.temp$shade[[i]][1]) == T) {
#       ic.temp$shade[[i]][1] = "black"
#     } else if (ic.temp$shade[[i]][1] == "white") {
#       ic.temp$shade[[i]][1] = "black"
#     } else if (ic.temp$shade[[i]][1] == "grey") {
#       ic.temp$shade[[i]][1] = "black"
#     } else {
#       ic.temp$shade[[i]][1] = "white"
#     }
#   }
#   
#   
#   ic.neg = replace(m.correct, 
#                    index_elements[length(split.m)], 
#                    ic.temp)
#   return(ic.neg)
# }
## ic neg prova 2 .-----
change.col = function(obj) {
  for(i in 1:length(obj$shade)) {
    if(is.na(obj$shade[[i]][1]) == T) {
      obj$shade[[i]][1] = "black"
    } else if (obj$shade[[i]][1] == "white") {
      obj$shade[[i]][1] = "black"
    } else if (obj$shade[[i]][1] == "grey") {
      obj$shade[[i]][1] = "white"
    } else if(obj$shade[[i]][1] == "black") {
      obj$shade[[i]][1] = "white"
    }
    else {
      obj$shade[[i]][1] = "black"
    }
  }
  return(obj)
}

ic.neg = function(m, mat.type = 9) {
  m.correct = correct(m, mat.type = mat.type)
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
  
  split.m = split.mat(m, mat.type = mat.type)
  
  ic.temp = split.m[[length(split.m)]]
  
  ic.temp = change.col(ic.temp)
  
  rule.mat = c(m$vrule, m$hrule) 
  if(any(grepl("quant", rule.mat)) == T) {
    ic.neg = change.col(split.m[[1]])
    for (i in 2:length(split.m)) {
      ic.neg = cof(ic.neg, change.col(split.m[[i]]))
    }
  } else {
    ic.neg = replace(m.correct, 
                     index_elements[length(split.m)], 
                     ic.temp)
  }
 
  return(ic.neg)
}


# ic ---- 
ic = function(m,which.element = NULL, 
              mat.type = 9, 
              how.small = 2, how.rot = 2) {
  ic.dist = list(ic.scale = ic.scale(m, 
                                     which.element = which.element, 
                                     mat.type = mat.type, 
                                     how.small = how.small), 
                 ic.flip = ic.flip(m, 
                                   which.element = which.element, 
                                   mat.type = mat.type, 
                                   how.rot = how.rot), 
                 ic.neg = ic.neg(m,
                                 mat.type = mat.type), 
                 ic.inc = ic.inc(m, 
                                 which.element = which.element, 
                                 mat.type = mat.type))
  return(ic.dist)
}



# Repetition -----

repetition = function(m, mat.type = 9) {
  m.correct = correct(m, mat.type = mat.type)
  
  if (mat.type == 9) {
    
    distr.repetition = list(  r.top = m$Sq6,
                              r.diag = m$Sq5,
                              r.left = m$Sq8)
  } else {
    
    distr.repetition = list(  r.top = m$Sq2,
                              r.diag = m$Sq1,
                              r.left = m$Sq4)
  }
  
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

wp = function(m, choose.matrix = 1, choose.copy = NULL, mat.type = 9) {
  m.correct = correct(m, mat.type = mat.type)
  check.rep = repetition(m)
  if (is.null(choose.copy) == F) {
    distr.wp.copy = m[[choose.copy]]
  } else {
    if (mat.type == 9) {
      sample.index = c(1:3)
    } else {
      sample.index = (c(2:4, 7))  
    }
     
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
  
  rule.mat = c(m$vrule, m$hrule) 
  if(any(grepl("quant", rule.mat)) == T) {
    split.m = split.mat(m)
    temp = change.col(split.m[[1]])
    temp = rotation(temp, 2)
    temp = temp.dice(temp)
    distr.wp.matrix = temp
    
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
                         grep("semi.circle", shapes.l$name), 
                         grep("star", shapes.l$name)), ]
  
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
  
  rule.mat = c(m$vrule, m$hrule) 
  if(any(grepl("quant", rule.mat)) == T) {
    rm(d.un)
    # split.m = split.mat(m, cell = 2)
    # g = get(names(split.m)[[1]])
    # h = rotation(size(g(), 2), 2)
   # temp2 = temp.dice(size(f(), 2))
    temp2 = size(f(), 3)
  #  temp2 = change.col(temp2)
    temp2 = temp.dice(temp2)
    d.un = cof(temp2, rectangle(s.x = 15, s.y = 18, 
                                pos.y = -1, 
                                pos.x = 0))
    
  }
  

  return(d.un)
  
}


responses = function(m, 
                     choose.matrix = 1, 
                     choose.copy = 2, 
                     choose.start = 1, 
                     which.element = NULL, 
                     how.small = 2, how.rot = 2,
                     choose.fig = NULL, 
                     mat.type = 9) {
  m.correct = correct(m, mat.type = mat.type)
  resp = list(correct = m.correct, 
              r.top = repetition(m, mat.type = mat.type)$r.top,
              r.diag = repetition(m, mat.type = mat.type)$r.diag,
              r.left = repetition(m, mat.type = mat.type)$r.left, 
              wp.copy = wp(m, choose.copy = choose.copy)$wp.copy, 
              wp.matrix = wp(m, choose.matrix = choose.matrix)$wp.matrix, 
              d.union = d.union(m, choose.start = choose.start, choose.fig = choose.fig), 
              ic.scale = ic(m, which.element = which.element, mat.type = mat.type, how.small = how.small)$ic.scale, 
              ic.flip = ic(m, which.element = which.element, mat.type = mat.type, how.rot = how.rot)$ic.flip, 
              ic.inc = ic(m, which.element = which.element, mat.type = mat.type)$ic.inc, 
              ic.neg = ic(m, which.element = which.element, mat.type = mat.type)$ic.neg)
  
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
draw.dist = function(dist.list, n.resp = 11,
                     main = F, single.print = F) {
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
    } else if (n.resp == 11) {
      par(mfrow =c(2, 6), mar = c(0.5, 6, 0.5, 2) + .1, 
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

