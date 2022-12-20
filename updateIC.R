M33<-apply(Raven(cof(square(),lily()),"OR","XOR"))
draw(M33)

dist_m33 = responses(M33)

draw.dist(dist_m33)


m = M33

(any(m$hrule == "AND") || any(m$hrule == "OR") || any(m$hrule == "XOR")) 

(any(m$vrule == "AND") || any(m$vrule == "OR") || any(m$vrule == "XOR")) 

p = split.mat(m)

ap = p[[1]]

for (i in 2:length(p)) {
  ap = cof(ap, p[[i]])
}
  

draw(rotation(ap, 2))
p = cof(m$Sq9)

split.m = split.mat(m)

if (any(grep("lily", names(split.m))) | any(grep("s.", names(split.m))) | any(grep("bow.tie", names(split.m)))) {
  ic.flip = rotation(split.m[[1]], 2)
  
  ic.scale = size(split.m[[1]], 2)
  ic.inc = hide(m.correct, 2)
} else {
  if ((any(m$hrule == "AND") || any(m$hrule == "OR") || any(m$hrule == "XOR")) )
}



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

m.start = correct(M1)
m1 = correct(m_VertShSi_01)
split.mat(m_VertShSi_01)
responses(m_VertShSi_01)

index_elements<-which(m.start$visible==1 & unlist(lapply(m.start$num, all, 1))) 
index_elements<-which(m1$visible==1 & unlist(lapply(m1$num, all, 1))) 


p= m_VertShSi_01
mp = correct(p)

mp = split.mat(p)
ic.scale(p)


responses(p)

ic.scale = function(m, 
                    which.element = NULL, 
                    mat.type = 9, 
                    how.small = 2) {
  m.correct = correct(m, mat.type = mat.type)
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
  
  if (length(index_elements) == 1) {
    ic.scale = size(m.correct, how.small)
  } else {
    split.m = split.mat(m)
    
    if (is.null(which.element) == T) {
      
      ic.scale = size(split.m[[1]], how.small)
      
      new_index = sample(index_elements,1)
      ic.inc = hide(m.correct, new_index)
      for (i in 2:length(split.m)) {
        ic.scale = cof(ic.scale, split.m[[i]])
      }
      
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
        ap = split.m[[1]]
        
        for (i in 2:length(p)) {
          ap = cof(ap, split.m[[i]])
        }
        ic.scale = size(ap, how.small)
      }
      
    }
  }
  ic.size = ic.scale 
  return(ic.size)
}


draw(ic.scale(M1, how.small = 1))
draw(ic.scale(M33))


ic.flip = function(m, 
                       which.element = NULL, 
                       mat.type = 9, 
                       how.rot = 2) {
  m.correct = correct(m, mat.type = mat.type)
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
  
  if (length(index_elements) == 1) {
    ic.rotation = rotation(m.correct, how.rot)
  } else {
    split.m = split.mat(m)
    
    if (is.null(which.element) == T) {
      
      ic.rotation = rotation(split.m[[1]], how.rot)
      
      new_index = sample(index_elements,1)
      ic.inc = hide(m.correct, new_index)
      for (i in 2:length(split.m)) {
        ic.rotation = cof(ic.rotation, split.m[[i]])
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
        
        for (i in 2:length(p)) {
          ap = cof(ap, split.m[[i]])
        }
        ic.rotation = rotation(ap, how.rot)
      }
      
    }
  }
  ic.rotate = ic.rotation 
  return(ic.rotate)
}

draw(ic.flip(M1, how.rot = 3))
draw(ic.flip(M33))
a = ic.flip(M33)
draw(a)


ic.inc = function(m, which.element = NULL, 
                  mat.type = 9) {
  m.correct = correct(m, mat.type = mat.type)
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
  
  if (length(index_elements) == 1) {
    if (any(grep("pie.4", m.correct$shape)) == T)  {
      random_shape = list(pie.2(), pie.2.inv())
      random_index = sample(1:2, 1)
      ic.inc = random_shape[[random_index]]
    } else if (any(grep("pie.2", m.correct$shape)) == T) {
      ic.inc = circle()
    } else if (any(grep("square", m.correct$shape)) == T) {
      ic.inc = cof(vline(pos.x =-m.correct$size.x[[1]], s.x = m.correct$size.x[[1]]), 
                   hline(pos.y = -m.correct$size.x[[1]], s.x = m.correct$size.x[[1]]), 
                   vline(pos.x =m.correct$size.x[[1]], s.x = m.correct$size.x[[1]]))
    } else {
      ic.inc = m.correct
    }
  } else {
    split.m = split.mat(m)
    if (is.null(which.element) == T) {
      new_index = sample(index_elements,1)
      ic.inc = hide(m.correct, new_index)
    } else {
      ic.inc = hide(m.correct, 
                    index_elements[which(names(split.m) == which.element)])
      
    }
  }
  return(ic.inc)
  
}


ic.neg = function(m, which.element = NULL, 
                  mat.type = 9) {
  m.correct = correct(m, mat.type = mat.type)
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
  split.m = split.mat(m)
  
  if (length(index_elements) == 1 & length(split.m) != 1) {
    m.c = m.correct
    if (any(unlist(m.c$shade == "black"), na.rm = T) | any(grep("line", unlist(m.c$shade)), na.rm = T) == T) {
      m.c$shade[[1]] = rep("white", 
                                  length(any(unlist(m.c$shade == "black"))))
    } else if (any(unlist(m.c$shade == "white")) == T) {
      m.c$shade[[1]] = rep("black", 
                                  length(any(unlist(m.c$shade == "white"))))
    } else if(is.na(any(unlist(m.c$shade))) == T) {
      m.c$shade[[1]] = rep("black", 
                                  length(is.na(any(unlist(m.c$shade)))))
    } else if (any(grep("line", unlist(m5$Sq9$shade)) == T) == T) {
      m.c$shade[[1]] = rep("white", 
                                  length(is.na(any(unlist(m.c$shade)))))
    }
    ic.col = m.c
  } else if (length(index_elements) == 1 & length(split.m) == 1) {

      ic.col = split.m[[1]]
      if (is.na(ic.col$shade[[1]]) == T ) {
        ic.col$shade[[1]] = "black"
      } else if (ic.col$shade[[1]] == "white") {
        ic.col$shade[[1]] = "black"
      } else if (ic.col$shade[[1]] == "black") {
        ic.col$shade[[1]] = "white"
      } else if (ic.col$shade[[1]] == "grey") {
        ic.col$shade[[1]] = "white"
      }
  } else {
    if (is.null(which.element) == T & length(split.m) != 1) {
      new_index = sample(index_elements,1)
      ic.temp = hide(m.correct, new_index)

      if (is.na(split.m[[new_index]]$shade[[1]][1]) == T ) {
        split.m[[new_index]]$shade[[1]] = "black"
      } else if (split.m[[new_index]]$shade[[1]] == "white") {
        split.m[[new_index]]$shade[[1]] = "black"
      } else if (split.m[[new_index]]$shade[[1]] == "black") {
        split.m[[new_index]]$shade[[1]] = "white"
      } else if (split.m[[new_index]]$shade[[1]] == "grey") {
        split.m[[new_index]]$shade[[1]] = "white"
      }
      ic.col = cof(ic.temp, split.m[[new_index]])
    } else if (is.null(which.element) == F & length(split.m) != 1){
      if (is.na(split.m[[which.element]]$shade[[1]]) == T ) {
        split.m[[which.element]]$shade[[1]] = "black"
      } else if (split.m[[which.element]]$shade[[1]] == "white") {
        split.m[[which.element]]$shade[[1]] = "black"
      } else if (split.m[[which.element]]$shade[[1]] == "black") {
        split.m[[which.element]]$shade[[1]] = "white"
      } else if (split.m[[which.element]]$shade[[1]] == "grey") {
        split.m[[which.element]]$shade[[1]] = "white"
      }
      ic.temp = split.m[[which.element]]
      for (i in 1:length(which(names(split.m) != which.element))) {
        ic.temp = cof(split.m[[which(names(split.m) != which.element)[i]]],
                      ic.temp)
      }
      ic.col = ic.temp
    }


  }
  return(ic.col)
}

draw(ic.neg(M33, which.element = "square"))
draw(ic.neg(M1))



# provo a metterle tutte insieme 

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
                                 which.element = which.element, 
                                 mat.type = mat.type), 
                 ic.inc = ic.inc(m, 
                                 which.element = which.element, 
                                 mat.type = mat.type))
  return(ic.dist)
}

p1 = responses(M33, choose.matrix = 5)
draw(M33)
draw.dist(p1,n.resp = 11, main = T )

p = ic(M33)
draw(p$ic.scale)
draw(p$ic.flip)
draw(p$ic.inc)
draw(p$ic.neg)



split.m = split.mat(M1)
m.start = correct(M1)
index_elements<-which(m.start$visible==1 & unlist(lapply(m.start$num, all, 1))) 



if (length(split.m) == 1) {
  ic.col = split.m[[1]]
  if (is.na(ic.col$shade[[1]]) == T ) {
    ic.col$shade[[1]] = "black"
  } else if (ic.col$shade[[1]] == "white") {
    ic.col$shade[[1]] = "black"
  } else if (ic.col$shade[[1]] == "black") {
    ic.col$shade[[1]] = "white"
  } else if (ic.col$shade[[1]] == "grey") {
    ic.col$shade[[1]] = "white"
  }
}
ic.col$shade
draw(ic.col)

mwhich.element = "triangle"

split.m[[which.element]]

is.na(split.m[[new_index]]$shade[[1]][1])


m = M1
split.m = split.mat(m)

if (length(split.m) == 1) {
  ic.col = split.m[[1]]
  if (is.na(ic.col$shade[[1]]) == T ) {
    ic.col$shade[[1]] = "black"
  } else if (ic.col$shade[[1]] == "white") {
    ic.col$shade[[1]] = "black"
  } else if (ic.col$shade[[1]] == "black") {
    ic.col$shade[[1]] = "white"
  } else if (ic.col$shade[[1]] == "grey") {
    ic.col$shade[[1]] = "white"
  }
}
ic.col
m.correct = correct(M1)
index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
length(index_elements)


m.c = m.correct
if (any(unlist(m.c$shade == "black"), na.rm = T) | any(grep("line", unlist(m.c$shade)), na.rm = T) == T) {
  m.c$shade[[1]] = rep("white", 
                              length(any(unlist(m.c$shade == "black"))))
} else if (any(unlist(m.c$shade == "white")) == T) {
  m.c$shade[[1]] = rep("black", 
                              length(any(unlist(m.c$shade == "white"))))
} else if(is.na(any(unlist(m.c$shade))) == T) {
  m.c$shade[[1]] = rep("black", 
                              length(is.na(any(unlist(m.c$shade)))))
} else if (any(grep("line", unlist(m5$Sq9$shade)) == T) == T) {
  m.c$shade[[1]] = rep("white", 
                              length(is.na(any(unlist(m.c$shade)))))
}
ic.col = m.c
