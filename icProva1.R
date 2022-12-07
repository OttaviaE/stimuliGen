# funzione che decompone la matrice nei suoi pezzi singoli e li salva dentor una 
# lista di liste (so che c'è decof ma non mi trovo bene)
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

split.mat(b2)

# prova cambio colore ma la metto in una funzione a parte ---- 

m.correct = correct(m_pratica1)

ic.neg = function(m) {
  m.correct = correct(m)
  
  if (any(unlist(m.correct$shade == "black")) == T) {
    m.correct$shade[[1]] = rep("white", 
                               length(any(unlist(m.correct$shade == "black"))))
  } else if (any(unlist(m.correct$shade == "white")) == T) {
    m.correct$shade[[1]] = rep("black", 
                               length(any(unlist(m.correct$shade == "white"))))
  } else if(is.na(any(unlist(m.correct$shade))) == T) {
    m.correct$shade[[1]] = rep("black", 
                               length(is.na(any(unlist(m.correct$shade)))))
  } 
  ic.col = m.correct
  return(ic.col)
}


draw(ic.neg(m_pratica1))

ic = function(m, 
              n.rule = 1, 
              which.element = NULL) {
  m.correct = correct(m)
  
  ##Inizio aggiunta brutta
  elements<-decof(m.correct)##il controllo con i tag è sbagliato $EVVIVA$!
  index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1)) )
  #unlist(lapply(m.correct$tag,function(x) any(x== "rotate"))) )
  
  if (length(elements) == 1) {
    ic.scale = size(m.correct, 3)
    ic.flip = rotation(m.correct, 3)
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

## prove -----

p = responses(b2, 
              which.element = "ellipse")

draw.dist(p, n.resp = 10, 
          main =T)


disappear = "ellipse"
p = ic(b2, which.element = "ellipse")
draw(p$ic.flip); draw(p$ic.scale); draw(p$ic.inc)

if (is.null(disappear) == T) {
  new_index = sample(index_elements,1)
  ic.inc = hide(m.correct, new_index)
} else {
  ic.inc = hide(m.correct, 
                index_elements[which(names(split.m) == disappear)])
}

draw(ic.inc)

m.correct = correct(b2)
draw(m.correct)
index_elements<-which(m.correct$visible==1 & unlist(lapply(m.correct$num, all, 1))) 
split.m <- vector("list", length(index_elements))

which.element = "ellipse"
ic.flip = NULL
ic.scale = NULL
if (is.null(which.element) == T) {
  ic.flip = rotation(split.m[[1]], 2)
  ic.scale = size(split.m[[1]], 2)
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
}

ic.flip = cof(split.m[[which(names(split.m) != which.element)[i]]], 
              ic.flip)

ic.scale = cof(split.m[[which(names(split.m) != which.element)[i]]], 
               ic.scale)

draw(ic.flip)
draw(ic.scale)


for (i in 1:length(split.m)) {
  split.m[[i]] <- vector("list", length(m.correct))
  for (j in 1:length(split.m[[i]])) {
    names(split.m)[i] = m.correct$shape[index_elements[i]]
    attr(split.m[[i]], "class") = "field"
   split.m[[i]][[j]] = m.correct[[j]][index_elements[i]]
   names(split.m[[i]])[j] = names(m.correct)[j]
  }
}




draw(split.mat(b2)[[1]])
draw(cof((split.m[[2]]), split.m[[1]]))
draw(cof(split.m[[1]], rotation(split.m[[2]], 2)))

draw(cof((split.m[[1]]),, ))
draw(cof(size(split.m$square, 5), (split.m$pentagon)))

for (i in 1:length(sub_horizon)) {
  sub_horizon[[i]] <- vector("list", window_front)
  for (m in 1:length(sub_horizon[[i]])) {
    sub_horizon[[i]][[m]] <- vector("list", horizon)
  }
}


draw(cof(m1, m2))

index_elements[1]
draw(m.correct$shape[index_elements[1]])
                      
draw(m_pratica4)
p = ic(m_pratica4)
draw(p$ic.flip)

ic = function(m, 
              n.rule = 1) {
  m.correct = correct(m)
  
  ic.scale = size(m.correct, 3)
  ic.flip = rotation(correct(m), 3) # il numero sulla rotazione dipende dalla figura
  if (n.rule != 1) {
    ic.inc = hide(m.correct, 3) # questo sarebbe da riscrivere sulla base di chi è effettivamente visibile in quella cella
  } else {
    ic.inc = m.correct
    ic.inc$attention = "I need more stuff to make IC-INC"
  }
  ic.dist = list(ic.scale = ic.scale, 
                 ic.flip = ic.flip, 
                 ic.inc = ic.inc)
  return(ic.dist)
}
