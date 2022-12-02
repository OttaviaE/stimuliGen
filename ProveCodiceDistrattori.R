ic = function(m, m2 = NULL, m3 = NULL, m4 = NULL) {
  if (is.null(m2) == F) {
    resp.correct = correct(m2)
  } else {
    resp.correct = correct(m)
  }
  if (is.null(m3) == F) {
    resp.correct = correct(m2)
  } else {
    resp.correct = correct(m)
  }
  if (is.null(m4) == F) {
    resp.correct = correct(m2)
  } else {
    resp.correct = correct(m)
  }
  
  
  rules.list = c(m$hrule, m$vrule) 
  if (any(rules.list == "rotation") == T) {
    ic.flip = rotation(resp.correct, 3)
  }
  
  ic = list(ic.flip = ic.flip)
  return(ic)
  
}

resp.correct = correct(a_3a)
if (any(rules.list == "rotation") == T) {
  ic.flip = rotation(resp.correct, 2)
}

draw(ic.flip)
draw(ic(a_3, a_3b, a_3a)$ic.flip)


a = decof(a_3$Sq9)

obj = NULL
for (i in 1:length(a)) {
  if (any(unlist(a[[i]]$tag) == "rotate") == T) {
    obj = a[[i]]
  }
}

new = cof(rotation(obj, 3))
draw(new)
draw(com(new, a_3a$Sq9, a_3b$Sq9))


unlist(a[[3]])




