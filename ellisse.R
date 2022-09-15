library(DescTools)
# funzione generativa ellisse -----
stimElli = function(rotation = NULL, 
                    color = NULL, 
                    line = NULL) {
  if (is.null(rotation) == T & is.null(color) == T & is.null(line) == T) {
    stop("Please specify at least one argument")
  }
  rot_rule1 = list(first = c(1:3) * pi/3, 
                   second = c(2,3, 1) * pi/3, 
                   third = c(3, 1)* pi/3)
  col_rule1 = list(first = c(0.10, 0.50, 0.00), 
                   second = c(0.50, 0.00, 0.10), 
                   third = c(0.00, 0.10))
  lty_rule1 = list(first = c(1:3), 
                   second = c(2,1,3), 
                   third = c(3,1))
  if (is.null(rotation) == F & is.null(color) == T & is.null(line) == T) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(rot_rule1)) {
      for (j in 1:length(rot_rule1[[i]])) {
        temp = NULL
        Canvas(15, 15)
        DrawEllipse(x = 0, y = 0, 
                    rot = rot_rule1[[i]][[j]], 
                    radius.x = 10, radius.y = 15, plot = T)
      }
    }
  } else if (is.null(rotation) == F & is.null(color) == F & is.null(line) == T) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(col_rule1)) {
      for (j in 1:length(col_rule1[[i]])) {
        Canvas(15, 15)
        DrawEllipse(x = 0, 
                    radius.x = 10, 
                    radius.y = 15, 
                    lwd = 2, 
                    col=SetAlpha(c(rep("black", 3)), 
                                 col_rule1[[i]][[j]]), 
                    rot = rot_rule1[[i]][[j]])
      }
    }
  } else if (is.null(rotation) == F & is.null(color) == F & is.null(line) == F) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(lty_rule1)) {
      for (j in 1:length(lty_rule1[[i]])) {
        Canvas(15, 15)
        DrawEllipse(x = 0, 
                    radius.x = 10, 
                    radius.y = 15, 
                    lwd = 2, 
                    lty = lty_rule1[[i]][[j]], 
                    col=SetAlpha(c(rep("black", 3)), 
                                 col_rule1[[i]][[j]]), 
                    rot = rot_rule1[[i]][[j]])
      }
    }
    
  } else if (is.null(rotation) == T & is.null(color) == F & is.null(line) == T) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(col_rule1)) {
      for (j in 1:length(col_rule1[[i]])) {
        Canvas(15, 15)
        DrawEllipse(x = 0, 
                    radius.x = 10, 
                    radius.y = 15, 
                    lwd = 2, 
                    col=SetAlpha(c(rep("black", 3)), 
                                 col_rule1[[i]][[j]]))
      }
    }
  } else if (is.null(rotation) == T & is.null(color) == F & is.null(line) == F) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(col_rule1)) {
      for (j in 1:length(col_rule1[[i]])) {
        Canvas(15, 15)
        DrawEllipse(x = 0, 
                    radius.x = 10, 
                    radius.y = 15, 
                    lwd = 2, 
                    col=SetAlpha(c(rep("black", 3)), 
                                 col_rule1[[i]][[j]]), 
                    lty = lty_rule1[[i]][[j]])
      }
    }
  } else if (is.null(rotation) == T & is.null(color) == T & is.null(line) == F) {
    par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
    for (i in 1:length(col_rule1)) {
      for (j in 1:length(col_rule1[[i]])) {
        Canvas(15, 15)
        DrawEllipse(x = 0, 
                    radius.x = 10, 
                    radius.y = 15, 
                    lwd = 2, 
                    lty = lty_rule1[[i]][[j]])
      }
    }
  }
}

# codice per generare l'ellisse corretta ----
# (volendo il codice per l'identificazione della risposta corretta può essere unico
# per tutti - il codice che identifica la componente - e viene usato in modo diverso 
# a seconda dello stimolo)

elliCorrect = function(x, 
                       rotation = NULL, 
                       color = NULL, 
                       line = NULL) {
  if (is.null(rotation) == T & is.null(color) == T & is.null(line) == T) {
    stop("I need arguments")
  }
  rot_rule1 = list(first = c(1:3) * pi/3, 
                   second = c(2,3, 1) * pi/3, 
                   third = c(3, 1)* pi/3)
  col_rule1 = list(first = c(0.10, 0.50, 0.00), 
                   second = c(0.50, 0.00, 0.10), 
                   third = c(0.00, 0.10))
  lty_rule1 = list(first = c(1:3), 
                   second = c(2,1,3), 
                   third = c(3,1))
  if ((is.null(rotation) == F & is.null(color) == T & is.null(line) == T)) {
    rule1_last = rot_rule1[[length(rot_rule1)]] 
    rule1_last = rule1_last[order(rule1_last)]
    rule1_first = rot_rule1[[1]] 
    rule1_first = rule1_first[order(rule1_first)]
    component1 = rule1_first[!rule1_first %in% rule1_last]
    par(mfrow=c(1,1))
    Canvas(15,15)
    DrawEllipse(x = 0, y = 0, radius.x = 10, radius.y = 15, lwd = 2,
                rot = component1, plot = T)
  } else if (is.null(rotation) == F & is.null(color) == F & is.null(line) == T) {
    rule1_last = rot_rule1[[length(rot_rule1)]] 
    rule1_last = rule1_last[order(rule1_last)]
    rule1_first = rot_rule1[[1]] 
    rule1_first = rule1_first[order(rule1_first)]
    component1 = rule1_first[!rule1_first %in% rule1_last]
    
    rule2_last = col_rule1[[length(col_rule1)]] 
    rule2_last = rule2_last[order(rule2_last)]
    rule2_first = col_rule1[[1]] 
    rule2_first = rule2_first[order(rule2_first)]
    component2 = rule2_first[!rule2_first %in% rule2_last]
    
    par(mfrow=c(1,1))
    Canvas(15,15)
    DrawEllipse(x = 0, y = 0, radius.x = 10, radius.y = 15, lwd = 2,
                rot = component1, col = SetAlpha("black", 
                                                 component2) ,  plot = T)
    
  } else if (is.null(rotation) == F & is.null(color) == F & is.null(line) == F) {
    rule1_last = rot_rule1[[length(rot_rule1)]] 
    rule1_last = rule1_last[order(rule1_last)]
    rule1_first = rot_rule1[[1]] 
    rule1_first = rule1_first[order(rule1_first)]
    component1 = rule1_first[!rule1_first %in% rule1_last]
    
    rule2_last = col_rule1[[length(col_rule1)]] 
    rule2_last = rule2_last[order(rule2_last)]
    rule2_first = col_rule1[[1]] 
    rule2_first = rule2_first[order(rule2_first)]
    component2 = rule2_first[!rule2_first %in% rule2_last]
    
    rule3_last = lty_rule1[[length(lty_rule1)]] 
    rule3_last = rule3_last[order(rule3_last)]
    rule3_first = lty_rule1[[1]] 
    rule3_first = rule3_first[order(rule3_first)]
    component3 = rule3_first[!rule3_first %in% rule3_last]
    
    par(mfrow=c(1,1))
    Canvas(15,15)
    DrawEllipse(x = 0, y = 0, radius.x = 10, radius.y = 15, lwd = 2,
                rot = component1, col = SetAlpha("black", 
                                                 component2) ,
                
                lty = component3,  
                plot = T)
  }
}
