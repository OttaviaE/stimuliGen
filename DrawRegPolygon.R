DrawRegPolygon<-function (x = 0, y = x, radius.x = 1, radius.y = radius.x, rot = 0, 
          nv = 3, border = par("fg"), col = par("bg"), lty = par("lty"), 
          lwd = par("lwd"), plot = TRUE) 
{
  
  lgp <- list(x = x, y = y, radius.x = radius.x, radius.y = radius.y, 
              rot = rot, nv = ifelse(nv==101,4,nv))
  maxdim <- max(unlist(lapply(lgp, length)))
  lgp <- lapply(lgp, rep, length.out = maxdim)
  if (length(col) < maxdim) {
    col <- rep(col, length.out = maxdim)
  }
  if (length(border) < maxdim) {
    border <- rep(border, length.out = maxdim)
  }
  if (length(lwd) < maxdim) {
    lwd <- rep(lwd, length.out = maxdim)
  }
  if (length(lty) < maxdim) {
    lty <- rep(lty, length.out = maxdim)
  }
  lst <- list()
  for (i in 1:maxdim) {
    theta.inc <- 2 * pi/lgp$nv[i]
    theta <- seq(0, 2 * pi - theta.inc, by = theta.inc)
    ptx <- cos(theta) * lgp$radius.x[i] + lgp$x[i]
    pty <- sin(theta) * lgp$radius.y[i] + lgp$y[i]
    if (lgp$rot[i] > 0) {
      dx <- ptx - lgp$x[i]
      dy <- pty - lgp$y[i]
      ptx <- lgp$x[i] + cos(lgp$rot[i]) * dx - sin(lgp$rot[i]) * 
        dy
      pty <- lgp$y[i] + sin(lgp$rot[i]) * dx + cos(lgp$rot[i]) * 
        dy
    }
    if(nv[[1]]==101 & radius.x[[1]] != radius.y[[1]]){
      pt<-expand.grid(c(radius.x+x,-radius.x+x),c(radius.y+y,-radius.y+y))
      pt<-pt[c(1,2,4,3),]
      ptx<-pt$Var1
      pty<-pt$Var2
      }
    
    if (plot) {polygon(ptx, pty, border = border[i], col = col[i], 
                lty = lty[i], lwd = lwd[i])}
    lst[[i]] <- list(x = ptx, y = pty)
  }
  lst <- lapply(lst, xy.coords)
  if (length(lst) == 1) 
    lst <- lst[[1]]
  invisible(lst)
}
