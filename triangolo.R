# traingolo ---- 

stimTri = function(rotation = NULL, 
                   color = NULL, 
                   line = NULL) {
  rot_rule = rotation 
  
  par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
  for (i in 1:length(rot_rule)) {
    for (j in 1:length(rot_rule[[i]])) {
      temp = NULL
      Canvas(15, 15)
      DrawRegPolygon(rot = rotation, nv=3)
    }
  }
}
