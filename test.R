library(DescTools)

a = c(1:3)

r1 = c(1,2)
r2 = c(2:3)

a[!a %in% r1]
a[!a %in% r2]


# 14/09/ 2022 ellisse ----

stimElli(rotation = T)
elliCorrect(rotation = T)

stimElli(rotation = T, color = T)
elliCorrect(rotation = T, color = T)

stimElli(rotation = T, color = T, line=T)
elliCorrect(rotation = T, color = T, line = T)


stimElli(color = T)
elliCorrect(color = T)

stimElli(line = T)
elliCorrect(line = T)



stimElli(color = T, line = T)
elliCorrect(color = T, line = T)



alpha = c(.10, .50, .70)


rot = c(0, 45, 90)



par(mfrow=c(3,3))
for (i in 1:length(alpha)) {
    for (j in 1:length(alpha[[i]])) {
      Canvas(15, 15)
      DrawEllipse(x = 0, y = 0, 
                  radius.x = 15, 
                  radius.y = 10, rot = rot[[i]][j],
                  col=SetAlpha("black", alpha[[i]][j]))
    }
    
  }
  

alpha = list(first = c(.10, .50, .90), 
             second = c(.90, .50, .10), 
             third = c(.50, .10))

rot = list(first =  c(0, 45, 90), 
           second = c(90, 45, 0), 
           third = c(45, 0))

stimGen("ellipse", color = "blue", rotation = rot)
stimGen("ellipse", color = "red", rotation = rot, alpha = alpha)
stimGen("circle", color = "yellow")
stimGen("circle", color = "blue", alpha = alpha)


par(mfrow=c(1,1))
Canvas(15, 15)
# ellisse
DrawEllipse(x = 0, y = 0, 
            radius.x = 15, 
            radius.y = 10, 
            rot = 45)
# cerchio 
Canvas(15, 15)
DrawEllipse(x = 0, y = 0, 
            radius.x = 10, 
            radius.y = 10, 
            rot = rot[[1]][3])
# rettangolo 
Canvas(15,15)
DrawEllipse(x = 5, y = 5, 
            radius.x = 10, 
            radius.y = 10, col = "black")


# riga orizzontale
Canvas(15, 15)
DrawEllipse(x = 0, y = 10, 
            radius.x = 15, 
            radius.y = 0)
# riga verticale 
Canvas(15, 15)
DrawEllipse(x = 0, y = 10, 
            radius.x = 15, 
            radius.y = 0)

# 19/09/2022 ----- 
# relation rule ----- 
# colors
(my_rule = rule_relation(rotation = T, which_rot = "htv"))
(my_rule = rule_relation(shade = T, which_shade = "wtb"))
(my_rule = rule_relation(line = T, which_line = "dads"))

(my_rule = rule_relation(rotation = NULL,  
                         shade = T, which_shade = "wtb")
)
(my_rule = rule_relation(rotation = T,  which_rot = "vth", 
                         shade = T, which_shade = "wtg",
                         line = T, which_line = "sdad")
)

par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
Canvas()
stimElli(my_rule, rotation = T, line = T)

stimTri(my_rule, line = T, color  = T, rot = T)

# which_lines 
line_ruleG = c(s,da,do)
line_rule[[i]] = line_ruleG

(my_rot = rule_relation(which_rot = "htv"))
(my_which_shade = rule_relation(which_rot = "htv", which_shade = "wtg"))
my_rules = rule_relation(which_rot = "htv", which_shade = "wtg", which_line = "sdad")
par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
Canvas()
stimElli(which_rot = my_rot)
stimElli(which_rot = my_rot, color = my_which_shade, which_line = my_rules)
stimTri(which_rot = my_rules, 
        color = my_rules, 
        which_line = my_rules)



Canvas()
stimTri(which_rot = my_rot)

par(mfrow = c(1,1))
plot(c(0,1),c(0,1), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
Canvas(15,15)
DrawEllipse(x = 0,
            radius.x = 10, 
            radius.y = 15, 
            lwd = 2, 
            rot = pi/3, plot = T)

DrawRegPolygon(rot = my_rot[[2]][1], nv=3)

DrawRegPolygon(x = 0.5, y = 0.5, rot =  c(my_rot[[1]][1], 
                                          my_rot[[2]][1], 
                                          my_rot[[3]][1]), radius.x = 0.5, nv = 3,
               col = SetAlpha(c("yellow", "blue", "pink"),0.5))

par(mfrow=c(3,3), mar = c(0.5,6,0.5,2)+0.1)
for (i in 1:length(my_rot)) {
  for (j in 1:length(my_rot[[i]])) {
    temp = NULL
    Canvas(15, 15)
    DrawEllipse(x = 0,
                radius.x = 10, 
                radius.y = 15, 
                lwd = 2, 
                rot = my_rot[[i]][[j]], plot = T)
  }
}

par(mfrow=c(1,2))
Canvas(15, 15)
DrawEllipse(x = 0,
            radius.x = 10, 
            radius.y = 15, rot = 0)




Canvas()
DrawEllipse(rot = c(1:3) * pi/3, col=SetAlpha(c("blue","red","green"), 0.5) )

