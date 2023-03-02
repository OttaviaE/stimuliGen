## Logic 3 (era logic 2 nella skill map)

::: {.grid}

::: {.g-col-4}
### 0

```{r}
a_logic3a<-logic_rules(Raven(cof(hexagon(shd="line1"),
                                 hexagon(shd="line2"),dot(),
                                 e.hexagon())),"AND")

a_logic3b<-logic_rules(Raven(cof(size(bow.tie(), 2),
                                 size(bow.tie.inv(), 2))),"XOR")
a_logic3 = com(a_logic3a, a_logic3b)

draw(a_logic3, hide =F)

```


```{r}
dist.a_logic3 = responses(a_logic3)

sel.al3 = c("correct", "r.top", "r.left", 
            "wp.copy", "wp.matrix", 
            "d.union", 
            "ic.inc", 
            "ic.flip") 

resp.a_logic3 = select.dist(dist.a_logic3, sel.al3)
p = split.mat(a_logic3, cell = 3)
resp.a_logic3$ic.flip = cof(resp.a_logic3$ic.inc, p$triangle)

resp.a_logic3$d.union = cof(a_logic3$Sq5, 
                            luck(s.x = 17))
resp.a_logic3$wp.matrix = cof(a_logic3$Sq5, 
                              dot())

draw.dist(resp.a_logic3, n.resp = 8)



```


:::
  
  ::: {.g-col-4}
### 1

```{r}
a1_logic3a<-logic_rules(Raven(cof(pentagon(shd="line2"),
                                  pentagon(shd="line1"),dot(),
                                  pentagon())),"AND")

a1_logic3b<-logic_rules(Raven((cof(petalo.giu, petalo.su, 
                                   petalo.sx, petalo.dx))),"XOR")

a1_logic3 = com(a1_logic3a, a1_logic3b)

draw(a1_logic3, hide =F)
```


```{r}
dist.a1_logic3 = responses(a1_logic3)

sel.al12 = c("correct", "r.top", "r.left", 
             "wp.copy", "wp.matrix", 
             "d.union", 
             "ic.inc", 
             "ic.flip") 

resp.a1_logic3 = select.dist(dist.a1_logic3, sel.al3)
resp.a1_logic3$ic.flip = cof(resp.a1_logic3$ic.inc, 
                             petalo.dx)

resp.a1_logic3$d.union = cof(a1_logic3$Sq5, 
                             circle(s.x = 12, s.y = 12))

resp.a1_logic3$wp.matrix = cof(a1_logic3$Sq5, 
                               dot())

draw.dist(resp.a1_logic3, n.resp = 8)



```

:::
  
  ::: {.g-col-4}

### 2

```{r}
# a2_logic3a<-logic_rules(Raven(cof(hexagon(shd="line1", lty = 0),
#                                  hexagon(shd="line2", lty = 0),
#                                  dot(),e.hexagon())),"XOR")

d.cerchio = 16
# a2_logic3a1<-apply(Raven(
#   st1=square(s.x = d.cerchio, s.y=d.cerchio)
# ))
# raggio = d.cerchio/2
# 
# 
# 
# a2_logic3a = logic_rules(Raven(cof(square(s.x = raggio *sqrt(2), 
#                                          s.y = raggio *sqrt(2), 
#                                          pos.x = (raggio*(pi/2))/2, 
#                                          pos.y = (raggio*(pi/2))/2,
#                                          shd="grey", lty = 0), 
#          square(s.x = raggio *sqrt(2), 
#                                          s.y = raggio *sqrt(2), 
#                                          pos.x = (raggio*(pi/2))/2, 
#                                         pos.y = -(raggio*(pi/2))/2,
#                                         shd="grey", lty = 0), 
#           square(s.x = raggio *sqrt(2), 
#                                          s.y = raggio *sqrt(2),  
#                                         pos.x = -(raggio*(pi/2))/2, 
#                                         pos.y = -(raggio*(pi/2))/2, 
#                  shd = "grey", lty = 0), 
#          square(s.x = raggio *sqrt(2), 
#                                          s.y = raggio *sqrt(2), 
#                                          pos.x = -(raggio*(pi/2))/2, 
#                                         pos.y = (raggio*(pi/2))/2, 
#                 shd = "grey", 
#                 lty = 0))), "XOR")
# 
# 
# a2_logic3b<-logic_rules(Raven((cof(slice(s.x = 10), 
#                                    rotation(slice(s.x = 10), 3), 
#                                    rotation(slice(s.x = 10), 5), 
#                                    rotation(slice(s.x = 10), 7)))),"AND")

maxi = cof(luck(pos.x = pos.x+cost.x, pos.y = pos.x, rot=pi, 
                s.x = cost.x, s.y=cost.y, shd = "white"), 
           luck(pos.x = pos.x-cost.x, pos.y = pos.x, rot=-pi, 
                s.x = cost.x, s.y=cost.y, shd = "white"), 
           luck(pos.x = pos.x, pos.y = pos.x+cost.x, rot=-pi, 
                s.x = cost.y, s.y=cost.x, shd = "white"),
           luck(pos.x = pos.x, pos.y = pos.x-cost.x, rot=-pi, 
                s.x = cost.y, s.y=cost.x, shd = "white")) 

a2_logic3a1 = apply(Raven(
  st1 = circle(s.x =15, s.y =15)
))

a2_logic3a = logic_rules(Raven(cof(slice(shd="line21", lty = 0), 
                                   rotation(slice(shd="line21", lty = 0), 3), 
                                   rotation(slice(shd="line21", lty = 0), 5), 
                                   rotation(slice(shd="line21", lty = 0), 7))), "XOR")

a2_logic3b = logic_rules(Raven(size(maxi, 2)), "AND")

a2_logic3 = com(a2_logic3a, a2_logic3a1,  a2_logic3b)

draw(a2_logic3, hide =F)
```


```{r}
dist.a2_logic3 = responses(a2_logic3)

sel.al23 = c("correct", "r.top", "r.left", 
             "wp.copy", "wp.matrix", 
             "d.union", 
             "ic.inc", 
             "ic.flip") 

resp.a2_logic3 = select.dist(dist.a2_logic3, sel.al23)
# resp.a1_logic3$ic.flip = cof(resp.a1_logic3$ic.inc, 
#                              petalo.dx)
# 
# resp.a1_logic3$d.union = cof(a1_logic3$Sq5, 
#                             circle(s.x = 12, s.y = 12))

resp.a2_logic3$ic.flip = cof(resp.a2_logic3$ic.inc,
                             size(luck(pos.x = pos.x-cost.x, pos.y = pos.x, rot=-pi, 
                                       s.x = cost.x, s.y=cost.y), 2))

resp.a2_logic3$d.union = cof(a2_logic3$Sq5, 
                             size(maxi, 2), 
                             square())

draw.dist(resp.a2_logic3, n.resp = 8)



```


:::
  
  :::
  
  