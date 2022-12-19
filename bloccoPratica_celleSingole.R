# blocco pratica -----

source("C:/Users/huawei/OneDrive/Documenti/GitHub/stimuliGen/Shapes_list-10-11-Ottavia.R")
source("C:/Users/huawei/OneDrive/Documenti/GitHub/stimuliGen/Class and Methods v02.R")
source("C:/Users/huawei/OneDrive/Documenti/GitHub/stimuliGen/Rules_27102022.R")
source("C:/Users/huawei/OneDrive/Documenti/GitHub/stimuliGen/CodiceDistrattoriVero.R")

print.dist = function(resp.list, mat.name) {
  for (i in 1:length(resp.list)) {
    svg(paste0(getwd(), "/RavenMat/",
               mat.name, "_", names(resp.list)[i], ".svg")
    )
    draw(resp.list[[i]])
    dev.off()
  }
}

print.mat = function(mat, mat.name) {
  p = matrix(names(mat)[1:9], nrow = 3)
  nv = NULL
  for (i in 1:nrow(p)) {
    for (j in 1:nrow(p)) {
      nv = c(nv, paste0(i, j))
    }
  }
  
  for (i in 1:8) {
    svg(paste0(getwd(), "/RavenMat/", mat.name, "_", nv[i], ".svg"))
    draw(mat[[i]])
    dev.off()
  }
  
}

set.seed(999)

# pratica 1----
m_pratica1 <-apply(Raven(st1=cof(e.hexagon()),
                         hrule=c("fill"),vrule=c("identity")))
draw(m_pratica1, hide = T)

print.mat(m_pratica1, "pratica001")

pratica_1_dist = responses(m_pratica1)


resp_m_pratica1 = within(pratica_1_dist, 
                         rm(r.left, r.top, ic.scale, ic.inc))

resp_m_pratica1[["ic.neg"]] = ic.neg(m_pratica1)
resp_m_pratica1[["wp.copy"]] = rotation(resp_m_pratica1$wp.copy, 2)
resp_m_pratica1[["d.difference"]] = square()

draw.dist(resp_m_pratica1, main = T, single.print = F)

print.dist(resp_m_pratica1, "pratica001")

# Pratica 2 -----

m_pratica2 = apply(Raven(st1=pie.4(),
                         hrule=c("identity"),
                         vrule=c("size")))
draw(m_pratica2, hide = TRUE)

print.mat(m_pratica2, "pratica002")

pratica_2_dist = responses(m_pratica2)

resp_m_pratica2 = list(correct = correct(m_pratica2), 
                       dist1 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist2 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist3 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist4 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist5 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist6 = circle(s.x = 15, s.y = 15, lwd = 3), 
                       dist7 = circle(s.x = 15, s.y = 15, lwd = 3)) 

draw.dist(resp_m_pratica2, n.resp = 8, main = T)
print.dist(resp_m_pratica2, "pratica002")

# Pratica 3 ----
m_pratica3 = apply(Raven(st1=u.pie.2(),
                         hrule=c("lty"),
                         vrule=c("identity")))
draw(m_pratica3, hide = T)
print.mat(m_pratica3, "pratica003")

pratica_3_dist = responses(m_pratica3)

resp_m_pratica3 = within(pratica_3_dist, 
                         rm(r.left, r.top, ic.scale, ic.inc))

correct_pratic3 = correct(m_pratica3)
correct_pratic3$shade[[1]] = rep("grey", 2)
resp_m_pratica3[["ic.neg"]] = (correct_pratic3)
resp_m_pratica3[["difference"]] = ellipse(shd = "black")
resp_m_pratica3[["ic.flip"]] = rotation(correct(m_pratica3), 3)
resp_m_pratica3$r.diag = NULL
resp_m_pratica3$r.left = rotation(pratica_3_dist$r.left, 3)

draw(resp_m_pratica3$ic.flip)


draw.dist(resp_m_pratica3, n.resp = 8, main = T)
print.dist(resp_m_pratica3, "pratica003")

# Pratcia 4 -----
m_pratica4a = apply(Raven(st1 = cof(dot(), 
                                    s.lily(), 
                                    square(s.x = 5, s.y = 5, 
                                           shd = "black", rot = pi/2)), 
                          hrule = "diff_shapes"))

m_pratica4b = apply(Raven(st1=pentagon(),
                          hrule=c("identity"),
                          vrule=c("identity")))

m_pratica4 = (com(m_pratica4a, m_pratica4b))

draw(m_pratica4, hide = T)

print.mat(m_pratica4, "pratica004")

pratica_4_dist = responses(m_pratica4, choose.copy = 1)

resp_m_pratica4 = within(pratica_4_dist, 
                         rm(r.left, r.top, ic.scale))

s_p4 = split.mat(m_pratica4)

s_p4[[2]]$shade[[1]] = "black"
  s_p4[[1]]$shade[[1]] = "white"
    
  resp_m_pratica4[["ic.neg"]] = cof(s_p4[[2]], s_p4[[1]])
  
  resp_m_pratica4$d.union = cof(resp_m_pratica4$wp.copy, 
                                cross.dice())
  
  draw.dist(resp_m_pratica4, main = T)
print.dist(resp_m_pratica4, "pratica004")
