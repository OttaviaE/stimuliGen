draw(cof(triangle(pos.x = 0, pos.y = 10, rot=pi/6, 
             s.x = 10, s.y=10), 
    triangle(pos.x = 0, pos.y = -10, rot=pi/2, 
             s.x = 10, s.y=10)))
draw(dot(), canvas = F)
draw(bow.tie(pos.x = 10))
draw(ellipse(rot=pi/6))
draw(triangle(s.x = 10, 
              s.y = 7, rot=pi/6), canvas = F)
draw(ellipse(rot = pi/2), canvas = F)
draw(triangle(s.x = 10, 
              s.y = 7, rot=pi/2), canvas = F)
draw(ellipse(rot=2*pi/3))

# bowtie ruotato sx 
m = cof((triangle(s.x = 10, 
                  s.y = 10, rot=pi/3, 
                  pos.y = -7, pos.x = 0)),
    (triangle(s.x = 10, 
                  s.y = 10, rot=4*pi/3, 
                  pos.x = 10, pos.y = 10)))
draw(m)
draw(m1, canvas = F)
m1 = dot(pos.x = 5, pos.y = 2)
draw(com(m, m1))

# bowtie ruotato destra
draw(triangle(s.x = 10, 
              s.y = 10, rot=2*pi/3, pos.y = -10, pos.x = 10), canvas = T)
draw(triangle(s.x = 10, 
              s.y = 10, rot=5*pi/3, 
              pos.x = 0, pos.y = 7), canvas = F)
draw( dot(pos.x = 5, pos.y = -1), canvas = F)

