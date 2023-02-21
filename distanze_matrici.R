rm(list = ls())
source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")
source("DrawRegPolygon.R")
source("CodiceDistrattoriVero.R")

# Approccio 1 ----
# L'intero contenuto della matrice in un dataframe
matiks2data.frame<-function(Mat){
  variable<-names(Mat)
  data<-data.frame(matrice= "matrice")
  for(vars in length(variable):1)
  {
    if(grepl("rule",variable[vars]))
    {
      data[[variable[vars]]]<-paste0(Mat[[variable[vars]]],collapse = ",")
    }else{
      variable_sq<-names(Mat[[variable[vars]]])
      cella<-Mat[[variable[vars]]]
      visibile<-which(cella$visible==1)
      variable_sq<-variable_sq[variable_sq!="visible" & variable_sq!="tag" & variable_sq!="num" ]
      colonne<-paste0(variable_sq[variable_sq!="visible" & variable_sq!="tag" & variable_sq!="num" ],".",variable[vars])
      for(sq_ele in 1:length(variable_sq))
      {
        data[[colonne[sq_ele]]]<-paste0(cella[[variable_sq[sq_ele]]][visibile],collapse=",")
      }
    }
  }
  return(data[,2:ncol(data)])
}

# Approccio 2 ----
matiks2competenze<-function(Mat){
  variable<-names(Mat)
  competenze<-NULL
  
  for(vars in length(variable):1)
  {
    if(grepl("rule",variable[vars]))
    {
      competenze<-c(competenze,paste0(variable[vars],".",unlist(Mat[[variable[vars]]])))
    }else{
      variable_sq<-names(Mat[[variable[vars]]])
      cella<-Mat[[variable[vars]]]
      visibile<-cella$visible
      variable_sq<-variable_sq[variable_sq!="visible" & 
                                 variable_sq!="tag" & 
                                 variable_sq!="num" &
                                 variable_sq!="shape"]
      for(shp in 1:length(cella$shape))
      {
        if(visibile[[shp]]==1)
        {
          for(sq_ele in 1:length(variable_sq))
          {
            competenze<-c(competenze,paste0(variable[vars],".",cella$shape[[shp]],".",variable_sq[sq_ele],".",
                               cella[[variable_sq[[sq_ele]]]][shp]))
          }
        }
      }
      
    }
  }
  return(competenze)
}

# Forme ----
square4bis <- function() {
  value <-cof(hline(pos.y=-11),vline(pos.x=11),
              hline(pos.y=11),vline(pos.x=-11))
  value$tag <- list("compose4")
  attr(value, "class") <- "field"
  value
}


smallbow.tie.inv <- function(pos.x = 0,pos.y=0,shd=NA) {
  value <-cof(triangle(pos.x = pos.x+5, pos.y = pos.y, rot=pi/3, 
                       s.x = 5, s.y=5,shd = shd), 
              triangle(pos.x = pos.x-5, pos.y = pos.y, rot=-pi, 
                       s.x = 5, s.y=5,shd = shd))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "field"
  value
}

bow.tie.inv <- function(pos.x = 0) {
  value <-cof(triangle(pos.x = pos.x+10, pos.y = pos.x, rot=pi/3, 
                       s.x = 10, s.y=10), 
              triangle(pos.x = pos.x-10, pos.y = pos.x, rot=-pi, 
                       s.x = 10, s.y=10))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "field"
  value
}


lilth<-lily()
s.lilth<-s.lily()
for(i in 1:length(lilth$shape)) {
  lilth$size.x[[i]] <-lilth$size.x[[i]]/2
  lilth$size.y[[i]] <-lilth$size.y[[i]]/2
  lilth$pos.y[[i]] <-lilth$pos.y[[i]]/2
  lilth$pos.x[[i]] <-lilth$pos.x[[i]]/2
  
}

s.lilth$size.x[[1]] <-s.lilth$size.x[[1]]/2
s.lilth$size.y[[1]] <-s.lilth$size.y[[1]]/2
s.lilth$pos.y[[1]] <-s.lilth$pos.y[[1]]/2
s.lilth$pos.x[[1]] <-s.lilth$pos.x[[1]]/2


papillon = bow.tie()
u.papillon = u.bow.tie()


for(i in 1:length(papillon$shape)) {
  papillon$size.x[[i]] <-papillon$size.x[[i]]/2
  papillon$size.y[[i]] <-papillon$size.y[[i]]/2
  papillon$pos.y[[i]] <-papillon$pos.y[[i]]/2
  papillon$pos.x[[i]] <-papillon$pos.x[[i]]/2
  
}

u.papillon$size.x[[1]] <-u.papillon$size.x[[1]]/2
u.papillon$size.y[[1]] <-u.papillon$size.y[[1]]/2
u.papillon$pos.y[[1]] <-u.papillon$pos.y[[1]]/2
u.papillon$pos.x[[1]] <-u.papillon$pos.x[[1]]/2


# a0_logic3 ---- 
a_logic3a <-logic_rules(Raven(square4()),"OR")

a_logic3b<-logic_rules(Raven(cof(diagline.inv(),diagline(),hline(),vline())),"XOR")

a_logic3 = com(a_logic3a, a_logic3b)

draw(a_logic3, hide = FALSE)

vec_a0_logic3<-rbind(matiks2data.frame(a_logic3a),matiks2data.frame(a_logic3b))

comp_a0_logic3<-c(matiks2competenze(a_logic3a),matiks2competenze(a_logic3b))
# a1_logic3 ----
a1_logic3a<-logic_rules(Raven(cof(hline(pos.y = 3,s.y=12),hline(pos.y = -3,s.y=12),
                                  square(shd="line12"),pentagon(s.x=3,s.y=3,shd="white"))
),"XOR")
a1_logic3b <- logic_rules(Raven(cof(vline(pos.x = 15, s.x = 15),
                                    vline(pos.x = -15, s.x = 15 ),
                                    hline(pos.y = 15, s.x=15),
                                    hline(pos.y = -15, s.x=15))),"OR")

a1_logic3 = com(a1_logic3a, a1_logic3b)

draw(a1_logic3, hide = F)

vec_a1_logic3<-rbind(matiks2data.frame(a1_logic3b),matiks2data.frame(a1_logic3a))
comp_a1_logic3<-c(matiks2competenze(a1_logic3a),matiks2competenze(a1_logic3b))
# b0_logic3 ----

b_logic3a<-logic_rules(Raven(cof(bow.tie(),
                                 bow.tie.inv())),"XOR")

b_logic3b<-logic_rules(Raven(cof(circle(s.x=4,s.y=4,shd="black")
                                 ,cross.dice(),
                                 hline(),vline())),"OR")

b_logic3 = com(b_logic3a, b_logic3b)

draw(b_logic3, hide = T)

vec_b0_logic3<-rbind(matiks2data.frame(b_logic3b),matiks2data.frame(b_logic3a))
comp_b0_logic3<-c(matiks2competenze(b_logic3a),matiks2competenze(b_logic3b))

# b1_logic3 ----

b1_logic3a<-logic_rules(Raven(cof(square(s.x = 20,s.y = 20),
                                  margin(square(s.x = 17,s.y = 17,shd = "line12"),3,"lty"),
                                  dice(),
                                  margin(square(s.x = 17,s.y = 17),3,"lty")
)),"XOR")

b1_logic3b<-logic_rules(Raven(cof(diagline(),horizontal_eight(),
                                  vertical_eight(),
                                  diagline.inv())),"OR")

b1_logic3 = com(b1_logic3a, b1_logic3b)

draw(b1_logic3, hide = T)

vec_b1_logic3<-rbind(matiks2data.frame(b1_logic3b),matiks2data.frame(b1_logic3a))

comp_b1_logic3<-c(matiks2competenze(b1_logic3a),matiks2competenze(b1_logic3b))

## Accordo database ----

AccordoA<-sum(vec_a0_logic3==vec_a1_logic3)/(nrow(vec_a0_logic3)*ncol(vec_a0_logic3))
AccordoB<-sum(vec_b0_logic3==vec_b1_logic3)/(nrow(vec_b0_logic3)*ncol(vec_b0_logic3))

## Accordo competenze ----
serieA<-unique(c(comp_a0_logic3,comp_a1_logic3))
skmapA<-matrix(0,ncol=length(serieA),nrow = 2)
for(ix in 1:length(serieA))
{
  skmapA[1,ix]<-any(comp_a0_logic3==serieA[ix])
  skmapA[2,ix]<-any(comp_a1_logic3==serieA[ix])
}
HammingA<-sum(abs(skmapA[1,]-skmapA[2,]))/length(serieA)

serieB<-unique(c(comp_b0_logic3,comp_b1_logic3))
skmapB<-matrix(0,ncol=length(serieB),nrow = 2)
for(ix in 1:length(serieB))
{
  skmapB[1,ix]<-any(comp_b0_logic3==serieB[ix])
  skmapB[2,ix]<-any(comp_b1_logic3==serieB[ix])
}
HammingB<-sum(abs(skmapB[1,]-skmapB[2,]))/length(serieB)


# a0_visuo3 ----
a_3a = apply(
  Raven(
    st1 = cof(circle(s.x = 17, s.y = 17), 
              pentagon(s.x = 16, s.y = 16), 
              e.hexagon(s.x = 17, s.y = 17)), 
    vrule = c("diff_shapes"), 
    hrule = "diff_shapes"
  )
)

a_3b = apply(
  Raven(
    st1 = pacman(), 
    vrule = c("rotation"), 
    hrule = "rotation"
  )
)


a_3c = apply(
  Raven(
    st1 = circle(s.x = 2, s.y = 2), 
    vrule = c("fill"), 
    hrule = "fill"
  )
)

a_3 = com(a_3a, a_3b, a_3c)
draw(a_3, hide = T)


vec_a0_visuo3<-rbind(matiks2data.frame(a_3a),matiks2data.frame(a_3b),matiks2data.frame(a_3c))

comp_a0_visuo3<-matiks2competenze(a_3)

# a1_visuo3 ----

a1_3a = apply(Raven(
  st1 = square(s.x = 20, s.y = 20), 
  vrule = "identity", 
  hrule= "identity"
))

a1_3b = apply(Raven(
  st1 = cof(slice(s.x = sqrt(square()$size.x[[1]]^2 /2)), luck(s.x = 10, 
                                                               s.y = 12), 
            triangle(s.x = 10, s.y = 10)), 
  vrule = "diff_shapes", 
  hrule= c("diff_shapes", "rotation")
))

a1_3c = apply(
  Raven(
    st1 = circle(s.x = 2, s.y = 2), 
    vrule = c("fill"), 
    hrule = "fill"
  )
)
a1_3 = com(a1_3a, a1_3b, a1_3c)
draw(a1_3, hide = F)

vec_a1_visuo3<-rbind(matiks2data.frame(a1_3a),matiks2data.frame(a1_3b),matiks2data.frame(a1_3c))

comp_a1_visuo3<-matiks2competenze(a1_3)

## Accordo database ----

AccordoA_visuo3<-sum(vec_a0_visuo3==vec_a1_visuo3)/(nrow(vec_a1_visuo3)*ncol(vec_a1_visuo3))


## Accordo competenze ----
serieA<-unique(c(comp_a0_visuo3,comp_a1_visuo3))
skmapA<-matrix(0,ncol=length(serieA),nrow = 2)
for(ix in 1:length(serieA))
{
  skmapA[1,ix]<-any(comp_a1_visuo3==serieA[ix])
  skmapA[2,ix]<-any(comp_a0_visuo3==serieA[ix])
}
HammingA_visuo3<-sum(abs(skmapA[1,]-skmapA[2,]))/length(serieA)


