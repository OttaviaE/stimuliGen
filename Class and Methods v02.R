## Definizione degli oggetti campo (field) e matrice (Raven)

field <- list(
  shape = NULL,
  size.x = list(),
  size.y = list(),
  rotation = list(),
  pos.x = list(),
  pos.y = list(),
  lty =list(),
  lwd = list(),
  num = list(),
  nv = list(),
  shade =list(),  
  visible = NULL, 
  tag = list()
)
class(field) <- "field"

Raven <- list(
  Sq1 = list(),
  Sq2 = list(),
  Sq3 = list(),
  
  Sq4 = list(),
  Sq5 = list(),
  Sq6 = list(),
  
  Sq7 = list(),
  Sq8 = list(),
  Sq9 = list(),
  hrule = list(),
  vrule = list()
)


class(Raven) <- "Raven_matrix"


#########################################################
###  BASIC METHODS FOR THE FIELDS
##########################################################

#' Concatenation of list or vector
#'
#' Function for the concatenation of difference fields
#' @return Return a field object
#' @examples
#'
#' @export
concatenation <- function(...) {
  UseMethod("concatenation")
}

concatenation.list <- function(...) {
  obj <- Map("c", ...)  
  return(obj)
}


concatenation.double <- function(...) {
  obj <- c(...)  
  attr(obj, "class") <- "double"
  obj
}

concatenation.character <- function(...) {
  obj <- c(...)  
  return(obj)
}

concatenation.integer <- function(...) {
  obj <- c(...)  
  return(obj)
}

#' Concatenation of field
#'
#' Function for the concatenation of difference fields
#' @return Return a field object
#' @examples
#'
#' @export
cof <- function(...,name, single) {
  UseMethod("cof")
}

cof.field <- function( ...,name=NULL, single=FALSE) {
  if(single==TRUE)
  {
    obj <- Map("concatenation", ...)
    obj$shape<-name
    obj$visible<-1
  }else{
    obj <- Map("c", ...)  
  }
  attr(obj, "class") <- "field"
  obj
}

#Per generalizzare la funzione per concatenzione di materici ho definito il metodo cof anche per
#i caratteri
cof.character <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "character"
  obj
}


#' Concatenationof matrix
#'
#' Function for the concatenation of difference matrices
#' @return Return a Matrix object
#' @examples
#'
#' @export
com <- function(...) {
  UseMethod("com")
}

com.Raven_matrix <- function(...) {
  obj <- Map("cof", ...)
  attr(obj, "class") <- "Raven_matrix"
  obj
}

#########################################################
###  BASIC METHODS FOR THE Raven
##########################################################


#' Matrix Raven function
#'
#' Function for the definition of a generic Raven matrix
#' @param st1 Fields in the top left square
#' @param hrule Rules applied with an horizontal logic
#' @param vrule Rules applied with an vertical logic
#' @return Return a Raven matrix object
#' @examples
#'
#' @export
Raven <- function(st1, hrule = "identity", vrule = "identity") {
  value <- list()
  squares <- paste0("Sq", 1:9)
  for (i in 1:length(squares))
  {
    value[[squares[i]]] <- st1 #Copy the same field in all the cells
  }
  value$hrule <- hrule
  value$vrule <- vrule
  attr(value, "class") <- "Raven_matrix"
  value
}

apply <- function(obj,rules) {
  UseMethod("apply")
}

draw <- function(obj, main = NULL, canvas = TRUE, 
                 hide = FALSE, n.cell = 9, bg = "white",mar=c(1,1,1,1),xlim=16) {
  UseMethod("draw")
}


apply.Raven_matrix <- function(obj,rules="HV") {
  # The rules are applied by row keeping fixed the row by means of the three vectors
  hrules <- obj$hrule
  row_1 <- paste0("Sq", 1:3)
  row_2 <- paste0("Sq", 4:6)
  row_3 <- paste0("Sq", 7:9)
  
  # The rules are applied by column keeping fixed the row by means of the three vectors
  vrules <- obj$vrule
  col_1 <- paste0("Sq", seq(1, 9, 3))
  col_2 <- paste0("Sq", seq(2, 9, 3))
  col_3 <- paste0("Sq", seq(3, 9, 3))
  
  #This table contains in the first row the label of the rules 
  #and in the second row the function associated
  function_list <- read.csv("function_list.prn", sep="")

  if(rules=="HV")
  {
    for (r in 1:length(hrules))
    {
      nth_rule<-function_list$function.[unlist(lapply(function_list$label,grepl,hrules[r]))]
      if(length(nth_rule)>1)
      {
        nth_rule <-unique(nth_rule)
        nth_rule <-nth_rule[!nth_rule=="fill"] 
      }
      f<-get(nth_rule )
      for (i in 1:3)
      {
        obj[[row_1[i]]] <- f(obj[[row_1[i]]],i,hrules[r],seed=1)
        obj[[row_2[i]]] <- f(obj[[row_2[i]]],i,hrules[r],seed=5)
        obj[[row_3[i]]] <- f(obj[[row_3[i]]],i,hrules[r],seed=6)
      }
    }
    
    #applying the vertical rules
    for (r in 1:length(vrules))
    {
      nth_rule<-function_list$function.[unlist(lapply(function_list$label,grepl,vrules[r]))]
      if(length(nth_rule)>1)
      {
        nth_rule <-unique(nth_rule)
        nth_rule <-nth_rule[!nth_rule=="fill"] 
      }
      f<-get(nth_rule )
      for (i in 1:3)
      {
        obj[[col_1[i]]] <- f(obj[[col_1[i]]],i,vrules[r],seed=1)
        obj[[col_2[i]]] <- f(obj[[col_2[i]]],i,vrules[r],seed=5)
        obj[[col_3[i]]] <- f(obj[[col_3[i]]],i,vrules[r],seed=6)
      }
    }
  }else{
    #applying the vertical rules
    for (r in 1:length(vrules))
    {
      nth_rule<-function_list$function.[unlist(lapply(function_list$label,grepl,vrules[r]))]
      if(length(nth_rule)>1)
      {
        nth_rule <-unique(nth_rule)
        nth_rule <-nth_rule[!nth_rule=="fill"] 
      }
      f<-get(nth_rule)

      for (i in 1:3)
      {
        obj[[col_1[i]]] <- f(obj[[col_1[i]]],i,vrules[r],seed=1)
        obj[[col_2[i]]] <- f(obj[[col_2[i]]],i,vrules[r],seed=5)
        obj[[col_3[i]]] <- f(obj[[col_3[i]]],i,vrules[r],seed=6)
      }
    }
    
    for (r in 1:length(hrules))
    {
      nth_rule<-function_list$function.[unlist(lapply(function_list$label,grepl,hrules[r]))]
      if(length(nth_rule)>1)
      {
        nth_rule <-unique(nth_rule)
        nth_rule <-nth_rule[!nth_rule=="fill"] 
      }
      f<-get(nth_rule )
           for (i in 1:3)
      {
        obj[[row_1[i]]] <- f(obj[[row_1[i]]],i,hrules[r],seed=1)
        obj[[row_2[i]]] <- f(obj[[row_2[i]]],i,hrules[r],seed=5)
        obj[[row_3[i]]] <- f(obj[[row_3[i]]],i,hrules[r],seed=6)
      }
    }
    
    
  }
  
  attr(obj, "class") <- "Raven_matrix"
  return(obj)
}

draw.field<- function(obj, main = NULL, canvas = TRUE, bg = "white",mar=c(1,1,1,1),xlim=16) {
  library(DescTools)
  if (canvas == TRUE)
  {
    Canvas(xlim=xlim,mar=mar, main = main, bg = bg)
  }
  for(j in 1:length(obj$shape))
  {
    if(obj$visible[[j]]==1)
    {
      if(grepl("line",obj$shade[[j]][1]))
      {
        elements<-decof(obj)
        plotting_lines<-which(obj$visible==1 & grepl("line",unlist(obj$shade)))
        for(ll in 1:length(plotting_lines)){
          line(elements[[plotting_lines[[ll]]]],obj$shade[[j]][1]) #Pejo tacon che sbrego
          
        }
        obj$shade[[j]][1] <- NA
      }
      if(obj$num[[j]][1]==1){
        
        DrawRegPolygon(x = obj$pos.x[[j]], y = obj$pos.y[[j]], rot = obj$rotation[[j]],
                       radius.x = obj$size.x[[j]], radius.y = obj$size.y[[j]], nv = obj$nv[[j]],
                       lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]])
        
        
      }else{
        
        DrawCircle(x = obj$pos.x[[j]], y = obj$pos.y[[j]],
                   r.out = obj$size.x[[j]],r.in= obj$size.y[[j]], theta.1=obj$theta.1[[j]],
                   theta.2=obj$theta.2[[j]], nv = obj$nv[[j]],
                   lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]])
        
      }
    }
  }
}



replace <- function(obj,index,obj2) {
  UseMethod("replace")
}

replace.field<-function(obj,index,obj2)
{
  for(i in 1:length(obj))
  {
    obj[[i]][[index]]<-obj2[[i]][[1]]
  }
  return(obj)
}

hide<- function(obj,index) {
  UseMethod("hide")
}

hide.field<-function(obj,index="Full")
{
  if(any(index=="Full"))
  {
    index<-1:length(obj$shape)
  }
  obj$visible[index]<-integer(length(index))
  return(obj)
}

show<- function(obj,index) {
  UseMethod("show")
}

show.field<-function(obj,index="Full")
{
  if(any(index=="Full"))
  {
    index<-1:length(obj$shape)
  }
  obj$visible[index]<-rep(1,length(index))
  return(obj)
}


decof<- function(obj) {
  UseMethod("decof")
}

decof.field<-function(obj)
{
  if(length(obj$shape)==1){
    nobj<-length(obj$nv[[1]])
  }else{
    nobj<-length(obj$nv)
  }
  
  if(nobj!=length(obj$shape)){
    name<-rep("token",nobj)
  }else{
    name<-obj$shape
  }
  newobj<-list()
  for(i in 1:nobj)
  {
    ele<-list(
      shape = name[i],
      size.x = obj$size.x[i],
      size.y = obj$size.y[i],
      theta.1 =obj$theta.1[i],
      theta.2 =obj$theta.2[i],
      rotation = obj$rotation[i],
      pos.x = obj$pos.x[i],
      pos.y = obj$pos.y[i],
      lty =obj$lty[i],
      lwd = obj$lwd[i],
      num = obj$num[i],
      nv = obj$nv[i],
      shade =obj$shade[i],  
      visible = obj$visible[i], 
      tag = obj$tag[i]
    )
    attr(ele, "class") <- "field"
    newobj[[i]]<-ele
  }
  return(newobj)
}

#Esempio di applicazione
#shapes_list("Shapes_list-07-11-Ottavia.R")

shapes_list<-function(filename)
{
  
  source(filename,local=TRUE)
  rm(filename)
  name <- ls()
  
  table<-data.frame(
    name=name,
    num_shapes=integer(length(name)),
    small=logical(length(name)),
    fill= logical(length(name)),
    rotate= logical(length(name))
  )
  for(r in 1:length(name))
  {
    f<-get(name[r])
    obj<-f()
    tags<-unlist(obj$tag)
    
    if(any(tolower(tags)=="simple"))
    {
      table$num_shapes[r] <- 1
    }else if(any(tolower(tags)=="compose2"))
    {
      table$num_shapes[r] <- 2
    }else if(any(tolower(tags)=="compose4"))
    {
      table$num_shapes[r] <- 4
    }
    table$fill[r]<-any(tolower(tags)=="fill")
    table$small[r]<-any(tolower(tags)=="small")
    table$rotate[r]<-any(tolower(tags)=="rotate")
  }
  return(table)
}

draw.Raven_matrix<- function(obj, main = NULL, 
                             hide = FALSE, 
                             n.cell = 9, 
                             bg = "white") { ###Definito Draw per i field si può semplificare questa
  
  library(DescTools)
   if (n.cell == 9) {
     par(mfrow = c(3, 3), mar = c(0.5, 6, 0.5, 2) + .1, 
         mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )
     squares <- paste0("Sq", 1:9)
     
   } else if (n.cell == 4) {
     squares <- paste0("Sq", c(1,2, 4,5))
     par(mfrow = c(2, 2), mar = c(0.5, 6, 0.5, 2) + .1, 
         mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )
   }
  

  if (hide == TRUE){n.cell<-n.cell-1}
    for (i in 1:n.cell)
    {
      #Fixing the plot area for each cells
      Canvas(xlim=16,mar=c(1,1,1,1), main = main, bg = bg)
      #Canvas(16,16)
      draw(obj[[squares[[i]]]],canvas = FALSE)
      
    }
  # } else {
  #   for (i in 1:(length(squares)-1))
  #   {
  #     #Fixing the plot area for each cells
  #     Canvas(xlim=16,mar=c(1,1,1,1), main = main, bg = bg)
  #     #Canvas(16,16)
  #     draw(obj[[squares[[i]]]],canvas = FALSE)
  #     
  #   }
  #}
 
}


found_points<-function(x1,x2,y1,y2,m1,q1){
  delta_y<- (y2 - y1)
  delta_x<- (x2 - x1)
  if(round(x2,3)==round(x1,3)){
    y<-m1*x1+q1
    x<-x1
  }else if(round(y1,3)==round(y2,3)){
    y<-y1
    x<-(y1-q1)/m1
  }else{
    m2 <-  delta_y / delta_x
    q2 <- y1-x1*m2
    x <- (q1 - q2) / (m2 - m1)
    y <- x*m2+q2
  }
  return(c(x,y))
}

filling<-function(q,m,coords,lwd=1)
{
  smooth<-19
  pt<-matrix(ncol=2,nrow = 2)
  n_solu<-1
  index<-c(1:length(coords$x),1)
  for(i in 2:length(index))
  {
    solution<-found_points(coords$x[index[i-1]],coords$x[index[i]],
                           coords$y[index[i-1]],coords$y[index[i]],
                           m,q)
    
    control_x<-round(min(coords$x[index[i-1]],coords$x[index[i]]),smooth)<=solution[1] &
      round(max(coords$x[index[i-1]],coords$x[index[i]]),smooth)>=solution[1]
    control_y<-round(min(coords$y[index[i-1]],coords$y[index[i]]),smooth)<=solution[2] &
      round(max(coords$y[index[i-1]],coords$y[index[i]]),smooth)>=solution[2]

    if(control_x && control_y && n_solu<=2)
    {
      if(n_solu==1){
        pt[n_solu,]<-solution
        n_solu<-n_solu+1 
      }else if(!all(pt[n_solu-1,]==solution))
      {
        pt[n_solu,]<-solution
        n_solu<-n_solu+1
      }
    }
  }
 # cat(pt[,1],pt[,2],"\n")
  polygon(pt[,1],pt[,2],lwd=lwd)
  return(pt)
}

line<-function(obj,rule,lwd=1,by=2.5)
{
  #coefficente angolare di default
  m<-1
  if(obj$num==1)
  {
    coords<-DrawRegPolygon(x = obj$pos.x[[1]], y = obj$pos.y[[1]], rot = obj$rotation[[1]],
                           radius.x = obj$size.x[[1]], radius.y = obj$size.y[[1]],
                           nv = obj$nv[[1]],plot = F)
  }else{
    coords<-DrawCircle(x = obj$pos.x[[1]], y = obj$pos.y[[1]], theta.1 =  obj$theta.1[[1]],
                       theta.2 =  obj$theta.2[[1]], r.out = obj$size.x[[1]], 
                       r.in = obj$size.y[[1]],nv = obj$nv[[1]],plot = F)
    coords<-coords[[1]]
  }

  # coords<-matrix(c(coords$x,coords$y),ncol = 2)
  
  if(grepl("both",rule))
  {
    m=c(-1,1)
  }else if(grepl("inv",rule))
  {
    m=-1
  }else if(grepl("h",rule))
  {
    m=0
  }
 
  for(j in 1:length(m)){
    if(grepl("1",rule))
    {
      first_coords<-coords
      first_coords$x[coords$x<=obj$pos.x[[1]]]<-obj$pos.x[[1]]
      for(i in seq(-30 ,30,by=by)){
        filling(q=i,m=m[j],coords=first_coords,lwd = lwd) #,p.x=obj$pos.x[[1]],p.y=obj$pos.y[[1]], rule)
      } 
    }
    if(grepl("2",rule))
    {
      first_coords<-coords
      first_coords$x[coords$x>=obj$pos.x[[1]]]<-obj$pos.x[[1]]
      for(i in seq(-30 ,30,by=by)){
        filling(q=i,m=m[j],coords=first_coords,lwd = lwd) #,p.x=obj$pos.x[[1]],p.y=obj$pos.y[[1]], rule)
      } 
    }
  }
  
}
