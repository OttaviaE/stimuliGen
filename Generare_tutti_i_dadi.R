current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
cat("\014")
rm(list = ls())

source("Shapes_list-10-11-Ottavia.R")
source("Class and Methods v02.R")
source("Rules_27102022.R")

shapes<-shapes_list("Shapes_list-10-11-Ottavia.R")

shapes_riducible<-shapes[shapes$small&shapes$num_shapes==1,]
for(i in 1:nrow(shapes_riducible))
{
  f<-get(shapes_riducible$name[i])
  object<-movement(f(),1,"pos",20,12)
  object<-size(object,4)

  object2<-object
  for(row in 1:3)
  {
    obj<-movement(object,row,"y")
    object2<-cof(obj,object2)
    for( col in 2:3 )
    {
      obj<-movement(obj,2,"x")
      object2<-cof(obj,object2)
      
    }
  }
  cof(object2,name=paste0("multi",shapes_riducible$name[i],
                          collapse = ""))
}



