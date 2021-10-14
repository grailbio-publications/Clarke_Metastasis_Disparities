library(diagram)


plot_null_scenario<-function(){
  delta_x<-0.2
  lrad<-0.04
  ylevel=0.95
  yoffset=0.75
  
  xstart<-0.1
  
  xcoord<-xstart+delta_x*1:4
  
  slabel<-c("I","II","III","IV")
  
  text(0.01,(ylevel+yoffset)/2,"Base",cex=2,pos=4)
  
  text(xstart+0.4*delta_x,ylevel,"Stage")
  
  for (ii in 1:3){
    straightarrow(from=c(xcoord[ii],ylevel),to=c(xcoord[ii+1],ylevel))
  }
  for (ii in 1:4){
    straightarrow(from=c(xcoord[ii],ylevel),to=c(xcoord[ii],yoffset))
  }
  
  for (ii in 1:length(slabel)){
    textellipse(c(xcoord[ii],ylevel),radx=lrad,rady=lrad,lab=slabel[ii])
  }
  
  text(xstart+0.4*delta_x,yoffset,"Mortality")
  
  for (ii in 1:length(slabel)){
    textrect(c(xcoord[ii],yoffset),radx=lrad,lab=slabel[ii])
  }
  rect(0.01,ylevel+1.5*lrad,0.99,yoffset-1.5*lrad,lwd=2)
}

plot_one_scenario<-function(){
  delta_x<-0.2
  lrad<-0.04
  ylevel=0.6
  yoffset=0.4
  
  xstart<-0.1
  
  xcoord<-xstart+delta_x*1:4
  text(0.01,(ylevel+yoffset)/2,"IV->III",cex=2,pos=4)
  
  slabel<-c("I","II","III","IV")
  text(xstart+0.4*delta_x,ylevel,"Stage")
  
  for (ii in 1:2){
    straightarrow(from=c(xcoord[ii],ylevel),to=c(xcoord[ii+1],ylevel))
  }
  for (ii in 1:3){
    straightarrow(from=c(xcoord[ii],ylevel),to=c(xcoord[ii],yoffset))
  }
  straightarrow(from=c(xcoord[4],ylevel),to=c(xcoord[3],yoffset))
  
  for (ii in 1:length(slabel)){
    textellipse(c(xcoord[ii],ylevel),radx=lrad,rady=lrad,lab=slabel[ii])
  }
  
  text(xstart+0.4*delta_x,yoffset,"Mortality")
  
  for (ii in 1:length(slabel)){
    textrect(c(xcoord[ii],yoffset),radx=lrad,lab=slabel[ii])
  }
  rect(0.01,ylevel+1.5*lrad,0.99,yoffset-1.5*lrad,lwd=2)
}

plot_two_scenario<-function(){
  delta_x<-0.2
  lrad<-0.04
  ylevel=0.25
  yoffset=0.05
  
  xstart<-0.1
  
  xcoord<-xstart+delta_x*1:4
  
  text(0.01,(ylevel+yoffset)/2,"IV->III,II,I",cex=2,pos=4)
  slabel<-c("I","II","III","IV")
  text(xstart+0.4*delta_x,ylevel,"Stage")
  
  for (ii in 1:2){
    straightarrow(from=c(xcoord[ii],ylevel),to=c(xcoord[ii+1],ylevel))
  }
  for (ii in 1:3){
    straightarrow(from=c(xcoord[ii],ylevel),to=c(xcoord[ii],yoffset))
  }
  for (ii in 1:3){
    straightarrow(from=c(xcoord[4],ylevel),to=c(xcoord[ii],yoffset))
  }
  
  for (ii in 1:length(slabel)){
    textellipse(c(xcoord[ii],ylevel),radx=lrad,rady=lrad,lab=slabel[ii])
  }
  
  text(xstart+0.4*delta_x,yoffset,"Mortality")
  
  for (ii in 1:length(slabel)){
    textrect(c(xcoord[ii],yoffset),radx=lrad,lab=slabel[ii])
  }
  rect(0.01,ylevel+1.5*lrad,0.99,yoffset-1.5*lrad,lwd=2)
}


#need to choose appropriate dimensions
pdf(sprintf("figs/%s_scenario_diagram.pdf",date_code),
    width=9,height=10)
openplotmat(asp=1,main="Scenarios Stopping Before Metastasis")
plot_null_scenario()
plot_one_scenario()
plot_two_scenario()
box()
dev.off()

