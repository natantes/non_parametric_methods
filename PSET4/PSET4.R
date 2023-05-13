

### PROBLEM 1

library(sm)
geyser_data <- read.csv("./Dev/non_parametric_methods/data/geyser.csv")
sm.density(geyser_data)
sm.density(geyser_data, display="contour")
sm.density(geyser_data, method="cv")
sm.density(geyser_data, method="cv", display="contour")


### PROBLEM 4

library(sm)
geyser_data <- read.csv("./Dev/non_parametric_methods/data/geyser.csv")
sm.regression(geyser_data$waiting, geyser_data$duration, method="cv", se=T)

h2df<- function(h_target, x, y){
   f<-function(df){
   sm.regression(x, y, df=df, display="none")$h - h_target
  } hmin<-sm.regression(x, y, df=length(x), display="none")$h
  hmax<-sm.regression(x, y, df=2.01, display="none")$h
  if ((h_target<= hmax)&(h_target >= hmin)){
  dfroot<-uniroot(f, c(2.01, length(x)), tol=0.005)$root
  return(round(dfroot, 2))
  }
  else return("out of range")
  }