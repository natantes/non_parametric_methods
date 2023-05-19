
setwd("/")

### PROBLEM 1

library(sm)
geyser_data <- read.csv("./Dev/non_parametric_methods/data/geyser.csv")
sm.density(geyser_data)
sm.density(geyser_data, display="contour")
sm.density(geyser_data, method="cv")
sm.density(geyser_data, method="cv", display="contour")


### PROBLEM 2

library(sm)
bills_df <- read.csv("./Dev/non_parametric_methods/data/bills.csv")
forged <- bills_df[bills_df$real == 0,]
reall <- bills_df[bills_df$real == 1,]
invisible(lapply(forged[,c('width','len')], as.numeric))
invisible(lapply(real[,c('width','len')], as.numeric))
forged <- subset(forged, select = -c(real) );
reall <- subset(reall, select = -c(real) );

denF <- sm.density(forged, method="cv", display="contour", 
           xlim=c(7, 12.5), ylim=c(138.75, 142.25))
denR <- sm.density(reall, method="cv", display="contour", 
                   xlim=c(7, 12.5), ylim=c(138.75, 142.25))

prob_forged <- denF$estimate/(denF$estimate + denR$estimate)

contour(x=denF$eval.points[,1], y=denR$eval.points[,2], z=prob_forged)

library(pracma)
interp2(x=denF$eval.points[,1], y=denR$eval.points[,2], Z=t(prob_forged),
        xp=8, yp=140.2, method="linear")
interp2(x=denF$eval.points[,1], y=denR$eval.points[,2], Z=t(prob_forged),
        xp=9, yp=140.5, method="linear")
interp2(x=denF$eval.points[,1], y=denR$eval.points[,2], Z=t(prob_forged),
        xp=9.8, yp=140.3, method="linear")

### PROBLEM 3

library(sm)
motor_data <- read.csv("./Dev/non_parametric_methods/data/motor.csv")
sm.regression(motor_data$accel, motor_data$time, h=2.5)
sm.regression(motor_data$accel, motor_data$time, h=5)
sm.regression(motor_data$accel, motor_data$time, h=7.5)
sm.regression(motor_data$accel, motor_data$time, h=10)
sm.regression(motor_data$accel, motor_data$time, h=12.5)
sm.regression(motor_data$accel, motor_data$time, h=15)
sm.regression(motor_data$accel, motor_data$time, method="cv")

### PROBLEM 4

library(sm)
geyser_data <- read.csv("./Dev/non_parametric_methods/data/geyser.csv")
out <- sm.regression(geyser_data$waiting, geyser_data$duration, method="cv", se=T)

h2df <- function(h_target, x, y){
   f<-function(df){
   sm.regression(x, y, df=df, display="none")$h - h_target
  } 
  hmin<-sm.regression(x, y, df=length(x), display="none")$h
  hmax<-sm.regression(x, y, df=2.01, display="none")$h
  if ((h_target<= hmax)&(h_target >= hmin)){
  dfroot<-uniroot(f, c(2.01, length(x)), tol=0.005)$root
  return(round(dfroot, 2))
  }
  else return("out of range")}

h <- sm.regression(geyser_data$waiting, geyser_data$duration, method="cv", se=T)$h
h2df(h, geyser_data$waiting, geyser_data$duration)

### PROBLEM 5

lidar_data <- read.csv("./Dev/non_parametric_methods/data/lidar.csv")
out <- sm.regression(lidar_data$range, lidar_data$logratio, method="cv")

approx(out$eval.points, out$estiamte, xout=500)
approx(out$eval.points, out$estiamte, xout=550)
approx(out$eval.points, out$estiamte, xout=600)

h2df <- function(h_target, x, y){
  f<-function(df){
    sm.regression(x, y, df=df, display="none")$h - h_target
  } 
  hmin<-sm.regression(x, y, df=length(x), display="none")$h
  hmax<-sm.regression(x, y, df=2.01, display="none")$h
  if ((h_target<= hmax)&(h_target >= hmin)){
    dfroot<-uniroot(f, c(2.01, length(x)), tol=0.005)$root
    return(round(dfroot, 2))
  }
  else return("out of range")}

h <- sm.regression(lidar_data$range, lidar_data$logratio, method="cv")$h
h2df(h, lidar_data$range, lidar_data$logratio)
