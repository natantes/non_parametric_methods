

setwd("/")
# clipboard is different for mac
scan(file=file.choose())


### PROBLEM 1

software_data <- scan("./Dev/non_parametric_methods/PSET2/software.csv")
dim(software_data)
head(software_data)
plot(density(software_data, bw=1, kernel="g"), main="Kernel Density Estimate of Software Data (Smoothing Paramater=1)")
plot(density(software_data, bw=4, kernel="g"), main="Kernel Density Estimate of Software Data (Smoothing Paramater=4)")
plot(density(software_data, bw=8, kernel="g"), main="Kernel Density Estimate of Software Data (Smoothing Paramater=8)")
plot(density(software_data, bw=14, kernel="g"), main="Kernel Density Estimate of Software Data (Smoothing Paramater=14)")

### PROBLEM 2

geyser_data <- read.csv("./Dev/non_parametric_methods/data/geyser.csv")
density(geyser_data$duration, bw="nrd", kernel="g")$bw
density(geyser_data$duration, bw="nrd0", kernel="g")$bw
density(geyser_data$duration, bw="SJ", kernel="g")$bw
density(geyser_data$duration, bw="ucv", kernel="g")$bw
plot(density(geyser_data$duration, bw="nrd"), main="Kernel Density Estimate of Geyser Data (nrd)")
plot(density(geyser_data$duration, bw="nrd0"), main="Kernel Density Estimate of Geyser Data (nrd0)")
plot(density(geyser_data$duration, bw="SJ"), main="Kernel Density Estimate of Geyser Data (SJ)")
plot(density(geyser_data$duration, bw="ucv"), main="Kernel Density Estimate of Geyser Data (ucv)")

### PROBLEM 4
y<-seq(from=14,to=23,by= 0.001)
forearm_data <- read.csv("./Dev/non_parametric_methods/data/forearm.csv")
forearm_data <- as.numeric(forearm_data[,1])
plot(y, dnorm(y, mean=mean(forearm_data), sd=sd(forearm_data)), type="l", lty=3, add=T)
lines(density(forearm_data, bw="SJ"), main="Kernel Density Estimate of Software Data (Smoothing Paramater with)")

library(GoFKernel)
fan.test(forearm_data, fun.den=dnorm, par=list(mean=mean(forearm_data), sd=sd(forearm_data)), bw=bw.SJ(forearm_data), lower=min(forearm_data) - 1, upper=max(forearm_data) + 1)

### PROBLEM 5

rats_data <- read.csv("./Dev/non_parametric_methods/data/rats.csv")
time <- rats_data$time
diet <- rats_data$diet
diet_1 <- time[diet == 1]
diet_0 <- time[diet == 0]
h_1 <- bw.SJ(diet_1)
h_0 <- bw.SJ(diet_0)

h_star <- ( 2 / ((1/h_1) + (1/h_0)) )
h_star

library(sm)
sm.density.compare(time, group=diet, model="equal", bw=h_star, nboot=10000)

### PROBLEM 6

blood_data <- read.csv("./Dev/non_parametric_methods/data/blood.csv")
grp <-blood_data$grp
den_diease<-density(blood_data$tri[grp == 1], bw=bw.SJ(blood_data$tri[grp == 1]), from=0, to=300)
den_healthy<-density(blood_data$tri[grp == 0], bw=bw.SJ(blood_data$tri[grp == 0]), from=0, to=300)
prob_diease<-(den_diease$y)/(den_healthy$y + den_diease$y)

approx(den_diease$x, prob_diease, xout=100)
approx(den_diease$x, prob_diease, xout=200)
approx(den_diease$x, prob_diease, xout=250)

den_diease<-density(blood_data$tri[grp == 1], bw=bw.SJ(blood_data$tri[grp == 1]), from=0, to=300, adjust=3)
den_healthy<-density(blood_data$tri[grp == 0], bw=bw.SJ(blood_data$tri[grp == 0]), from=0, to=300, adjust=3)
prob_diease<-(den_diease$y)/(den_healthy$y + den_diease$y)

approx(den_diease$x, prob_diease, xout=100)
approx(den_diease$x, prob_diease, xout=200)
approx(den_diease$x, prob_diease, xout=250)

plot(den_diease$x, prob_diease)
# ?density


