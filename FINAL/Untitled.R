
### setwd("~/")

### PROBLEM 1
set.seed(3522023)

ice_data <- scan("./Dev/non_parametric_methods/data/ice.csv")
plot(density(ice_data, bw=1, kernel="g"), main="Kernel Density Estimate of Ice Data (Smoothing Paramater=1)")
plot(density(ice_data, bw=4, kernel="g"), main="Kernel Density Estimate of Ice Data (Smoothing Paramater=4)")
plot(density(ice_data, bw=6, kernel="g"), main="Kernel Density Estimate of Ice Data (Smoothing Paramater=6)")
plot(density(ice_data, bw=8, kernel="g"), main="Kernel Density Estimate of Ice Data (Smoothing Paramater=8)")
plot(density(ice_data, bw=14, kernel="g"), main="Kernel Density Estimate of Ice Data (Smoothing Paramater=14)")

skewness_calc <- function(data, ind) {
  sum <- 0
  for (num in data[ind]) {
    sum <- sum + (num - mean(data[ind]))^3
  }
  sum <- sum / length(data[ind])
  sum <- sum / (sd(data[ind]))^(3/2)
  return(sum)
}

test_stat <- skewness_calc(ice_data, 1:length(ice_data))
test_stat

test_stat <- skewness_calc(y, 1:length(ice_data))
test_stat

library(boot)
boot(ice_data, skewness_calc, R=100000)

ice_data <- log(ice_data)

plot(density(ice_data, bw=1, kernel="g"), main="Kernel Density Estimate of Ice Data (Smoothing Paramater=1)")
plot(density(ice_data, bw=4, kernel="g"), main="Kernel Density Estimate of Ice Data (Smoothing Paramater=4)")
plot(density(ice_data, bw=6, kernel="g"), main="Kernel Density Estimate of Ice Data (Smoothing Paramater=6)")
plot(density(ice_data, bw=8, kernel="g"), main="Kernel Density Estimate of Ice Data (Smoothing Paramater=8)")
plot(density(ice_data, bw=14, kernel="g"), main="Kernel Density Estimate of Ice Data (Smoothing Paramater=14)")

test_stat <- skewness_calc(ice_data, 1:length(ice_data))
test_stat

test_stat <- skewness_calc(y, 1:length(ice_data))
test_stat

boot(ice_data, skewness_calc, R=100000)


### PROBLEM 2

set.seed(3522023)
peabody_data <- read.csv("./Dev/non_parametric_methods/data/Peabody.csv")
peabody.d <- subset(peabody_data, disadv == 1)
peabody.nd <- subset(peabody_data, disadv == 0)
# peabody.d <- peabody_data[disadv == 1]
# peabody.nd <- peabody_data[disadv == 0]

# plot(density(peabody.d$peabody, bw="SJ", kernel="g"), main="Kernel Density Estimate of Peabody Data for 
#      Disadvantaged (Smoothing Paramater Chosen Using Sheather-Jones)")
# plot(density(peabody.nd$peabody, bw="SJ", kernel="g"), main="Kernel Density Estimate of Peabody Data for 
#      Non-Disadvantaged (Smoothing Paramater Chosen Using Sheather-Jones)")

h_1 <- bw.SJ(peabody.d$peabody)
h_0 <- bw.SJ(peabody.nd$peabody)
h_star <- ( 2 / ((1/h_1) + (1/h_0)) )

library(sm)
sm.density.compare(peabody_data$peabody, group=peabody_data$disadv, model="equal", bw=h_star, nboot=10000)



