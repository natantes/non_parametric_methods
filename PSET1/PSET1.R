
### PROBLEM 3

set.seed(32923)
y <- rnorm(5, mean=0, sd=1)
y_mat <- matrix(rnorm(5*100000, mean=0, sd=1), 100000, 5)
y_b <- apply(y_mat, 1, sd)

s <- mean(y_b)
s_0 <- s * sqrt( (5 - 1) / (5) )
s_1 <- s * sqrt( (5 - 1) / (5 - 1.5) )
cat("estimates: ", s, s_0, s_1)

bias_s <- s - 1
bias_s_0 <- s_0 - 1
bias_s_1 <- s_1 - 1
cat("bias: ", bias_s, bias_s_0, bias_s_1)



### PROBLEM 4

software_data <- read.csv("./Dev/non_parametric_methods/PSET1/software.csv")
software_data <- software_data[,1]
lambda <- 1 / mean(software_data)

y<-seq(from=0,to=120,by= 0.01)

log_software_data <- log(software_data)

log_mean <- mean(log_software_data)
log_sd <- sd(log_software_data)
plot(y, pexp(y, rate = lambda), type="l", xlab="y", ylab="F(y)")
plot(ecdf(software_data), verticals=T, cex=0.5, xlab="y", ylab="F(y)", add=T)
lines(y, plnorm(y, meanlog = log_mean, sdlog = log_sd), type="l", lty=3)



### PROBLEM 5

peabody_data <- read.csv("./Dev/non_parametric_methods/PSET1/Peabody.csv")
peabody_data <- peabody_data[,1]

peabody_mean <- mean(peabody_data)
peabody_sd <- sqrt(var(peabody_data) / length(peabody_data))

peabody_mean
peabody_sd

peabody_median <- median(peabody_data)
peabody_median

set.seed(32923)
Median <- function(x, ind){median(x[ind])}
Mean_Median_Diff <- function(x, ind){mean(x[ind]) - median(x[ind])}

library(boot)
boot(peabody_data, Median, 100000)
boot(peabody_data, Mean_Median_Diff, 100000)



### PROBLEM 6

software_data <- read.csv("./Dev/non_parametric_methods/PSET1/software.csv")
software_data <- software_data[,1]

software_mean <- mean(software_data)
software_sd <- sd(software_data)

t_calculator <- function(x, ind){mean(software_data[ind]) / sd(software_data)}

t <- t_calculator(software_data, 1:length(software_data))
t

library(boot)
boot(software_data, t_calculator, 100000)

