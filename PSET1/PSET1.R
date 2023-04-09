
### PROBLEM 3

set.seed(32923)
y <- rnorm(5, mean=0, sd=1)
y_mat <- matrix(rnorm(5*100000, mean=0, sd=1), 100000, 5)
y_b <- apply(y_mat, 1, sd)
y_b
s <- mean(y_b)
s_0 <- s * sqrt( (length(y_b) - 1) / (length(y_b)) )
s_1 <- s * sqrt( (length(y_b) - 1) / (length(y_b) - 1.5) )

bias_s <- s - 1
bias_s_0 <- s_0 - 1
bias_s_1 <- s_1 - 1

bias_s
bias_s_0
bias_s_1

### PROBLEM 4

software_data <- read.csv("./stat_352-0/software.csv")
software_data <- software_data[,1]
lambda <- 1 / mean(software_data)

y<-seq(from=0,to=10,by= 0.001)
plot(ecdf(software_data), verticals=T, cex=0.5)
# plot(pexp(y, rate = lambda), type="l", xlab="y", ylab="F(y)")

log_software_data <- log(software_data)
log_software_data

log_mean <- mean(log_software_data)
log_sd <- sd(log_software_data)
plot(plnorm(y, meanlog = log_mean, sdlog = log_sd))
plot(ecdf(software_data), verticals=T, add=T, cex=0.5)


### PROBLEM 5

peabody_data <- read.csv("./stat_352-0/Peabody.csv")
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

software_data <- read.csv("./stat_352-0/software.csv")
software_data <- software_data[,1]

software_mean <- mean(software_data)
software_sd <- sd(software_data)

t_calculator <- function(x, ind){mean(software_data[ind]) / sd(software_data)}

t <- t_calculator(software_data, 1:length(software_data))
t

library(boot)
boot(software_data, t_calculator, 100000)

