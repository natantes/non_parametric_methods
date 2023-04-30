

setwd("/")

### PROBLEM 1

crustacean_data <- read.csv("./Dev/non_parametric_methods/PSET2/crustaceans.csv")
crustacean_data <- as.numeric(crustacean_data[,1])
hist(crustacean_data, breaks=20, freq=F)


### PROBLEM 1

software_data <- read.csv("./Dev/non_parametric_methods/PSET2/software.csv")
software_data <- as.numeric(software_data[,1])
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
plot(density(geyser_data$duration, bw="nrd"), main="Kernel Density Estimate of Software Data (Smoothing Paramater with)")
plot(density(geyser_data$duration, bw="nrd0"), main="Kernel Density Estimate of Software Data (Smoothing Paramater with)")
plot(density(geyser_data$duration, bw="SJ"), main="Kernel Density Estimate of Software Data (Smoothing Paramater with)")
plot(density(geyser_data$duration, bw="ucv"), main="Kernel Density Estimate of Software Data (Smoothing Paramater with)")

### PROBLEM 4

### PROBLEM 5

