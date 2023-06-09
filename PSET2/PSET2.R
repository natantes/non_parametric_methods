

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
geyser_data <- as.numeric(geyser_data[1])
density(geyser_data, bw="nrd", kernel="g")$bw
density(geyser_data, bw="nrd0", kernel="g")$bw
density(geyser_data, bw="SJ", kernel="g")$bw
density(geyser_data, bw="ucv", kernel="g")$bw

### PROBLEM 4
# None

### PROBLEM 5
hist(software_data,  breaks="FD", freq=F)
