

setwd("/")

### PROBLEM 1

crustacean_data <- read.csv("./Dev/non_parametric_methods/PSET2/crustaceans.csv")
crustacean_data <- as.numeric(crustacean_data[,1])
hist(crustacean_data, breaks=20, freq=F)


### PROBLEM 2

software_data <- read.csv("./Dev/non_parametric_methods/PSET2/software.csv")
software_data <- as.numeric(software_data[,1])
hist(software_data, freq=F)
hist(software_data, breaks=5, freq=F)
hist(software_data,  breaks=15, freq=F)
hist(software_data,  breaks=20, freq=F)

### PROBLEM 4

### PROBLEM 5
hist(software_data,  breaks="FD", freq=F)
