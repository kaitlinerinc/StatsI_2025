#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Run the following commands
install.packages("car")
library(car)
data(Prestige)
help(Prestige)

#Create a new variable "professional"
professional <- ifelse(Prestige$type == "prof", 1, 0)
append(Prestige, professional)

linear_model <- lm(prestige ~ income + professional + income:professional, data = Prestige)
linear_model

#math Question 1
(0.0031709 - 0.0023257) * 1000

37.7812800 + (-0.0023257) * 6000



#Question 2
test_statistic <- 0.042/0.016
test_statistic

n <- 30
p <- 2
degrees_freedom <- n - p - 1
degrees_freedom
p_value <- 2 * pt(test_statistic, df=degrees_freedom, lower.tail = FALSE)
p_value

test_statistic_2 <- 0.042/0.013
test_statistic_2

n2 <- 76
df2 <- n2 - p - 1
df2

p2 <- 2 * pt(test_statistic_2, df=df2, lower.tail = FALSE)
p2
