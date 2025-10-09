#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list = ls())
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
# lapply(c("stringr"),  pkgTest)
lapply(c(), pkgTest) 

library(ggplot2)
library(GGally)
library(tidyverse)
library(knitr)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Steps to make a confidence interval for y

#Question 1: Education

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Steps to make a confidence interval for y

#Step 1: Calculate the sample mean. 
#The sample mean equals the sum of the values divided by the number of values.
n <- length(y)
mean_y <- sum(y)/n

#or you can use the mean function
mean(y)

#Step 2: Begin Calculating the standard deviation by finding how much each data 
#point differs from the mean and squaring each value.
distance_from_mean <- y - mean(y)

distance_from_mean_squared <- distance_from_mean^2

#Add the squared values to find the sum of squares
sum_of_squares <- sum(distance_from_mean_squared)

#Find the variance by dividing the sum of squared by
#the degrees of freedom (the number of values minus 1)
variance <- sum_of_squares/(n-1)

#Find the standard deviation (the square root of variance)
standard_deviation_y <- sqrt(variance)
standard_deviation_y

#Step 3: Calculate the standard error, which equals the standard deviation 
#divided by the square root of n(the number of observations in the sample)

standard_error_y <- standard_deviation_y/(sqrt(n))
standard_error_y

#Step 4: Calculate the t-score by inputting: 
#(1 minus the confidence coefficient divided by 2)
#The degrees of freedom (n-1)

t_score <- qt(p=(.1/2), n-1)
t_score

# Step 6: Calculate the lower limit of the confidence interval
# by doing the sample mean + (t-score * standard error)

lower_limit <- mean(y) + (t_score * standard_error_y)
lower_limit

# Step 7: Calculate the upper limit of the confidence interval
# by doing the sample mean - (t-score * standard error)

upper_limit <- mean(y) - (t_score * standard_error_y)
upper_limit

#Conclusion: Based on the confidence interval, we are 90 
#percent confident that the true mean of IQ at the school
#is between 93.95993 and 102.9201.

#Question 1: Part 2

#Step 1: Assumptions
# n < 30, so I will perform a t-test instead of a z-test.
#This is because I cannot assume a normal distribution

#Step 2: State Hypotheses
#Alternative Hypothesis: The average student IQ in the school 
#counselor's school is higher than 100

#Null Hypothesis: The average student IQ in the school 
#counselor's school is less than or equal to 100

#Step 3: Calculate the test statistic 
#(sample mean - population mean)/(sample standard error)
test_statistic <- (mean(y) - 100)/(standard_error_y)
test_statistic

pt(test_statistic, df=24, lower.tail=FALSE)

#The calculated p value is greater than the critical value of 0.05. 
#Therefore, we fail to reject the null hypothesis that the average student IQ 
#in the school counselor's school is less than or equal to 100.

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)


#Question 2: Political Economy

#form/pattern
#direction
#strength
#outliers

#Plot the relationships among Y, X1, X2, and X3?
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
ggpairs(data=expenditure, columns = 2:5, title = "U.S. Social Welfare Expenditures")

#Per capita personal income in state (X1) and per capita expenditure on shelters/housing 
#assistance in state (Y) are positively correlated. The form is moderately spread out. 
#A correlation coefficient of 0.532 indicates that it is a moderate positive correlation.

#per capita expenditure on shelters/housing assistance in state (Y) is positively correlated 
#with number of residents per 100,000 that are ”financially insecure” in state (X2). The form
#is relatively tight. The form appears to curve indicating that a linear measurement may be
#the best way of analyzing this data. A correlation coefficient of 0.448 indicates a moderate
#positive linear correlation

#Per capita personal income in state (X1) is positively correlated with the number of residents per 
#100,000 that are ”financially insecure” in state (X2). The form
#is relatively spread out A correlation coefficient of 0.221 suggests there is a weak correlation.

#per capita expenditure on shelters/housing assistance in state (Y) is positively correlated with 
#the number of people per thousand residing in urban areas in state (X3). The form is moderately
#tight. A correlation coefficient of 0.464 suggests a moderate positive correlation.

#Per capita personal income in state (X1) is positively correlated with 
#the number of people per thousand residing in urban areas in state (X3). The distribution
#appears relatively tight. A correlation coefficient of 0.595 indicates a moderate positive
#association. This is the largest correlation coefficient and the tightest grouping
#that we see among variables in this data set.

#the number of residents per 100,000 that are ”financially insecure” in state (X2) and 
#the number of people per thousand residing in urban areas in state (X3) are positively
#correlated. The distribution appears very spread out. A correlation coefficient of
#0.221 indicates a weak/negligible positive correlation.

#Plot the relationship between Y and Region

ggplot(expenditure) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(aes(x = as.factor(Region), y=Y)) +
  labs(x="Region", y="Per capita expenditure on shelters/housing assistance in state (Y)",
  title = "Per Capita Expenditure on Shelters/Housing by Region") +
  scale_x_discrete(labels=c("1" = "Northeast", "2" = "North Central", "3" = "South", "4" = "West"))


#On average, the West has the highest per capita expenditure on 
#housing assistance. The North Central region has the next highest 
#average expenditure, followed by the Northeast. Lastly, the South 
#has the lowest average per capita expenditure on housing assistance.

#Please plot the relationship between Y and X1
ggplot(expenditure, aes(x=X1, y=Y)) +
  geom_point(size=2) +
  labs(x="Per capita personal income in state (X1)", 
       y="Per capita expenditure on shelters/housing assistance in state (Y)", 
       title = "Personal Income and Expenditure on Housing Assistance")


#Reproduce the above graph including one more variable Region and display
#different regions with different types of symbols and colors.
ggplot(expenditure, aes(x=X1, y=Y, color=factor(Region), shape=factor(Region))) +
  geom_point(size=2.5) +
  labs(x="Per capita personal income in state (X1)", 
       y="Per capita expenditure on shelters/housing assistance in state (Y)", 
       title = "Personal Income and Expenditure on Housing Assistance") +
  scale_shape_manual(name = "Region", values = c(17, 18, 19, 20), labels = c("Northeast", "North Central", "South", "West")) +
  scale_color_manual(name = "Region", values = c(17, 18, 19, 20), labels = c("Northeast", "North Central", "South", "West"))

