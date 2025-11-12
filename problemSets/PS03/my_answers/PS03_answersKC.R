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
# lapply(c("stringr"),  pkgTest)
library(ggplot2)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

#Question 1

#1.
lm(voteshare ~ difflog, inc.sub)

#2.
theme_update(plot.title = element_text(hjust = 0.5))

ggplot(inc.sub, aes(difflog, voteshare)) +
  geom_point() +
  labs(title = "Difference in Campaign Spending and Incumbent's Vote Share",
       x = "Difference in Campaign Spending Between Incumbent and Challenger",
       y = "Incumbent's Vote Share"
  ) +
  geom_smooth(method = lm)

#3. 
regression1 <- lm(voteshare ~ difflog, inc.sub)
residuals1 <- regression1$residuals

#4.
#y = 0.04167x + 0.57903


#Question 2

#1.
lm(presvote ~ difflog, inc.sub)

#2.
ggplot(inc.sub, aes(difflog, presvote)) +
  geom_point() +
  labs(title = "Difference in Spending and Voteshare of Presidential Candidate",
       x = "Difference Between Incumbent and Challenger's Spending",
       y = "Vote Share of the Incumbent's Party's Candidate"
       ) +
  geom_smooth(method = lm)

#3.
regression2 <- lm(presvote ~ difflog, inc.sub)
residuals2 <- regression2$residuals

#4.

#y = .023837x + 0.507583


#Question 3

#1.
lm(voteshare ~ presvote, inc.sub)

#2.
ggplot(inc.sub, aes(presvote, voteshare)) +
  geom_point() +
  labs(title = "Incumbent's Electoral Success and Vote Share of Presidential Candidate",
       x = "Incumbent's Electoral Success",
       y = "Vote Share of the Incumbent Party's Candidate"
  ) +
  geom_smooth(method = lm)

#3.
#y = 0.3880x + 0.4413

#Question 4

#1.
lm(residuals1 ~ residuals2, inc.sub)

#2.
ggplot(inc.sub, aes(residuals2, residuals1)) +
  geom_point() +
  labs(title = "Residuals",
       x = "Residuals from Question 2",
       y = "Residuals from Question 1"
  ) +
  geom_smooth(method = lm)

#3.
#y = mx + b

regression4 <- lm(residuals1 ~ residuals2, inc.sub)
options(scipen = 999)
regression4

#y = 0.256877012700097440145x - 0.000000000000000005934 


#Question 5

#1.
lm(voteshare ~ difflog + presvote, inc.sub)

#2.
#y = 0.44864 + 0.03554x1 + 0.25688x2


