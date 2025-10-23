getwd()
setwd("~/GitHub/StatsI_2025/problemSets/PS02/my_answers")
#Step 1: State Hypotheses
#Null Hypothesis: The variables are statistically independent
#Alternative Hypothesis: The variables are statistically dependent
#Calculate the expected value for each cell, Row total/Grand total * Column total
observed_table <- matrix( 
  c(14, 6, 7, 7, 7, 1),
  nrow = 2,
  ncol = 3,
  byrow = TRUE
)

table_row_totals <- rbind(observed_table,
                          c(sum(observed_table[ ,1]),
                            sum(observed_table[ ,2]),
                            sum(observed_table[ ,3])))

class_table <- cbind(table_row_totals,
                          c(sum(table_row_totals[1, ]),
                            sum(table_row_totals[2, ]),
                            sum(table_row_totals[3, ])))


rownames(class_table) <- c("Upper Class", "Lower Class", "Column Total")
colnames(class_table) <- c("Not Stopped", "Bribe Requested", 
                           "Stoped/Given Warning", "Row Total")
class_table
  
#Find the expected values by calculating row total/grand total * column total
expected_values <- matrix(c(
  (class_table[1,4] / class_table[3,4] * class_table[3,1]), 
  (class_table[1,4] / class_table[3,4] * class_table[3,2]), 
  (class_table[1,4] / class_table[3,4] * class_table[3,3]), 
  (class_table[2,4] / class_table[3,4] * class_table[3,1]), 
  (class_table[2,4] / class_table[3,4] * class_table[3,2]), 
  (class_table[2,4] / class_table[3,4] * class_table[3,3])), 
   nrow = 2, 
   ncol = 3, 
   byrow = TRUE
  )

rownames(expected_values) <- c("Upper Class", "Lower Class")
colnames(expected_values) <- c("Not Stopped", "Bribe Requested", 
                               "Stopped/Given Warning")
expected_values

#Calculate the Chi-Squared Statistic by taking the (Observed value - Expected Value) 
#squared dividing that by the expected value. Do that for every value in the table
#then sum all of them together

test_statistic <- sum(((observed_table - expected_values)^2)
                        / expected_values)
test_statistic

#Find the degrees of freedom by doing (Rows - 1) * (Columns - 1)
class_df <- (nrow(observed_table) - 1) * (ncol(observed_table) - 1)
class_df

#Find the p-value
pchisq(test_statistic, df=class_df, lower.tail=FALSE)

#Cell [1,1]
difference <- observed_table - expected_values
difference[1,1] / sqrt(expected_values[1,1]*
          (1 - class_table[1,4] / class_table[3,4])*
          (1 - class_table[3,1]/class_table[3,4]))

#Cell [1,2]
difference[1,2] / sqrt(expected_values[1,2]*
          (1 - class_table[1,4] / class_table[3,4])*
          (1 - class_table[3,2]/class_table[3,4]))

#Cell [1,3]
difference[1,3] / sqrt(expected_values[1,3]*
          (1 - class_table[1,4] / class_table[3,4])*
          (1 - class_table[3,3]/class_table[3,4]))

#Cell [2,1]
difference[2,1] / sqrt(expected_values[2,1]*
          (1 - class_table[2,4] / class_table[3,4])*
          (1 - class_table[3,1]/class_table[3,4]))

#Cell [2,2]
difference[2,2] / sqrt(expected_values[2,2]*
          (1 - class_table[2,4] / class_table[3,4])*
          (1 - class_table[3,2]/class_table[3,4]))


#Cell [2,3]
difference[2,3] / sqrt(expected_values[2,3]*
          (1 - class_table[2,4] / class_table[3,4])*
          (1 - class_table[3,3]/class_table[3,4]))

#Problem 2
women <- read.csv("women.csv")
bivariate_regression <- lm(women$water ~ women$reserved)
bivariate_regression

summary(bivariate_regression)

