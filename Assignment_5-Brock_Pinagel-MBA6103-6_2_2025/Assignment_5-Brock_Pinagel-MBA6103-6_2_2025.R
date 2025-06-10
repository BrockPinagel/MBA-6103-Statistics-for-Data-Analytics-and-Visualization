library(summarytools)
library(package = "tidyverse")

#work with summarytools package built-in dataframe tobacco
tobacco <- tobacco

#Recode BMI into three groups based on median and change to factor
summary(tobacco$BMI)

tobacco.1 <- tobacco %>%
  mutate(BMI.3 = cut_number(BMI, n = 3))

freq(tobacco.1$BMI.3)

tobacco.1$BMI.3 <- recode(tobacco.1$BMI.3,  
                          "[8.83,23.9]" = "low", 
                          "(23.9,27.5]" = "med",
                          "(27.5,39.4]" = "high")

#Use ctable() function from summarytools package to create contingency table
ctable(x = tobacco.1$BMI.3, 
      y = tobacco.1$diseased, 
      prop = "n")   # Do not show proportions

#Bayes for medical test X: 
##P(A|X) = P(A)*P(X|A) / P(A)P(XA) + P(~A)P(X~A)
#P(A) = 1.5% of people have a certain genetic defect.
#P(~A) = 99% of people not have genetic effect
#P(X/A) = 89% of tests for the gene detect the defect (true positives) = Chance of a positive test result given that the person actually has the gene.
#P(X~A) = 8% of the tests are false positives = Chance of a positive test if the person doesn’t have the gene. 
#?P(A|X) = If a person gets a positive test result, what are the odds they actually have the genetic defect?

BayesMedical <- function(pA, pXA, p.A, pX.A) {
  pAX <- (pA * pXA) / ((pA * pXA) + (p.A * pX.A))
  return(pAX)
}

#define probabilities
pA <- 0.015 
p.A <- 0.985
pXA <- 0.89
pX.A <- 0.08

BayesMedical(pA, pXA, p.A, pX.A)