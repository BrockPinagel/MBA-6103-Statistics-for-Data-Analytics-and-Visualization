# Brock Pinagel
# Dr. Colin M. Wasiloff
# MBA 6103
# Assignment 3

# Use the data file “GSS2018.csv” and work with the variables, SEX, PRES16, POLVIEWS, NATENVIR, NATARMS, NATSCI, NATENRGY

# coding to tidy the data
# SEX: 1=MALE, 2=FEMALE
# PRES16: "VOTE CLINTON OR TRUMP", 0=IAP, 1=Clinton, 2=Trump, 3=Other,4=Didn't vote, 8=DK, 9=NA 
# POLVIEWS: "Think of self as liberal or conservative", 1=Extremely Liberal, 2=Liberal, 3=Slightly Liberal, 4=Moderate, 5=Slightly Conservation, 6=Conservative, 7=Extremely Conservative, 8=DK, 9=NA
# NATENVIR: "Improving & protecting environment", 0=IAP, 1=TOO LITTLE, 2=ABOUT RIGHT, 3=TOO MUCH, 8=DK, 9=NA
# NATARMS: "Military, armaments, and defense", 0=IAP, 1=TOO LITTLE, 2=ABOUT RIGHT, 3=TOO MUCH, 8=DK, 9=NA
# NATSCI: "Supporting scientific research", 0=IAP, 1=TOO LITTLE, 2=ABOUT RIGHT, 3=TOO MUCH, 8=DK, 9=NA
# NATENRGY: "Developing alternative energy sources", 0=IAP, 1=TOO LITTLE, 2=ABOUT RIGHT, 3=TOO MUCH, 8=DK, 9=NA
# 1.	Create a Data Frame Summary of the seven variables using dfSummary(). Label this Figure 1.
# 2.	Create a professional descriptive statistics table of the five continuous data variables with mean, SD, skewness, kurtosis. Label this Table 1.
# 3.	Create a professional descriptive statistics table of POLVIEWS, NATSCI and NATENRGY x PRES16. Report n, mean, SD. In the table, make the continuous variables as columns, and the categorical variables as rows. Label this Table 2.
# 4.	Create a professional descriptive statistics table of POLVIEWS, NATSCI, and NATENRGY x SEX x PRES16. Report n, mean, SD. In the table, make the continuous variables as columns, and the categorical variables as rows. Label this Table 3.
# 5.	Write a brief narrative of Table 2. 


library(summarytools)
library(tidyverse)
library(tableone)
library(dplyr)

gss.2018 <- read.csv(file = "GSS2018.csv")

# select specific variables 
gss.2018.small <- gss.2018 %>%
  select(SEX, PRES16, POLVIEWS, NATENVIR, NATARMS, NATSCI, NATENRGY)

# frequency distribution of every variable in a data frame
freq(gss.2018.small)

gss.2018.small.cleaned <- gss.2018.small %>%
  mutate_at(vars(PRES16, NATENVIR, NATARMS, NATSCI, NATENRGY), ~ case_when(!. %in% c(0,8,9)~ .)) %>% # recode value as missing
  mutate_at(vars(PRES16), ~ case_when(!. %in% c(3,4)~ .)) %>% # recode value as missing
  mutate_at(vars(POLVIEWS), ~ case_when(!. %in% c(8,9)~ .)) %>% # recode value as missing
  mutate_at(vars(NATENVIR, NATARMS, NATSCI, NATENRGY), as.numeric) %>%
  mutate(SEX = recode_factor(.x = SEX,
                             '1' = 'MALE',
                             '2' = 'FEMALE')) %>%  
  mutate(PRES16 = recode_factor(.x = PRES16,
                                '1' = 'CLINTON',
                                '2' = 'TRUMP'))

# drop all missing from data frame (aka listwise deletion)
gss.2018.small.cleaned.na <- gss.2018.small.cleaned %>%
  drop_na()
freq(gss.2018.small.cleaned.na)

summary(object = gss.2018.small.cleaned.na)

# Question 1
summarytools::view(dfSummary(gss.2018.small.cleaned))

# (Question 2) creating a descriptive statistics table for report with customized outputs
descr(gss.2018.small.cleaned,
      stats     = c("mean", "sd","skewness", "kurtosis"),
      transpose = TRUE,
      headings  = FALSE) 

# (Question 2) send to an object for exporting
output1 <- descr(gss.2018.small.cleaned,
                 stats     = c("n.valid", "mean", "sd","skewness", "kurtosis"),
                 transpose = TRUE,
                 headings  = FALSE)
output1

# (Question 2) write object to .csv for manipulation in excel, format and publish ready
write.csv(output1, file = "ouput1.csv")

# (Question 3) Drop unused columns
gss.2018.q3 <- gss.2018.small.cleaned%>%
  select(-SEX, -NATENVIR, -NATARMS)

# (Question 3) create a table with continuous variable x categorical variable with all available data
output2 <- gss.2018.q3%>% 
  group_by(PRES16) %>%
  drop_na(PRES16) %>%
  descr(stats   = c("n.valid", "mean", "sd"),
        transpose  = TRUE) %>%
  tb(order = 3) #1, 2, 3 make different orders of the variables

output2

# (Question 3) write to .csv for formatting and publishing
write.csv(output2, file = "ouput2.csv")

# (Question 4) Drop unused columns
gss.2018.q4 <- gss.2018.small.cleaned%>%
  select(-NATENVIR, -NATARMS)

# (Question 4) create same table with continuous variable x second categorical variable
output3 <- gss.2018.q4 %>% 
  group_by(SEX, PRES16) %>%
  drop_na(PRES16) %>%
  descr(stats   = c("n.valid", "mean", "sd"),
        transpose  = TRUE) %>%
  tb(order = 3) #1, 2, 3 make different orders of the variables

output3

# (Question 4) write to .csv for formatting and publishing
write.csv(output3, file = "ouput3.csv")
