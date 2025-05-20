# Brock Pinagel
# Dr. Colin M. Wasiloff
# MBA 6103
# Assignment 2
# Use the data file “GSS2018.csv” and create a professional frequency 
# distribution data table of the variables, SEX, AGE (with 6 class intervals), 
# RACE, MARITAL, DEGREE, INCOME (with the following 6 class intervals: <$25K, 
# $25-$50K, $50-$75K, $75-$110K, $110-$170K, >$170K), grouped by SEX. Insert the 
# table into a brief business style memo and describe the results shown in the table.

# coding for gss.2018.small
# SEX: 1=MALE, 2=FEMALE
# AGE: 99=NA
# RACE: 1=WHITE, 2=BLACK, 3=OTHER
# MARITAL: "Are you currently – married, widowed, divorced, separated, or have you never
# been married?" VALUE, LABEL: 1=MARRIED, 2=WIDOWED, 3=DIVORCED,
# 4=SEPARATED, 5=NEVER MARRIED, 9=NA
# DEGREE: 0=NO DEGREE, 1=HIGH SCHOOL, 2=JUNIOR COLLEGE, 3=BACHELOR, 4=GRADUATE
# INCOME: 1=<$1K, 2=$1K-$3K, 3=$3K-$4K, 4=$4K-$5K, 5=$5K-$6K, 6=$6K-$7K, 7=$7K-
# $8K, 8=$8K-$10K, 9=$10K-$12.5K, 10=$12.5K-$15K, 11=$15K-$17.5K, 12=$17.5K-$20K,
# 13=$20K-$22.5K 14=$22.5K-$25K, 15=$25K-$30K, 16=$30K-$35K, 17=$35K-$40K,
# 18=$40K-$50K, 19=$50K-$60K, 20=$60K-$75K, 21=$75K-$90K, 22=$90K-$110K,
# 23=$110K-$130K, 24=$130K-$150K, 25=$150K-$170K, 26=$170K+, 27=NA, 98=NA

library(summarytools)
library(tidyverse)
library(tableone)
library(dplyr)

gss.2018 <- read.csv(file = "GSS2018.csv")

# Drop unused columns
gss.2018.small <- gss.2018 %>%
  select(-DIVORCE, -CHILDS, -AGEKDBRN, -PRES16, -POLVIEWS, -NATENVIR, -NATARMS, -NATSCI, -NATENRGY, -GRASS, -SATJOB, -OWNGUN, -PISTOL)

# Make 6 class intervals for AGE 
# i >= (Max Age - Min Age) / 6

(89-18)/6

# Make class interval 12
# 18-29, 30-41, 42-53, 54-65, 66-77, 78-89+

gss.2018.small.cleaned <- gss.2018.small %>%
  mutate(SEX = recode_factor(.x = SEX,
                             '1' = 'MALE',
                             '2' = 'FEMALE')) %>%
  mutate_at(vars(AGE), ~ case_when(!. %in% c(99)~ .)) %>% # recode value as missing
  mutate_at(vars(AGE), as.numeric) %>%
  mutate(AGE.CAT = cut(x = AGE,
                       breaks = c(-Inf, 29, 41, 53, 65, 77, Inf),
                       labels = c('18-29', '30-41', '42-53', '54-65', '66-77', '78-89+'))) %>%
  mutate_at(vars(INCOME), ~ case_when(!. %in% c(27, 98)~ .)) %>% # recode value as missing
  mutate(INCOME = cut(x = INCOME,
                      breaks = c(-Inf, 14, 18, 20, 22, 25, Inf),
                      labels = c('<$25K', '$25-$50K', '$50-$75K', '$75-$110K', '$110-$170K', '>$170K'))) %>%
  mutate(DEGREE = recode_factor(.x = DEGREE,
                                '0' = 'NO DEGREE',
                                '1' = 'HIGHSCHOOL',
                                '2' = 'JUNIOR COLLEGE',
                                '3' = 'BACHELOR',
                                '4' = 'GRADUATE')) %>%
  mutate(RACE = recode_factor(.x = RACE,
                              '1' = 'WHITE',
                              '2' = 'BLACK',
                              '3' = 'OTHER')) %>%
  mutate_at(vars(MARITAL), ~ case_when(!. %in% c(9)~ .)) %>% # recode value as missing
  mutate(MARITAL = recode_factor(.x = MARITAL,
                                 '1' = 'MARRIED',
                                 '2' = 'WIDOWED',
                                 '3' = 'DIVORCED',
                                 '4' = 'SEPARATED',
                                 '5' = 'NEVER MARRIED'))

freq(gss.2018.small.cleaned)

# check coding of AGE.CAT using the Grouped Statistics function group_by with summarytools descr()

gss.2018.small.cleaned %>% 
  group_by(AGE.CAT) %>% 
  drop_na() %>%
  descr(stats   = c("n.valid", "min", "max"),
        transpose  = TRUE) %>%
  tb(order = 3) # 1, 2, 3 make different orders of the variables

# Drop AGE and missing

gss.2018.small.cleaned_0 <- gss.2018.small.cleaned %>%
  select(-AGE)

gss.2018.small.cleaned.na <- gss.2018.small.cleaned_0 %>%
  drop_na()

# group frequency distribution by SEX

gss.2018.table <- CreateTableOne(strata = 'SEX', data = gss.2018.small.cleaned.na)

print(gss.2018.table, showAllLevels = TRUE,  varLabels = TRUE, test = FALSE)

# output table to csv file to publish 

gss.table <- print(gss.2018.table, showAllLevels = TRUE,  varLabels = TRUE, test = FALSE)

write.csv(gss.table, file = "gss.2018.table.csv")





