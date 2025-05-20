# Brock Pinagel
# Dr. Colin M. Wasiloff
# MBA 6103
# Assignment 1
# Use R to tidy up the file “legal_weed_age_GSS2018.csv” and create the legalize 
# bar graph with  percent of responses in age group. 

# Paste the 2016 figure from the lecture and this new 2018 figure into a brief 
# business style memo that describes the results presented in the figures. 

library(package = "tidyverse")

#coding: AGE (89 = 89 or older, 99 = NA), GRASS (0 = IAP, 1 = LEGAL, 2 = NOT LEGAL, 8 = DK, 9 = NA)#coding: AGE (89 = 89 or older, 99 = NA), GRASS (0 = IAP, 1 = LEGAL, 2 = NOT LEGAL, 8 = DK, 9 = NA)

legal_weed_age_GSS2018_cleaned <- legal_weed_age_GSS2018 %>%
  mutate_at(vars(GRASS), ~ case_when(!. %in% c(0,8,9)~ .)) %>% #recode multiple values as missing
  mutate(GRASS = recode_factor(.x = GRASS,
                               '1' = 'LEGAL',
                               '2' = 'NOT LEGAL')) %>%
  mutate_at(vars(AGE), ~ case_when(!. %in% c(99)~ .)) %>% #recode value as missing
  mutate(AGE.CAT = cut(x = AGE,
                       breaks = c(-Inf, 29, 59, 74, Inf),
                       labels = c("< 30", "30 - 59", "60 - 74", "75+" )))

summary(object = legal_weed_age_GSS2018_cleaned)

#use for dropping all rows with any missing data

legal_weed_age_GSS2018_cleaned_0 <- legal_weed_age_GSS2018_cleaned %>%
  drop_na(AGE:AGE.CAT)
summary(object = legal_weed_age_GSS2018_cleaned_0)

#visualization using ggplot (from tidyverse package)

# pipe the data set into ggplot
# in the aesthetics provide the variable names for the x and y axes
# choose a geom for graph type
# add axis labels with labs
# choose a theme for the overall graph look

# adding dodge to show bars side by side + Percent of age group
legalize.bar <- legal_weed_age_GSS2018_cleaned_0 %>%
  mutate(GRASS = recode_factor(.x = GRASS,
                               `LEGAL` = "Yes",
                               `NOT LEGAL` = "No")) %>%
  group_by(GRASS, AGE.CAT) %>%
  count() %>%
  group_by(AGE.CAT) %>%
  mutate(perc.GRASS = 100*n/sum(n)) %>%
  ggplot(aes(x = AGE.CAT, fill = GRASS,
             y = perc.GRASS)) +
  geom_col(position = 'dodge') +
  theme_minimal() +
  scale_fill_manual(values = c("#78A678", '#7463AC'),
                    name = "Should marijuana\nbe legal?") +
  labs(x = "Age group (in years)",
       y = "Percent of responses in age group")
legalize.bar