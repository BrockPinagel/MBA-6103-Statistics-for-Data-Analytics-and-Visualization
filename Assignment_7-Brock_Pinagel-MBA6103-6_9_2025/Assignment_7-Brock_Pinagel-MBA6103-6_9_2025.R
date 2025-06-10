library(DescTools)
library(ggpubr)
library(semTools)
library(tidyverse)
library(summarytools)

#Distance to substance abuse facility with needle exchange
opioid.dist <- read.csv(file = "opioid_dist_to_needle_exchange_2018.csv")

#learn about the distribution
descr(opioid.dist$DISTANCE)

#The mean distance to the nearest substance abuse facility that provides at least one type of MAT is 24.04 miles away, with a standard deviation of 22.66 miles. 

#Quick histogram using R base hist() function--notice positive skew
hist(opioid.dist$DISTANCE)

#using library semTools skew() to obtain more detailed distribution statistics
semTools::skew(object = opioid.dist$DISTANCE)

#transforming a variable using five common methods
opioid.dist.cleaned <- opioid.dist %>%
  mutate(miles.log = log(x = DISTANCE)) %>% #computes log **Note that this transformation does not work if your data have 0 values. 
  mutate(miles.log1 = log1p(x = DISTANCE))%>%#Some researchers use log + 1 = log1p if data have 0.
  mutate(miles.sqrt = DISTANCE^(1/2))%>% #computes x^(1/2) **Good if data have 0**
  mutate(miles.cube.root = DISTANCE^(1/3)) %>% #computes x^(1/3)
  mutate(miles.inverse = 1/DISTANCE)%>%##Does not work if scores have 0
  select(DISTANCE, miles.log,miles.log1, miles.sqrt,miles.cube.root,miles.inverse) #Select variables for analysis

View(opioid.dist.cleaned)

descr(opioid.dist.cleaned)

# graph DISTANCE and the transformations 
# Histogram + density + vertical mean + median
#Original distance

distance.o <- opioid.dist.cleaned %>%
  ggplot(aes(x = DISTANCE)) +
  geom_histogram(fill = "#7463AC", color = "white", bins = 30, aes(y = ..density..)) +
  geom_function(fun = dnorm, colour = "black", size = 1,
                args = list(mean = mean(opioid.dist.cleaned$DISTANCE), 
                            sd = sd(opioid.dist.cleaned$DISTANCE))) +
  theme_minimal() +
  labs(y = "Density") + 
  geom_rug(sides="b")
distance.o

#log+1 transformation of distance
distance.l <- opioid.dist.cleaned %>%
  ggplot(aes(x = miles.log1)) +
  geom_histogram(fill = "#7463AC", color = "white", bins = 30, aes(y = ..density..)) +
  geom_function(fun = dnorm, colour = "black", size = 1,
                args = list(mean = mean(opioid.dist.cleaned$miles.log1), 
                            sd = sd(opioid.dist.cleaned$miles.log1))) +
  theme_minimal() +
  labs(y = "Density") + 
  geom_rug(sides="b")
distance.l

#sqrt transformation of distance
distance.s <- opioid.dist.cleaned %>%
  ggplot(aes(x = miles.sqrt)) +
  geom_histogram(fill = "#7463AC", color = "white", bins = 30, aes(y = ..density..)) +
  geom_function(fun = dnorm, colour = "black", size = 1,
                args = list(mean = mean(opioid.dist.cleaned$miles.sqrt), 
                            sd = sd(opioid.dist.cleaned$miles.sqrt))) +
  theme_minimal() +
  labs(y = "Density") + 
  geom_rug(sides="b")
distance.s

#cube root transformation of distance
distance.c <- opioid.dist.cleaned %>%
  ggplot(aes(x = miles.cube.root)) +
  geom_histogram(fill = "#7463AC", color = "white", bins = 30, aes(y = ..density..)) +
  geom_function(fun = dnorm, colour = "black", size = 1,
                args = list(mean = mean(opioid.dist.cleaned$miles.cube.root), 
                            sd = sd(opioid.dist.cleaned$miles.cube.root))) +
  theme_minimal() +
  labs(y = "Density") + 
  geom_rug(sides="b")
distance.c

#2x2 multi panel figure
gridExtra::grid.arrange(distance.o,distance.l,distance.s,distance.c,ncol = 2)

#z score of 200 miles for cuberoot transformation
((200^(1/3))-4.35)/1.33

#Percentile of 200 miles with the cuberoot transformation
pnorm(1.126342, mean = 0, sd = 1, lower.tail = T)

# set a starting value for sampling
set.seed(seed = 1945)

# sample 500 counties at random
counties.500 <- opioid.dist.cleaned %>%
  sample_n(size = 500, replace = TRUE)

# compute the mean distance in the sample
mean(counties.500$DISTANCE, na.rm = TRUE)

#What would happen if we took 20 samples of counties where each sample had 500 counties in it?
# get 20 samples
# each sample has 500 counties
# put samples in a data frame with each sample having a unique id called sample_num
# n = 10,000

set.seed(seed = 111)
samples.20 <- bind_rows(replicate(n = 20, opioid.dist.cleaned %>%
                                    sample_n(size = 500, replace = TRUE),
                                  simplify = FALSE), .id = "sample_num")

# find the mean for each sample
sample.20.means <- samples.20 %>%
  group_by(sample_num) %>%
  summarize(mean.distance = mean(x = DISTANCE, na.rm = TRUE))

# find the mean of the 20 sample means
mean(sample.20.means$mean.distance, na.rm = TRUE)

# means and confidence intervals for the 20 samples of the 500 counties randomly selected from the population with a parametric mean. 
mean.d <- mean(opioid.dist.cleaned$DISTANCE)
ggerrorplot(samples.20, x = "sample_num", y = "DISTANCE", 
            desc_stat = "mean_ci",
            error.plot = "errorbar",            
            add = "mean",
            ci = 0.95,
            xlab = "Sample",
            ylab = "Distance (95% CI)") +
  geom_hline(aes(yintercept = mean.d),linetype = 2, color =
               "deeppink")

# Calculate the 95% CI of the sample of 500 counties.
ci.500 <- counties.500 %>%
  summarize(mean = mean(x = DISTANCE),
            sd = sd(x = DISTANCE),
            n = length(x = DISTANCE),
            se = sd(x = DISTANCE)/sqrt(n),
            lower.ci = mean - 1.96 * se,
            upper.ci = mean + 1.96 * se)
ci.500

#Adjust the output
print(ci.500, digits = 4) #show specific number of digits
print( sprintf(ci.500, fmt = '%.2f')) #show specific number of digits

#import gss.2018 data
gss.2018 <- read.csv(file = "GSS2018.csv")

##coding
#GRASS: "Should marijuana be made legal", 0=IAP, 1=LEGAL, 2=NOT LEGAL, 8=DK, 9=NA
#OWNGUN: "Have gun in home", 0=IAP, 1=YES, 2=NO, 3=REFUSED, 8=DK, 9=NA

#select specific variables and tidy 
gss.2018.small <- gss.2018 %>%
  select(GRASS,OWNGUN) %>%
  mutate_at(vars(GRASS), ~ case_when(!. %in% c(0,8,9)~ .)) %>% #recode value as missing
  mutate_at(vars(OWNGUN), ~ case_when(!. %in% c(0,3,8,9)~ .)) %>% #recode value as missing
  mutate(OWNGUN = recode_factor(.x = OWNGUN,
                                '1' = 'YES',
                                '2' = 'NO'))%>%
  mutate(GRASS = recode_factor(.x = GRASS,
                             '1' = 'LEGAL',
                             '2' = 'NOT LEGAL'))

freq(gss.2018.small)
dfSummary(gss.2018.small)

#wilson = The Wilson interval is recommended by Agresti and Coull (1998) as well as by Brown et al (2001).
# Legal grass
DescTools::BinomCI(938, 1447,
                   conf.level = 0.95,
                   method = "wilson")

# Gun in home
DescTools::BinomCI(537, 1530,
                   conf.level = 0.95,
                   method = "wilson")

#The unweighted margin of sampling error for the full sample of 1,500 Likely Voters is +/- 2.5 percentage points with a 95% level of confidence. 
1.96*sqrt((0.5*0.5)/457)
#Answer +/- 4.6 percentage points



