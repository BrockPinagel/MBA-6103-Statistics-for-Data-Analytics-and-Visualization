# Brock Pinagel
# Dr. Colin M. Wasiloff
# MBA 6103
# Assignment 4

# Overview:  This assignment is asking you to prepare a report that contains 
# descriptive statistics and recommendations.  Complete all the required statistics using R. 

library(tidyverse)
library(summarytools)

# import case study data
m4 <- read.csv(file = "Module 4 Assignment.csv")

#tidy the data
##CODE BOOK
#V1 = MTYPE: 1 = "Machine 1", 2 = "Machine 2". The rated production rate for machine type 1 is 700 parts per minute, machine type 2 is rated at 200 parts per minutes.
#V2 = PROD: Total parts produced by the machine during the hours that the machine was operating.
#V3 = HRSWRK: Number of hours that the machine was operating. The machines were scheduled for a shift of 7.75 hours each day. 
#V4 = HRSDOWN: Number of hours the machine was down, or inoperative, for any reason. 

#rename variables
colnames(m4) <- c('MTYPE', 'PROD', 'HRSWRK', 'HRSDOWN')

m4.cleaned <- m4 %>%
  mutate(EXPECTED = ifelse(MTYPE == 1, HRSWRK*700*60, 
                    ifelse(MTYPE == 2, HRSWRK*200*60, HRSWRK*150*60)))%>%
  mutate(EFFICIENCY = PROD/EXPECTED*100)  %>%
  mutate(MTYPE = recode_factor(.x = MTYPE,
                               '1' = 'Machine 1',
                               '2' = 'Machine 2',
                               '3' = 'Machine 3')) 

# Select variables for analysis
m4.stats <- m4.cleaned %>%
  select(MTYPE, EFFICIENCY, HRSWRK, HRSDOWN)

# change the Stats presented in first and second optional expressions
st_options(
  dfSummary.custom.1 = 
    expression(
      paste(
        "Q1 - Q3 :",
        round(
          quantile(column_data, probs = .25, type = 2, 
                   names = FALSE, na.rm = TRUE), digits = 1
        ), " - ",
        round(
          quantile(column_data, probs = .75, type = 2, 
                   names = FALSE, na.rm = TRUE), digits = 1
        )
      )
    )
)
st_options(
  dfSummary.custom.2 = 
    expression(
      paste(
        "Skew, Kur :",
        round(
          skewness(column_data, na.rm = TRUE), digits = 1
        ), " , ",
        round(
          kurtosis(column_data, na.rm = TRUE), digits = 1
        )
      )
    )
)

# dfSummary() x categorical variable using group_by()
output.dfS <- m4.stats%>% 
  group_by(MTYPE) %>% 
  dfSummary()
view(output.dfS)

# descriptive statistics table
ds.t <- m4.stats%>% 
  group_by(MTYPE) %>% 
  descr(stats   = c("n.valid", "mean", "sd"),
        transpose  = TRUE) %>%
  tb(order = 3) # 1, 2, 3 make different orders of the variables

ds.t

write.csv(ds.t, file = "ds.t.csv")

# Create boxplot, remove legend, customize x axis labels
box.c1.efficiency <-m4.stats %>% 
  ggplot(aes(x=MTYPE, y=EFFICIENCY, fill = MTYPE)) +
  geom_boxplot(alpha = .3) +
  geom_jitter(aes(color = MTYPE), alpha = .1) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, angle=0),
        legend.position='none')
box.c1.efficiency


box.c1.hrswrk <-m4.stats %>% 
  ggplot(aes(x=MTYPE, y=HRSWRK, fill = MTYPE)) +
  geom_boxplot(alpha = .3) +
  geom_jitter(aes(color = MTYPE), alpha = .1) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, angle=0),
        legend.position='none')
box.c1.hrswrk

box.c1.hrsdwn <-m4.stats %>% 
  ggplot(aes(x=MTYPE, y=HRSDOWN, fill = MTYPE)) +
  geom_boxplot(alpha = .3) +
  geom_jitter(aes(color = MTYPE), alpha = .1) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, angle=0),
        legend.position='none')
box.c1.hrsdwn

# hide x axis ticks
b1 <- box.c1.efficiency + 
  ylab("Efficiency (%)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position='none')
b1

# hide x axis ticks
b2 <- box.c1.hrswrk + 
  ylab("Hours Working") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position='none')
b2

## show x axis ticks
b3 <- box.c1.hrsdwn + 
  ylab("Hours Down") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=10, angle=0),
        legend.position='none')
b3

gridExtra::grid.arrange(
  b1, 
  b2, 
  b3, ncol = 1, nrow = 3)














