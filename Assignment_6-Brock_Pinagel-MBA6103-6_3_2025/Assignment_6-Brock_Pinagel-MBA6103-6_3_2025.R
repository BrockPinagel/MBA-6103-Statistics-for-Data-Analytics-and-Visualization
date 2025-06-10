library(tidyverse)

# Q 1 (a) R function
dbinom(x = 7, size = 12, prob = 0.762)

# Q 1 (b) R function
pbinom(q = 7, size = 12, p = 0.762, lower.tail = TRUE)

# Q 1 (c) R function
pbinom(q = 6, size = 12, p = 0.762, lower.tail = FALSE)

# Q 2 (a) R function
# Define invalid signature probabilities for each district
p_avondale <- 142 / 2800   # ~0.0507
p_midway   <- 170 / 1800   # ~0.0944
p_kingston <- 276 / 1400   # ~0.1971

# Approach A: sample 5, reject if 2 or more invalid (i.e., P(X >= 2))
reject_prob_A <- function(p) {
  1 - pbinom(1, size = 5, prob = p)
}

# Approach B: sample 10, reject if 3 or more invalid (i.e., P(X >= 3))
reject_prob_B <- function(p) {
  1 - pbinom(2, size = 10, prob = p)
}

# Compute rejection probabilities for each district and approach
approach_A <- c(
  Avondale = reject_prob_A(p_avondale),
  Midway   = reject_prob_A(p_midway),
  Kingston = reject_prob_A(p_kingston)
)

approach_B <- c(
  Avondale = reject_prob_B(p_avondale),
  Midway   = reject_prob_B(p_midway),
  Kingston = reject_prob_B(p_kingston)
)

# Combine into a data frame for comparison
result <- data.frame(
  District = names(approach_A),
  `Approach_A (n=5, ≥2)` = round(approach_A, 4),
  `Approach_B (n=10, ≥3)` = round(approach_B, 4)
)

print(result)
write.csv(result, file = "result.csv")

# Q 3 Poisson parameters
lambda <- 12

# Probability of 17 or more cars
p_17_or_more <- 1 - ppois(16, lambda)
print(round(p_17_or_more, 4))

# Q 4 (a) R function for "exactly": dpois(x, lambda) is the probability of x successes in a period when the expected number of events is lambda. 
dpois(x = 1, lambda = .300)

# Q 4 (b) What is the probability that 1 hit during the next 10 baseball games?
dpois(x = 1, lambda = .300 * 10)

# Q 5 (a) R function for "exactly": dpois(x, lambda) is the probability of x successes in a period when the expected number of events is lambda. 
dpois(x = 0, lambda = 2.075)

# Q 5 (b) R function for "exactly": dpois(x, lambda) is the probability of x successes in a period when the expected number of events is lambda. 
dpois(x = 1, lambda = 2.075)

# Q 5 (c) What is the probability that 3 or less home runs will be hit in a game?
#R function for "at most": ppois(q, lambda, lower.tail = TRUE) is the cumulative probability
ppois(q = 3, lambda = 2.075, lower.tail = TRUE) 

# Q 5 (d) What is the probability that at least 2 home runs will be hit in a game?
#R function for "at least": ppois(q, lambda, lower.tail = FALSE) is the cumulative probability
ppois(q = 1, lambda = 2.075, lower.tail = FALSE)

# Q 6 (a)	What is the percentage of students scoring 84 or more in the exam?
#pnorm(q, mean, sd, lower.tail = TRUE)
pnorm(84, 72, 15.2, lower.tail = FALSE)

# Q 7 (a) A man is randomly selected. His height is 6 feet. What percentile will he be?
#pnorm(q, mean, sd, lower.tail = TRUE)
pnorm(72, 70, 3, lower.tail = TRUE)

# Q 8 (a) Exactly 95% of the batteries are estimated to fail between what two values?
19-(1.96*1.2)
19+(1.96*1.2)
