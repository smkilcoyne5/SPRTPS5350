
library(ggplot2)

#-------------------------------------------------------------------------------
# Binomial distribution

# Problem
# Suppose a player takes 10 three point shots in a game. 
# Assume they have a 40 percent chance of making the shot
# 
# Q1. Find the probability of making exactly 4 shots 
# Solution

dbinom(x=4, size =10, prob=0.4)

# Q2. What is the probability of making four or less shots?
# Solution
library(ggplot2)
pbinom(q=4, size=10, prob=0.4)

#Graph it
k <-  seq(0,10,1) #make a sequence of data
k
probTable <- data.frame(answer = k, prob=dbinom(k,10,0.4)) #make a probability table

#plot it
ggplot(probTable,aes(x=answer,y=prob)) + geom_bar(stat="identity", fill="blue") +
  scale_x_continuous(breaks=k) + theme_bw()
#-------------------------------------------------------------------------------
# Poisson distribution - 
# Discrete probability distribution that expresses the probability of a 
# given number of events occurring in a fixed interval of time or space

# Problem
# If Liverpool scores 2.18 goals per game, find the 
# probability of having 4 or more goals per game
# Solution

ppois(4, lambda = 2.18) #4 or less goals per game
ppois (4,lambda = 2.18, lower=FALSE) # 4 or more goals per game
1-ppois(4,lambda = 2.18)
goals <- seq(0,10,1)
goals
prob <- dpois(goals,lambda=2.18)
prob
ggplot() + geom_step(aes(x=goals,y=prob)) + scale_x_continuous(breaks=goals)+theme_bw()
#-------------------------------------------------------------------------------
# Normal distribution

# Problem
# The NBA teams dataset contains NBA team averages for the 19-20 seasonp. The column 
# Points follows a normal distribution.
#
# Q1. What is the percentage of teams that score less than 110 points per game?
# Solution

nba <- read_csv("data/nba-team-stats.csv")
head(nba)
meanPts <- mean(nba$PTS) #get the mean points per game
sdPts <-  sd(nba$PTS)
pnorm(110, mean=meanPts, sd=sdPts) # what is the percentage of teams that score less than 110 per game?

# Q2. What fraction of NBA teams score between 105 and 110 pts?
# Solution

pnorm(110, mean=meanPts, sd=sdPts) - pnorm(3, mean=meanPts, sd=sdPts)
pts_width <- seq(100,120,1)
prob <-  dnorm(pts_width, mean = meanPts, sd= sdPts)

ggplot() + geom_point(aes(x=pts_width, y=prob)) + theme_bw()

#-------------------------------------------------------------------------------
# Exponential distribution

# Q1. Assume an NCAA basketball team's possession length follows 
# an exponential distribution
# with the average length of possession = 15 seconds 
# Find the probability that a possession is 10 seconds.
# Solution

1 - pexp(10, rate=1/15)
seconds <- seq(1,30,1)
prob <- pexp(seconds, rate=1/15)
ggplot() + geom_point(aes(x=seconds,y=prob)) + theme_bw()

