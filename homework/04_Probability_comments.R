library(ggplot2)

#Q1: Find probability of making exactly 4 shots

dbinom(x=4, size=10, prob=0.4)

#AnswerQ1: .2508 or 25.1%

#Q2: Find probability of making 4 or less shots

pbinom(q=4, size=10, prob=0.4)

#AnswerQ2: .6331 or 63.3%

#Graph

k <- seq(0,10,1)
k

###JO: Try installing the 'ggthemes' packages and messing around with a few of those###

probTable <- data.frame(answer = k, prob = dbinom(k, 10, 0.4)) #making a probability table
View(probTable)

ggplot(probTable, aes(x = answer, y = prob)) + geom_bar(stat = "identity", fill = "blue") + 
  scale_x_continuous(breaks = k) + theme_bw()

ggplot(probTable, aes(x = answer, y = prob)) + geom_bar(stat = "identity", fill = "red") + 
  scale_x_continuous(breaks = k) + theme_classic()

ggplot(probTable, aes(x = answer, y = prob)) + geom_bar(stat = "identity", fill = "purple") + 
  scale_x_continuous(breaks = k) + theme_light() #messing around with the themes

#Q3: If Liverpool scores 2.18 goals per game, find the probability of have 4 or more goals per game

ppois(4, lambda = 2.18) #4 or less goals per game, = .9296
ppois(4, lambda = 2.18, lower = FALSE) #4 or more goals per game
1 - ppois(4, lambda = 2.18) #alternative way of calculating

#AnswerQ3: .0703 or 7.0%

goals <- seq(0, 10, 1)
goals
prob <- dpois(goals, lambda = 2.18)
prob
ggplot() + geom_step(aes(x = goals, y = prob)) + scale_x_continuous(breaks = goals) + theme_bw()

prob
ggplot() + geom_line(aes(x = goals, y = prob)) + theme_classic()
#checking output for a line graph that does not use the goal breaks

#Q4: What percentage of teams score less than 110 points per game?

nbaPTS <- read.csv("data/nba-team-stats.csv")
head(nbaPTS)

meanPTS <- mean(nbaPTS$PTS) #mean team ppg

sdPTS <- sd(nbaPTS$PTS)  

pnorm(110, mean = meanPTS, sd = sdPTS) #gives % of teams that scored less than 110 per game
  
#AnswerQ4: .3279 or 32.8%

#Q5: What fraction of teams score between 105 and 110 ppg?

pnorm(110, mean = meanPTS, sd = sdPTS) - pnorm(105, mean = meanPTS, sd = sdPTS)
#had - pnorm(3, mean.....) in the sample code, was that a mistake? Is this correct?

#AnswerQ5: .2892 or 28.9%

pts_width <- seq(100, 120, 1)
prob1 <- dnorm(pts_width, mean = meanPTS, sd = sdPTS)

ggplot() + geom_point(aes(x = pts_width, y = prob1)) + theme_bw()

#Q6: With the average NCAA possession being 15 seconds, 
#find the probability that a possession is 10 seconds.
#assuming exponential distribution

1 - pexp(10, rate = 1/15) #JO: this is the probability that a possession is 10+ seconds

#is this finding the probability that the possession is AT LEAST 10 seconds?
#we should discuss this one
##JO: Yes - the plot is helpful here.
#AnswerQ6: .5134 or 51.3%

seconds <- seq(1, 30, 1)
prob2 <- pexp(seconds, rate = 1/15)
ggplot() + geom_point(aes(x = seconds, y = prob2)) + theme_classic()
#this gives probability that a possession is under _x_ amount of seconds, right?

#JO:Yes, that's correct.
