install.packages("tidyverse")
install.packages("moments")
library(tidyverse)

pp <- hw5q4
pp <- power_play_goals

summary(pp)
#mean = 47.29, median = 46.0, etc.

#Calculate Stats
total = length(pp$PP)
mean = mean(pp$PP)
sd = sd(pp$PP)

pData <- function(nSD){
  lo = mean - nSD*sd
  hi = mean + nSD*sd
  percent = round(sum(pp$PP>=lo & pp$PP<=hi)/total *100,2)
}

print(paste("Percent of Data within 1 SD is ", pData(1), "%", sep=""))
#64.52%
print(paste("Percent of Data within 1.28 SD is ", pData(1.28), "%", sep=""))
#74.19%
print(paste("Percent of Data within 2 SD is ", pData(2), "%", sep=""))
#93.55%

#skewness
print(skewness(pp$PP))
#.7579

#kurtosis
print(kurtosis(pp$PP))
#3.004
#JO: It's fine. In Excel, it cheats as it calculates Excess 
#    Kurtosis rather than Kurtosis. 
#    A Kurtosis of 3 is equal to an Excess Kurtosis of 0.
# This example calculates 
set.seed(1234) #set.seed sets the starting number used to generate a sequence of random numbers 
kurtosis(rnorm(1000)) #this gives you the kurtosis of 1000 random numbers

#what did I do wrong with the skewness and kurtosis?

#q-q plot
PowerP <- ggplot(pp, aes(sample = pp$PP))
PowerP + stat_qq() + stat_qq_line()
