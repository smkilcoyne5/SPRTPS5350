#install.packages('moments')
library(moments)
library(tidyverse)
library(here)
bat <- read_csv("data/avg-hit-speed.csv")

#Probability of Avg Hit Speed Less Than 88mph
a <- pnorm(88,mean=mean(bat$avg_hit_speed),sd=sd(bat$avg_hit_speed))
a
#Probability of Avg Hit Speed Greater Than 92 mph
b <- 1-pnorm(92,mean=mean(speed$avg_hit_speed),sd=sd(speed$avg_hit_speed))
b

#Probability of Avg Hit Speed Between 88 and 92 mph
c <- pnorm(92,mean=mean(speed$avg_hit_speed),sd=sd(speed$avg_hit_speed))

#Find Avg Hit Speed at 10th Percentile
d <- qnorm(.1,mean=mean(speed$avg_hit_speed),sd=sd(speed$avg_hit_speed))
d

#Find 95% Confidence Interval 
lower <- qnorm(.025,mean=mean(speed$avg_hit_speed),sd=sd(speed$avg_hit_speed))
upper <- qnorm(.975,mean=mean(speed$avg_hit_speed),sd=sd(speed$avg_hit_speed))

ci <- data.frame(lower,upper)

#Look at Norm Dist Stats

# Calculate Stats
total = length(bat$avg_hit_speed)
mean = mean(bat$avg_hit_speed)
sd = sd(bat$avg_hit_speed)

# Function ... nSD is the number of SD you are looking at
pData <- function(nSD){
  lo = mean - nSD*sd
  hi = mean + nSD*sd
  percent = round(sum(bat$avg_hit_speed>=lo & bat$avg_hit_speed<=hi)/total *100,2)
}


print(paste("Percent of data within 1 SD is ",pData(1),"%", sep=""))  # 86%
print(paste("Percent of data within 1 SD is ",pData(1.28),"%", sep=""))  # 86%
print(paste("Percent of data within 2 SD is ",pData(2),"%", sep=""))  # 86%
print(paste("Percent of data within 3 SD is ",pData(3),"%", sep=""))  # 93%
print(paste("Percent of data within 4 SD is ",pData(4),"%", sep=""))  # 96%
print(paste("Percent of data within 5 SD is ",pData(5),"%", sep=""))  # 97%
print(paste("Percent of data within 6 SD is ",pData(6),"%", sep=""))  # 98%


#skewness
print(skewness(bat$avg_hit_speed))
#kurtosis
print(kurtosis(bat$avg_hit_speed))

#qq plot
p <- ggplot(bat, aes(sample = bat$avg_hit_speed))
p + stat_qq() + stat_qq_line()
