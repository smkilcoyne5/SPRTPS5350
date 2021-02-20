#install.packages("tidyverse") JO no need to install tidyverse everytime

#install.packages("moments") 
library(tidyverse)
bat <- read_csv("data/avg-hit-speed.csv") #JO: reading data in on my end
#bat <- avg_hit_speed

#probability of average hit speed less than 88mph
a <- pnorm(88, mean = mean(bat$avg_hit_speed), sd=sd(bat$avg_hit_speed))
a

#probability = 37.5%

#probability of average hit speed greater than 92mph
b <- 1-pnorm(92, mean = mean(bat$avg_hit_speed), sd=sd(bat$avg_hit_speed))
b

#probability = 9.5%

#probability of average hit speed between 88 and 92 mph
c <- pnorm(92, mean = mean(bat$avg_hit_speed), sd=sd(bat$avg_hit_speed))
c

c-a
#probability = 53.0% - did I do this right? I differ a bit from your example code which I thought only gave <92mph

#find avg hit speed at 10th percentile
d <- qnorm(.1, mean=mean(bat$avg_hit_speed), sd=sd(bat$avg_hit_speed))
d

#answer = 85.6mph

#find 95% confidence interval
lower <- qnorm(.025, mean=mean(bat$avg_hit_speed), sd=sd(bat$avg_hit_speed))
upper <- qnorm(.975, mean=mean(bat$avg_hit_speed), sd=sd(bat$avg_hit_speed))

ci <- data.frame(lower, upper)
View(ci)
#Answer: 95% CI -> (83.96, 93.59)

#Norm Dist Stats

#Calculate Stats
total = length(bat$avg_hit_speed)
mean = mean(bat$avg_hit_speed)
sd = sd(bat$avg_hit_speed)

#total = 257, mean = 88.78, sd = 2.45

pData <- function(nSD){
  lo = mean - nSD*sd
  hi = mean + nSD*sd
  percent = round(sum(bat$avg_hit_speed>=lo & bat$avg_hit_speed<=hi)/total *100,2)
}

print(paste("Percent of Data within 1 SD is ", pData(1), "%", sep=""))
#71.6%
print(paste("Percent of Data within 1.28 SD is ", pData(1.28), "%", sep=""))
#81.32%
print(paste("Percent of Data within 2 SD is ", pData(2), "%", sep=""))
#95.72%
print(paste("Percent of Data within 3 SD is ", pData(3), "%", sep=""))
#99.61%
print(paste("Percent of Data within 4 SD is ", pData(4), "%", sep=""))
#99.61%
print(paste("Percent of Data within 5 SD is ", pData(5), "%", sep=""))
#100%

#is the above work right? It differs slightly from your #s
# JO it's fine. more importantly, try to understand how the function works.

library(moments)

#skewness
print(skewness(bat$avg_hit_speed))
#-0.297

#kurtosis
print(kurtosis(bat$avg_hit_speed))
#4.187

#qqplot
p <- ggplot(bat, aes(sample = bat$avg_hit_speed))
p + stat_qq() + stat_qq_line()
#somewhat close to normally distributed based on qqplot, but kurtosis is pretty high
