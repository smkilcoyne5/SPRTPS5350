#install.packages("tidyverse")
#install.packages("moments")
library(tidyverse)

data <- hw5_q1
summary (data)
#mean = 244.4, median = 243.0, etc

#probability of x<=200
a <- pnorm(200, mean = mean(data$Goals), sd=sd(data$Goals))
a
#Answer = 6.78%

#probability of x>250
b <- 1-pnorm(250, mean = mean(data$Goals), sd=sd(data$Goals))
b
#Answer = 42.6%

#probability of x<200 or x>250
a+b
#Answer = 49.3%

#probability of x>=275
c <- pnorm(275, mean=mean(data$Goals), sd=sd(data$Goals))
d <- pnorm(300, mean=mean(data$Goals), sd=sd(data$Goals))
d-c
#Answer = 12.1%

#X given Cum. Perc.
e <- qnorm(.3, mean=mean(data$Goals), sd=sd(data$Goals))
e
#Answer = 228.8

#find 80% confidence interval
lower <- qnorm(.1, mean=mean(data$Goals), sd=sd(data$Goals))
upper <- qnorm(.9, mean=mean(data$Goals), sd=sd(data$Goals))

ci <- data.frame(lower, upper)
View(ci)
#Answer: 80% CI -> (206.28, 282.56)