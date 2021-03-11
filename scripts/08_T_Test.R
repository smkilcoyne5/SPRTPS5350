library(tidyverse)
df <- read_csv("data/rosario.csv")

qqnorm(df$mph)
qqline(df$mph)
# Run a t-test - Compare Rosario's data to 89 mph
t.test(df$mph,mu=89) #two-tailed
t.test(df$mph,mu=89,alternative="greater") #upper tail
t.test(df$mph,mu=89,alternative="less") #lower tail
t.test(df$mph,mu=89,alternative="two.sided") #two-tailed, you don't need to specify 'two.sided' as that's the default

# Load Harden Data
df_p <- read_csv('data/harden.csv')
head(df_p)

# Get 10 games of data
ten_games <- df_p %>% 
  filter(G<=10) %>% 
  dplyr::summarise(tp=sum(tp),tpa=sum(tpa))


#Recall the Harden example from last class - Compare his 
#first 10 games of data to a null hypothesis of 40% from 3point
prop.test(ten_games$tp, ten_games$tpa, p = .4,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)

# 65 hits in 202 At-Bats, compared to .257 batting average
prop.test(65,202, p = .257,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)

#Here's the same Conforto example - but I hard-coded this
#season vs last season so it's 65 hits in 202 AB vs 141 hits in 549 AB
res <- prop.test(x = c(65, 141), n = c(202, 549))
res

# Try changing all the numbers above and see how it impacts the tests
# Use seager and nba_bubble and dodgers data to do 
# homework questions 1,2,3,5,6. If you want to do question 4,
# try to grab the data from the homework and import into R yourself.
# Q7, you can just enter the data
