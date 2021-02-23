library(tidyverse)

df <- read_csv("data/winston.csv")

conf = qnorm(.975) #calculate z value at .975 mark for 2 tails at 95% CI
sd = sd(df$yards_gained) #calculate SD
level = pnorm(conf)-(1-pnorm(conf)) #calculate confidence level

# Population Standard Deviation

df %>% 
  summarise(lower=mean(yards_gained)-(conf*(sd/sqrt(n()))),mean=mean(yards_gained),
            upper=mean(yards_gained)+(conf*(sd/sqrt(n())))) %>% 
  mutate(conf=level)

# Get one game of data
one_game <- 
df %>% 
  filter(game_no==1) %>% 
  t.test(yards_gained)

# Run a t-test
t.test(one_game$yards_gained) 

# Load Harden Data
df_p <- read_csv('data/harden.csv')
head(df_p)

# Get 10 games of data
ten_games <- df_p %>% 
  filter(G<=10) %>% 
  dplyr::summarise(tp=sum(tp),tpa=sum(tpa))

# Get a confidence interval for the proportion. I'm 
# t-testing it vs 40% three point percentage
prop.test(ten_games$tp, ten_games$tpa, p = .4,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)

# Get Yankees run scored data
df_r <- read_csv("data/yankees.csv")
st_dev = sd(df_r$R) #population SD
E = 1 # sampling error

#Sample Size Calc
n = conf^2 ∗ st_dev^2/ E^2 
round(n,0) # To estimate the Yankees runs per game +/- 1, we need a sample size of at least 44 games
