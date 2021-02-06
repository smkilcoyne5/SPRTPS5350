
#mlb_batting_averages <- read_csv('data/mlb_batting_averages.csv')
#JO: this all looks good. just write the line of code that reads the data in. 
# also shorter object names are good, instead of mlb_batting_averages do something
# like 'mlb' or even just 'df'. also, give the script a readable 
# descriptive short name 

#install.packages("tidyverse")
library("tidyverse")
#Question 2 of Homework 2 in R
View(mlb_batting_averages)
#a) compute mean, median, firstQ, thirdQ of batting average
batting <- mlb_batting_averages %>% select(BA)
summary(batting)
#mean = .273, median = .275, firstQ = .254, thirdQ = .291
#b)compute range, IQR, variance, std dev, coef of var
summary_stats <- batting %>% 
  summarise(range = max(BA) - min(BA), IQR = quantile(BA, probs=0.75) - quantile(BA, probs=0.25),
            var=var(BA), sd = sd(BA), coeff_var = sd(BA)/mean(BA))
summary_stats
#c) construct a boxplot
batting %>% ggplot(aes(x="",y=BA)) + geom_boxplot() + labs (x = "Batting Average")
#d) based on results, what would you expect typical MLB player's batting average?
    #expect batting average to be between .254 and .291
