# Use mlb_batting_averages.csv to complete question number 2 in R

# here is helper guide

pitches <- read_csv("data/fb_pitches.csv")

#a) Compute mean, median, first quartile and third quartile of BA using Excel functions
#b) Compute the range, interquartile range, variance, standard deviation and coefficient of variation using excel functions
#c) Construct a boxplot
#d) Based on the results of (a) through (c), what would you expect a typical MLB player's batting average to be?

mph <- pitches %>% select(player_name,mph)

head(mph)

summary(mph) #get summary stats

summary_stats <- mph %>% 
  summarise(range=max(mph)-min(mph),iq_range=quantile(mph,probs=0.75)-quantile(mph,probs=0.25),
            var=var(mph),sd=sd(mph),coeff_var=sd(mph)/mean(mph))

summary_stats

#boxplot

mph %>% ggplot(aes(x= "",y=mph))+geom_boxplot()  + labs(x = "Fastball Speed (mph)")

