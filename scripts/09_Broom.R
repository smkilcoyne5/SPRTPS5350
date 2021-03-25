#load some packages
library(modelr)
library(tidyverse)
library(corrplot)
library(caret)
#load 2019 pitching data
df <- read_csv('data/pitch.csv')

#remove a few columns
df <- df %>% select(-c(W,L,SV,G,GS,playerid))

#Do some histograms/boxplot
df %>%
  ggplot(aes(ERA)) + geom_histogram(binwidth = .5)

df %>%
  ggplot(aes(ERA)) + geom_histogram(binwidth = .25)

df %>% 
  ggplot(aes(K9)) + geom_boxplot()

df %>% 
  ggplot(aes(HR9)) + geom_boxplot()

df %>% 
  ggplot(aes(BB9)) + geom_boxplot()

#Do some correlations
cor_df <- df %>% select(IP:WAR)
str(cor_df)
correlations <- round(cor(cor_df),2)
correlations
corrplot(correlations,order="hclust") #show correlations

highCorr <- findCorrelation(correlations,cutoff=.9,names=TRUE) #find highly correlated predictors
highCorr

#Regression model
m1 <- lm(ERA~K9,data=df)
m1
class(m1) #check the data type
summary(m1) #look at the regression stats

#put the regression in tidyverse code
m1 <- df %>% 
  lm(ERA~K9,data=.)

library(broom)

#Look at a few things
m1 %>% tidy() %>% View() #look at b0 and b1

m1 %>% glance() %>% View() # look at the regression stats

m1 %>% augment() %>% View() #look at the output values

m1 %>% augment(data=df) %>% View() #bind the output values to the data

#new regression
m2 <- df %>%
  lm(ERA ~ HR9, data = .) 

m2 %>% 
  tidy()

#use glance and just grab r-squared and p-value
m2 %>% 
  glance() %>% 
  select(adj.r.squared,p.value)

#grab only the data where p value is less than .05
m1 %>% 
  tidy() %>% 
  filter(p.value < .05)

m2 %>% 
  tidy() %>% 
  filter(p.value < .05)

#multivariate regression
m3 <- df %>% 
  lm(ERA ~ K9 + HR9, data = .)

m3 %>% 
  tidy()

#another multivariate regression
m4 <- 
  df %>% 
  lm(ERA ~ K9 + HR9 + BB9, data = .)

m4 %>% 
  tidy()

m4 %>% 
  glance()

#add predicted values to model
df %>%
  add_predictions(m4) 

df_model <- df %>%
  add_predictions(m4) 

#look at some summary stats
summary(m4)
#look at residual plots
plot(m4)

#add residuals to the model
df %>%
  add_residuals(m4)

#plot the residuals for all 4 models
df %>%
  gather_residuals(m1,m2,m3,m4) %>% 
  ggplot(aes(ERA, resid)) +
  geom_bin2d() +
  geom_smooth() +
  facet_wrap(~model)


formulas <- list(
  m1 = ERA ~ K9,
  m2 = ERA ~ HR9,
  m3 = ERA ~ K9 + HR9,
  m4 = ERA ~ K9 + HR9 + BB9
)

#look at model stats for all 4 models
formulas %>%
  map(lm, data = df) %>%
  map_df(glance,.id="model") %>% View()

#you could also do an anova
anova(m1,m2,m3,m4)
