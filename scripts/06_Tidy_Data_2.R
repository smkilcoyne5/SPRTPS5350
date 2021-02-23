library(tidyverse)

df <- read_csv("data/harden.csv")
df$Date <- as.Date(df$Date,format="%m/%d/%Y")
three_days <- df %>% 
  filter(G<=3) %>% 
  select(Date,tpa) %>% 
  spread(key = Date, value = tpa)
three_days
three_days %>% 
  gather(key = "date", value = "tpa", 1:3)

df %>% 
  separate(Date, c("year","month","day"), sep = "-")

head(df)
split <- df %>% 
  separate(Date, c("year","month","day"), sep = "-",convert=TRUE) 

split %>% 
  unite("date",month,day,year,sep="/")

# replace NA with 0
df_a <- df %>% 
  replace_na(replace = list(c(tp = 0,tpa=0))) 

# remove rows that have NA
df %>% 
  drop_na(tpa)
  
             