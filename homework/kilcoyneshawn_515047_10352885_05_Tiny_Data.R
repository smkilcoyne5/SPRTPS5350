#install.packages('naniar')
#install.packages('here')
library(tidyverse)
library(here)
library(lubridate)
library(naniar)
nba_game_log <- read_csv("data/NBA Data.csv")
df <- nba_game_log

df$DATE <- lubridate::dmy(df$DATE)

#average ppg
df %>% 
  group_by(TEAM,POS) %>% 
  summarise(n = sum(PTS))

#creating a new position name
nba <- df %>% 
  mutate(POSITION=if_else(POS %in% c("PG", "G"), "Guards",
                    if_else(POS %in% c("F","SF","SG"), "Wings", "Bigs")))

View(nba)

#spread function
nba %>% 
  group_by(TEAM, POSITION) %>% 
  summarise(PTS= sum(PTS), n=n()) %>% 
  filter(n>1) %>% #changed this because I used the preseason data
  select(-n) %>% 
  spread(POSITION, PTS)

wings <- nba %>% 
  group_by(TEAM,POSITION) %>% 
  summarise(PTS = sum(PTS),n=n()) %>% 
  filter(n>1) %>% 
  select(-n) %>% 
  spread(POSITION, PTS) %>% 
  mutate(percent_wings = Wings / (Wings+Guards+Bigs) * 100) %>% #points from wings
  arrange(desc(percent_wings))

p <- ggplot(wings, aes(y=reorder(TEAM, percent_wings), x = percent_wings)) + geom_bar(stat = "identity", fill = "gray")
q <- p + labs(x="Percentage of Points from Wings", y="Team")
q

#bigs
bigs <- nba %>%
  group_by(TEAM,POSITION) %>% 
  summarise(PTS = sum(PTS),n=n()) %>% 
  filter(n>1) %>% 
  select(-n) %>% 
  spread(POSITION, PTS) %>% 
  mutate(percent_Bigs = Bigs / (Wings+Guards+Bigs) * 100) %>% #points from bigs
  arrange(desc(percent_Bigs))

p1 <- ggplot(bigs, aes(y=reorder(TEAM, percent_Bigs), x = percent_Bigs)) + geom_bar(stat = "identity", fill = "gray")
q1 <- p1 + labs(x="Percentage of Points from Bigs", y = "Team")
q1

#guards
guards <- nba %>%
  group_by(TEAM,POSITION) %>% 
  summarise(PTS = sum(PTS),n=n()) %>% 
  filter(n>1) %>% 
  select(-n) %>% 
  spread(POSITION, PTS) %>% 
  mutate(percent_guards = Guards / (Wings+Guards+Bigs) * 100) %>% #points from guards
  arrange(desc(percent_guards))

p2 <- ggplot(guards, aes(y=reorder(TEAM, percent_guards), x = percent_guards)) + geom_bar(stat = "identity", fill = "gray")
q2 <- p2 + labs(x="Percentage of Points from Guards", y = "Team")
q2

#gather
wings %>% 
  select(-c(percent_wings)) %>% 
  gather("Position", "Points", 2:4, convert = TRUE)

#separate
nba %>% 
  separate(DATE, c("year","month","day"), sep = "-")

nba %>% 
  separate(DATE, c("year","month","day"), sep = "-", convert = TRUE)

#unite
nba %>% 
  separate(DATE, c("year","month","day"), sep = "-", convert = TRUE) %>% 
  unite("date", month, day, year, sep = "/")
#why didn't this work?
#JO: There's an issue with the date column data. You'll find that dealing with dates/times is a real coding headache :|
#replacing a value with NA
nba <- nba %>% 
  replace_with_na(replace = list(POS = c("F")))

nba %>% 
  group_by(POS) %>% 
  count(n=n())
#46 NA's

#remove NAs
nba <- nba %>% 
  replace_na(replace = list(POS = "F"))

#check
nba %>% 
  group_by(POS) %>% 
  count(n=n())
#still 46

#think theres still issues with dates

nba <- nba %>% 
  replace_with_na(replace = list(POS = c("F")))

#drop NA
nba_1 <- drop_na(nba, POS)

#count rows
dim(nba)
#29

dim(nba_1)
#29
