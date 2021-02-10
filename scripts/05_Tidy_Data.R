#install.packages('naniar')
library(tidyverse)
library(here)
library(lubridate)
library(naniar)


#random nba dataset - https://www.reddit.com/r/dfsports/comments/khdlv0/nba_game_log_data_2020_preseason_20192020_regular/
df <- read_csv("data/nba_game_log.csv") %>% rename(PLAYER=`PLAYER NAME`)

#Convert Character to Date in the dataset - we'll cover this in a different guide
df$DATE <- lubridate::dmy(df$DATE)


#Average Points Per Game
df %>%
  group_by(TEAM,POS) %>%
  summarise(n = sum(PTS))

#Create a new position name
nba <- df %>% 
  mutate(POSITION=if_else(POS %in% c("PG","G"),"Guards",
                  if_else(POS %in% c("F","SF","SG"),"Wings","Bigs")))

#Spread function
nba %>% 
  group_by(TEAM,POSITION) %>% 
  summarise(PTS= sum(PTS),n=n()) %>% 
  filter(n>10) %>%  #remove all-star game teams
  select(-n) %>% #remove count function
  spread(POSITION,PTS)

wings <- nba %>% 
  group_by(TEAM,POSITION) %>% 
  summarise(PTS= sum(PTS),n=n()) %>% 
  filter(n>10) %>%  #remove all-star game teams
  select(-n) %>% #remove count function
  spread(POSITION,PTS) %>%  # use spread function
  mutate(percent_wings = Wings / (Wings+Guards+Bigs) * 100) %>% #see how many points come from wings
  arrange(desc(percent_wings))

p <- ggplot(wings, aes(y = reorder(TEAM, percent_wings), x = percent_wings)) + geom_bar(stat = "identity",fill="gray")
q <- p + labs(x="Percentage of Points from Wings",y="Team")
q

## Try doing the above with bigs and guards


###GATHER###
wings %>%  
  select(-c(percent_wings)) %>% 
  gather("Position", "Points", 2:4, convert = TRUE)

###SEPARATE###
nba %>% 
    separate(DATE, c("year","month","day"), sep = "-")

nba %>% 
  separate(DATE, c("year","month","day"), sep = "-",convert=TRUE)

###UNITE###
nba %>% 
  separate(DATE, c("year","month","day"), sep = "-",convert=TRUE) %>% 
  unite("date", month, day, year, sep = "/")

###REPLACE A VALUE WITH NA###
nba <- nba %>% 
  replace_with_na(replace = list(POS = c("F")))

#How many NAs are there?
nba %>% 
  group_by(POS) %>% 
  count(n=n())

#Remove the NA
nba <- nba %>% 
  replace_na(replace = list(POS = "F"))

#Check your work
nba %>% 
  group_by(POS) %>% 
  count(n=n())

#Change NA to Zero in Base R. I use this function all the time
nba[is.na(nba)] <- 0

#REPLACE F with NAs again
nba <- nba %>% 
  replace_with_na(replace = list(POS = c("F")))

#Drop NA
nba_1 <- drop_na(nba, POS)

#Count rows in NBA dataset
dim(nba)
#Count rows in NA dataset
dim(nba_1)
