#Creating Dataset for Class Exercise

library(tidyverse)
library(here)
construct <- read_csv("data/constructors.csv")
races <- read_csv('data/races.csv')
drivers <- read_csv('data/drivers.csv')
results <- read_csv('data/results.csv')

h <- results %>% 
  inner_join(drivers,by=c("driverId"="driverId")) %>% 
  inner_join(races,by=c("raceId"="raceId")) %>% 
  inner_join(construct,by=c("constructorId"="constructorId")) %>%
  select(year,round,surname,constructorRef,grid,positionOrder) %>% 
  filter(surname == "Hamilton"&year>2006) %>% 
  mutate(pole=if_else(grid==1,"on_pole","not_on_pole"),
             race=if_else(positionOrder==1,"won_race","did_not_win_race")) %>% 
  select(constructorRef,pole,race) %>% 
  group_by(pole) %>% count(race) %>% 
  spread(race,n) %>% 
  adorn_totals("row","column") 

head(h)
