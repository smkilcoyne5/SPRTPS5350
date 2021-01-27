library(tidyverse)
library(here)

#Import some Formula 1 Data
construct <- read_csv("data/constructors.csv")
races <- read_csv('data/races.csv')
drivers <- read_csv('data/drivers.csv')
results <- read_csv('data/results.csv')

## Join results to drivers and calculate total number of points by driver ##
# Try to do some different summarise formulas like mean points, count of points, sd of points 
results %>% 
  inner_join(drivers,by=c("driverId"="driverId")) %>%  #join the results table to the drivers table on driverId  
  group_by(driverId,driverRef) %>% #group on driver name
  summarise(points=sum(points)) %>% #calculate total points
  arrange(desc(points)) #put in descending order

## Total number of races by circuit and most recent winner of the race
# Try to change some things and do a different analysis
results %>% 
  inner_join(races,by=c("raceId"="raceId")) %>% 
  filter(positionOrder==1) %>% #keep winning driver position
  inner_join(drivers,by=c("driverId"="driverId")) %>% #join to driver table
  select(circuitId,name,year,driverId,driverRef) %>% 
  rename(Winner=driverRef) %>%  #rename the column driverRef
  group_by(circuitId,name) %>% #group on it
  summarise(first = first(year),last=last(year),last_winner=last(Winner),total_races=n()) %>%   # see when first,last year of race was and then count how many races have been held by circuit
  arrange(desc(total_races)) #arrange in descending order of total number of races by

#Distinct - Removes rows with duplicate values (in a column).
races %>% 
  distinct(name) #show me all the different F1 circuits


#Lewis Hamilton career race data

hamilton <- results %>% 
  inner_join(drivers,by=c("driverId"="driverId")) %>% 
  inner_join(races,by=c("raceId"="raceId")) %>% 
  inner_join(construct,by=c("constructorId"="constructorId")) %>%
  select(year,round,surname,constructorRef,grid,positionOrder) %>% 
  filter(surname == "Hamilton"&year>2006) %>% 
  rename(Qualify_Pos=positionOrder,Finish_Pos=grid)

#Try to calcuate number of wins, number of races run by team, average difference between
#qualify position and finishing position
#Is Hamilton better at qualifying 1st or winning the race?

names(hamilton)

