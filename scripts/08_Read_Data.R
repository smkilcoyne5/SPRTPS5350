Packages <- c("lubridate","tidyverse","rvest")
lapply(Packages, library, character.only = TRUE)
# Download the data
u <- "https://projects.fivethirtyeight.com/nba-model/nba_elo.csv"
elo.df <- read.csv(u)



