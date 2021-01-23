#2020 MLB statcast data through Sep 15
library(tidyverse)
df %>% write_csv("data/fastball-pct.csv")
full_df <- read_csv("data/pitching.csv")

#Look at your data
View(df)
dim(df) # see the dimensions of your data in rows and columns (11,358 rows, 6 columns)
dim(full_df)  # see the dimensions of your data in rows and columns (207,058 rows, 6 columns)
names(df) # see the names of your columns
head(df) # see the first 5 rows of your data
tail(df) # see the last 5 rows of your data
str(df)
#########FILTER###########

#pitchers that threw fastballs 100% of the time in a given outing
filter(df, fb_pct == 1)

#filter - Extract rows that meet logical criteria. 
degrom <- filter(df, player_name == "Jacob deGrom")
head(degrom)

#plot it on game-by-game level
ggplot(data = degrom, mapping = aes(game_date, fb_pct)) +
  geom_line()

#filter two things - threw fastball more than 90% of time, threw more than 50 pitches to LHB
filter(df, fb_pct > .9, pitches > 50, stand == "L")


# 1. Create a vector that contains the name of each of your group members.
# 2 Extract all rows whose name value appears in the vector
# 3 Recreate the plot. Choose an aesthetic to distinguish different names

#get data for deGrom and Kershaw
gnames <- c("Jacob deGrom",  "Clayton Kershaw")
group_names <- filter(df, player_name %in% gnames)
#plot it
ggplot(group_names, aes(game_date, fb_pct, color = player_name)) +
  geom_line()

#plot data based on batter side
ggplot(group_names, aes(game_date, fb_pct, color = interaction(player_name, stand))) +
  geom_line()


### Let's use the pitch-by-pitch dataset

#see the slowest pitches
arrange(full_df,release_speed)

#the fastest pitches
arrange(full_df,desc(release_speed))


# Which pitcher threw the most pitches in a game (grouped by batter stand)?

arrange(df,desc(pitches))


# In what game did deGrom throw the most pitches to an RHB (grouped by batter stand)?

arrange(degrom,desc(pitches))


# Select - Extract columns by name

#select a range of columns
select(df,player_name:game_date)

names(df)
#select every column but
select(df,-c(fb,fb_pct))


#mutate - create new columns
mutate(full_df,spin_factor = release_spin_rate / release_speed)
mutate(full_df,spin_factor = release_spin_rate / release_speed, inverse = sqrt(spin_factor))

#drop the old data
transmute(full_df,spin_factor = release_spin_rate / release_speed, inverse = sqrt(spin_factor))


full_df <- na.omit(full_df) #omit any rows that have an NA in it


#summarise
summarise(full_df, avg_release_speed = mean(release_speed), n = n())

summarise(full_df,
          n = n(), # Number of pitches / rows
          n_pitch_types = n_distinct(pitch_type) # number of unique pitch types
)


#1. Add a new column to df that converts fb_pct to a percent

mutate(df, percent = fb_pct * 100)


#2. Determine how many unique names appear in the fulldf data set.

#3. Create a summary that displays the min, mean, and max release_speed for
#   Jacob deGrom fastballs (FF) in full_df

#4. Write a filter statement to find all of Jacob deGrom's fastballs thrown harder than 100 mph
#   Write a summary statement to count how many pitches were thrown more than 100 mph



summarize(full_df, n = n_distinct(player_name))

degrom <- filter(full_df, player_name == "Jacob deGrom", pitch_type == "FF")
summarise(degrom, min = min(release_speed), mean = mean(release_speed),
          max = max(release_speed))

degrom2<- filter(full_df, player_name == "Jacob deGrom", pitch_type == "FF"&release_speed >100)
summarise(degrom2, nn = n())


### %>% (Pipe - CTRL + SHIFT + M) ###
#Passes result on left into first argument of function on right.

full_df %>% 
  filter(player_name=="Jacob deGrom")

full_df %>% 
  filter(player_name=="Jacob deGrom",pitch_type == "FF") %>% 
  dplyr::summarise(min = min(release_speed), mean = mean(release_speed),
            max = max(release_speed))


#grabbing data for class 2 bonus exercises 
full_df %>% 
  filter(pitch_type=="FF") %>% 
  group_by(player_name) %>% 
  summarise(mph=mean(release_speed),rpm=mean(release_spin_rate),n=n()) %>% 
  filter(n>300) %>% write_csv('data/fb_pitches.csv')

#Rewrite the code below to use the pipe operator. 
#Then run it to ensure that it works.

full_df
full_df2 <- mutate(full_df, spin_factor = release_spin_rate/release_speed)
full_df3 <- filter(full_df2, spin_factor > 45)
summarise(full_df3, nn = n())





full_df %>% 
  mutate(spin_factor = release_spin_rate/release_speed) %>% 
  filter(spin_factor > 45) %>% 
  summarise(nn = n())




###Grouping ###

#Are there more curveballs or sliders in full df?
curves <- full_df %>%
  filter(pitch_type == "CU") %>%
  summarise(total = n())

curves

sliders <- full_df %>%
  filter(pitch_type == "SL") %>%
  summarise(total = n())

sliders


### Group by and summarize

full_df %>%
  group_by(pitch_type) %>%
  summarise(mph = mean(release_speed), rpm = mean(release_spin_rate), n = n())

#Make it nicer 
full_df %>%
  group_by(pitch_type) %>%
  summarise(mph = mean(release_speed), rpm = mean(release_spin_rate), n = n()) %>% 
  filter(n>10) %>% #at least 10 pitches per category  
  arrange(desc(mph)) #sorted on avg mph


#Group by a common value

#pitches seen by batter handedness
df %>%
  group_by(stand) %>%
  summarise(total = sum(pitches))


##Does release speed on fastballs and sinkers change over time?
 

full_df %>% 
  filter(pitch_type == "FF"|pitch_type=="SI") %>% #Use | as 'OR' 
  group_by(game_date,pitch_type) %>%
  summarise(mph = mean(release_speed)) %>%
  ggplot(aes(x =game_date, y = mph, color = pitch_type)) +
  geom_line()


#ungroup - Removes grouping criteria from a data frame.

full_df %>%
  group_by(player_name,pitch_type) %>%
  ungroup()


# Plot Zack Greinke's average fastball velocity over time 
# Does it change?

  
full_df %>%
  filter(player_name=="Zack Greinke" & pitch_type=="FF") %>% 
  group_by(game_date) %>%
  summarise(mph = mean(release_speed)) %>%
  ggplot(aes(game_date, mph)) + geom_line()

# Remove all pitchers that threw less than 250 pitches from full_df


full_df %>% 
  group_by(player_name) %>% 
  summarise(n=n()) %>% 
  filter(n<250)



### END AT SLIDE 104 ###
