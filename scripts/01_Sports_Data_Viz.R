#install these packages if you haven't done so already
#install.packages("gridExtra") 
#install.packages("tidyverse")
#install.packages("here")

#Load the packages
library(tidyverse)
library(here)
library(gridExtra)

#Read the data
df <- read_csv("data/nba_player_stats_1920.csv")
# Basic plotting in R
plot(df$TRB,df$BLK) 
# View the NBA player data
View(df)

#Scatterplot of the NBA data in GGplot
ggplot(data = df) +
  geom_point(mapping = aes(x = TRB, y = BLK))

#Aesthetics
ggplot(df) + geom_point(aes(x = TRB, y = BLK, color = Pos)) 
ggplot(df) + geom_point(aes(x = TRB, y = BLK, size = Pos))
ggplot(df) + geom_point(aes(x = TRB, y = BLK, shape = Pos))
ggplot(df) + geom_point(aes(x = TRB, y = BLK, alpha = Pos)) #transparency of the datapoint

#Set vs Map
ggplot(df) + geom_point(aes(x = TRB, y = BLK, color = "green")) # color = green adds text to the legend
ggplot(df) + geom_point(aes(x = TRB, y = BLK), color = "green") # color of the dot is now green

#Facets
small_df <- df %>% filter(Tm %in% c("BOS","DEN","MIL","ATL","SAS"))  #We'll cover filter shortly but all we're doing here is keeping a few teams worth of data
q <- ggplot(new_df) + geom_point(aes(x = TRB, y = BLK))
q + facet_grid(. ~ Pos) #Position in the columns
q + facet_grid(Tm ~ .) #Teams in the rows
q + facet_grid (Tm ~ Pos) #Teams in the rows and Position in the columns
q + facet_wrap(~ Pos) #1D Ribbon wrapped into 2D

#What's similar about these plots?
#Same x-axis, same y-axis, same data
#What's different?
#geometric object (geom point vs geom smooth)
par(mfrow = c(1,2))
x <- ggplot(data = df) +
  geom_point(mapping = aes(x = TRB, y = BLK))
y <- ggplot(data = df) +
  geom_smooth(mapping = aes(x = TRB, y = BLK))
grid.arrange(x,y, ncol=2, nrow = 1)

#Make a boxplot
ggplot(df) +
  geom_boxplot(mapping = aes(x = Pos, y = TRB))

#Order the positions in the typical order of PG -> C
df$Pos <- factor(df$Pos , levels=c("PG", "SG", "SF", "PF","C"))  

#Plot it again
ggplot(df) +
  geom_boxplot(mapping = aes(x = Pos, y = TRB))

#Add a layer
ggplot(df) +
  geom_point(mapping = aes(x = TRB, y = BLK)) +
  geom_smooth(mapping = aes(x = TRB, y = BLK))

#Global vs Local
df_guards <- df %>% filter(Pos %in% c("PG","SG")) #lets just use guards for this example
#Local -
ggplot(data = df_guards, mapping = aes(x = TRB, y = BLK)) +
  geom_point(mapping = aes(color = Pos)) + #Position color is only on geom_point
  geom_smooth()

ggplot(data = df_guards, mapping = aes(x = TRB, y = BLK)) +
  geom_point(mapping = aes(color = Pos)) +
  geom_smooth(mapping = aes(color = Pos), se = FALSE) #Draw a line based on the color. se = false removes error bars


ggplot(data = df_guards, mapping = aes(x = TRB, y = BLK)) +
  geom_point(mapping = aes(color = Pos)) +
  geom_smooth(mapping = aes(group = Pos), se = FALSE) #draws sep lines for PG and SG but lines are same color
#Global
ggplot(data = df_guards, mapping = aes(x = TRB, y = BLK,color=Pos)) + #Position color on all geoms
  geom_point() + 
  geom_smooth() 

#Draw a density plot
ggplot(data = df,
       mapping = aes(x = TRB, y = BLK, color = Pos)) +
  geom_density_2d(color = "black") +
  geom_point()

#Draw a bar chart
ggplot(data = df) +
  geom_bar(mapping = aes(x = Pos))
#this gives us count by position
df %>% group_by(Tm) %>% summarise(count=n()) %>% arrange(desc(count))
#Here is a count 
ggplot(data = small_df) +
  geom_bar(mapping = aes(x = Tm))

#Count by Position using stat = count
ggplot(data = df) +
  geom_bar(mapping = aes(x = Pos),
           stat = "count")

sample <- data.frame(x = c("A", "B", "C"), y = 1:3) #create a dataframe
sample #look at it 
ggplot(data = sample) +
  geom_bar(mapping = aes(x = x, y = y), stat = "identity") #identity plots exactly whats in the dataframe

#Color,Fill,Clarity
ggplot(df, mapping = aes(x = Pos)) +
  geom_bar(mapping = aes(color = Pos)) 
ggplot(df, mapping = aes(x = Pos)) +
  geom_bar(mapping = aes(fill = Pos))

ggplot(small_df, mapping = aes(x = Pos)) +
  geom_bar(mapping = aes(fill = Tm))
#Dodge
ggplot(small_df, mapping = aes(x = Pos)) +
  geom_bar(mapping = aes(fill = Tm),
           position = "dodge")

#Stack, Dodge, Identity, Fill
p <- ggplot(small_df, aes(x = Tm, fill = Pos))
q <- p + geom_bar(position = "stack")
r <- p + geom_bar(position = "dodge")
s <- p + geom_bar(position = "identity") #overlaps
t <- p + geom_bar(position = "fill") #uses proportion

grid.arrange(q,r,s,t, ncol=2, nrow = 2)
