hw3_q1 <- read_csv('data/hw3_q1.csv')
names(hw3_q1)
hw3_q1 <- hw3_q1 %>% rename(Budget_Millions=Budget,PTS=Points) #JO adding this so code runs
names(hw3_q1)
hw3_q1 %>% 
  ggplot(aes(x = Budget_Millions, y = PTS)) + geom_point()
#scatterplot of budget vs. pts
hw3_q1 %>% 
  summarise(cor(Budget_Millions, PTS), cov(Budget_Millions, PTS))
#computing coefficient of correlation and covariance
hw3_q4 <- read_csv('data/hw3_q4.csv')
names(hw3_q4)
names(hw3_q1)
hw3_q4 <- hw3_q4 %>% rename(Free_Throw_Percentage=ft_pct,Three_Point_Percentage=
                              three_pct)
hw3_q4 %>% 
  ggplot(aes(x = Free_Throw_Percentage, y = Three_Point_Percentage)) + geom_point()
#scatterplot of FT% vs. 3P%
hw3_q4 %>% 
  summarise(cor(Free_Throw_Percentage, Three_Point_Percentage), cov(Free_Throw_Percentage, Three_Point_Percentage))
#computing coefficient of correlation and covariance
nba_r <- hw3_q4 %>% 
  na.omit() %>% 
  select(-c(Player, Pos)) #%>% 
  #as.matrix() JO: Commenting Out to make it look nice.

cor_nba <- round(cor(nba_r), 2)
cor_nba

