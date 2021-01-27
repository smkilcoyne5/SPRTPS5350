full_df <- read_csv("data/pitching.csv")
head(full_df)

fb <- full_df %>% filter(pitch_type == "FF")

#Covariance - use = "complete.obs" removes NA
cov(fb$release_speed,fb$release_spin_rate,use = "complete.obs")

#Correlation
cor(fb$release_speed,fb$release_spin_rate,use = "complete.obs")


# Run some correlations 
full_df %>%
  na.omit() %>% 
  filter(pitch_type %in% c("SL", "CU", "FS", "KC", "CH", "FC","FF")) %>%  
  group_by(pitch_type) %>% 
  summarise(r = cor(release_speed, release_spin_rate)) %>% 
  arrange(desc(r))


## FF = 4 seam fastball
## Fs = 2 seam fastball
## CU = Curveball
## FC = Cutter
## CH = Change
## SL = Slider
## KC = Knuckle Curve


#Make a correlation matrix - very powerful exploratory analysis!

nba <- read_csv("data/nba_player_stats_1920.csv")
names(nba)
head(nba)

str(nba)
round(cor(my_data), 2)
nba_r <- nba %>%
  na.omit() %>% 
  select(-c(Player,Pos,Tm)) %>% 
  as.matrix() 

cor_nba <- round(cor(nba_r), 2)

View(cor_nba)

