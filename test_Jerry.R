library(tidyverse)

what = read_csv('AVONETdata_BirdTree_Pigot2020.csv')
what

unique(what$Trophic.Level)

unique(what$Trophic.Niche)

length(unique(what$Species3))

length(unique(what$Family3))

fam_diet <- what %>% 
  select(Family3,Species3,Trophic.Level)

fam_diet %>% 
  group_by(Family3) %>% 
  summarise()

