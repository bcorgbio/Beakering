library(tidyverse)
library(factoextra)
library(ggplot2)
library(Momocs)

# Load data on bird beak measurements and diets
data <- read_csv("AVONETdata_BirdTree_Pigot2020.csv")

data.norm <- data %>% 
  mutate(w.l.ratio = Beak.Width / Beak.Length_Culmen)
data.norm$w.l.ratio
# Perform PCA on the beak measurement
ggplot(data, aes(x = Beak.Width, y = Beak.Length_Culmen, color = Trophic.Level)) +
  geom_point() + geom_smooth(method="lm")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')
