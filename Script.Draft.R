library(tidyverse)
library(factoextra)
library(ggplot2)
library(Momocs)

# Load data on bird beak measurements and diets
data <- read_csv("AVONETdata_BirdTree_Pigot2020.csv") 

data.norm <- data %>% 
  mutate(w.l.ratio = Beak.Length_Culmen / Beak.Width)
data.norm$w.l.ratio


# Perform PCA on the beak measurement
ggplot(data, aes(x = Beak.Width, y = Beak.Length_Culmen, color = Trophic.Level)) +
  geom_point() + geom_smooth(method="lm")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')

ggplot(data.norm, aes(x = Trophic.Niche, y = w.l.ratio, color = Trophic.Level)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(quantile(data.norm$w.l.ratio, 0.1), quantile(data.norm$w.l.ratio, )))

