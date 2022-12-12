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

ggplot(data.norm, aes(x = Trophic.Niche, y = w.l.ratio, color = Trophic.Niche)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(quantile(data.norm$w.l.ratio, 0.0), quantile(data.norm$w.l.ratio, 0.999)))

data.norm <- data.norm %>% 
  mutate(bsize.bodymass.ratio = w.l.ratio / Mass)
data.norm$bsize.bodymass.ratio

ggplot(data.norm, aes(x = Trophic.Level, y = bsize.bodymass.ratio, color = Trophic.Level)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(quantile(data.norm$bsize.bodymass.ratio, 0.5), quantile(data.norm$bsize.bodymass.ratio, 0.979)))

ggplot(data.norm, aes(x = w.l.ratio, y = Mass, color = Trophic.Level, type="n")) +
  geom_point() + geom_smooth(method="lm")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10') 

# beak length = height of pyramid, beak depth = length of side, beak width = width of pyramid
data



ggplot(data.norm, aes(x = w.l.ratio, y = beak.volume, color = Trophic.Level, type="n")) +
  geom_point() + geom_smooth(method="lm")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10') 

colnames(data.norm)
head(data)
colnames(data)

data.norm <- data.norm %>% 
  mutate(beak.volume = Beak.Length_Culmen*Beak.Depth*Beak.Width*0.3) 
data.norm$beak.volume

ggplot(data.norm, aes(x = Mass, y = beak.volume, color = Trophic.Level, type="n")) +
  geom_point() + geom_smooth(method="lm")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')+
  geom_point(na.rm = TRUE)



