#--------------
# LOAD PACKAGES
#--------------
library(tidyverse)
library(factoextra)
library(ggplot2)
library(Momocs)
library(scatterplot3d)
library(viridis)
library(RColorBrewer)

# Load data on bird beak measurements and diets
data <- read_csv("AVONETdata_BirdTree_Pigot2020.csv") 

data.norm <- data %>% 
  mutate(w.l.ratio = Beak.Length_Culmen / Beak.Width)
data.norm$w.l.ratio



data.updated <- data[!is.na(data$Trophic.Level),]
# Perform analysis on the beak measurement
ggplot(data.updated, aes(x = Beak.Width, y = Beak.Length_Culmen, color = Trophic.Level, type = "n")) +
  geom_smooth(method="lm")+
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


# 
# Don't have volume rn... 
# 
# ggplot(data.norm, aes(x = w.l.ratio, y = beak.volume, color = Trophic.Level, type="n")) +
#   geom_point() + geom_smooth(method="lm")+
#   scale_y_continuous(trans='log10')+
#   scale_x_continuous(trans='log10') 
#   
# 
# 
# colnames(data.norm)
# head(data)
# colnames(data)
# 
# data.norm <- data.norm %>% 
#   mutate(beak.volume = Beak.Length_Culmen*Beak.Depth*Beak.Width*0.3) 
# data.norm$beak.volume
# 
# #Volume and Mass
# ggplot(data.norm, aes(x = Mass, y = beak.volume, color = Trophic.Level, type="n")) +
#   geom_point() + geom_smooth(method="lm")+
#   scale_y_continuous(trans='log10')+
#   scale_x_continuous(trans='log10')+
#   geom_point(na.rm = TRUE)




#Width vs. Trophic Level
ggplot(data, 
       aes(x = Beak.Width, 
           y = Trophic.Level, 
           color = Trophic.Level)) +
  geom_smooth(method="lm")
#Length vs. Trophic Level
ggplot(data, aes(x = Beak.Length_Culmen, y = Trophic.Level, color = Trophic.Level)) +
  geom_smooth(method="lm")

#I think we should maybe try to normalize at least the length compared to tail lengths or something

unique(data$Habitat.Density)

#Three-dimensional scatter plot using the scatterplot3d() function
scatterplot3d(x = data.norm$Beak.Length_Culmen, y = data.norm$Mass, z = data.norm$Beak.Width)
# plot(df, type="n") + # suppress the plotting of the individual points
# lines(df)  # plot the best fit lines

 
#  stat_density2d(aes(fill = ..density..), geom = 'tile', contour = F) +
#  scale_fill_distiller(palette = 'RdYlBu')
