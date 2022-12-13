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
library(ggplot2)
library(reshape2)


# Load data on bird beak measurements and diets to new data file, 'data'
data <- read_csv("AVONETdata_BirdTree_Pigot2020.csv") 

#We mutated the df called data to add another column that pseudo-normalizes the length/ width proportions
data.norm <- data %>% 
  mutate(l.w.ratio = Beak.Length_Culmen / Beak.Width)
data.norm$l.w.ratio


#We mutated the newly formed data set to take out all of the NA birds at the Trophic Level
data.updated <- data.norm[!is.na(data$Trophic.Level),]
data.updated <- data.norm[!is.na(data$Trophic.Niche),]

#Made a plot that compares beak width to length on a log scale, colored by Trophic Level
ggplot(data.updated, aes(x = Beak.Width, y = Beak.Length_Culmen, color = Trophic.Level, type = "n")) +
  geom_smooth(method="lm")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10')

#Made a plot that compares beak length/width on a log scale, colored by Trophic Level
ggplot(data.norm, aes(x = Trophic.Niche, y = l.w.ratio, color = Trophic.Level)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(quantile(data.norm$l.w.ratio, 0.0), quantile(data.norm$l.w.ratio, 0.999)))

#Made a plot that compares beak length/width on a log scale, colored by Trophic Niche
ggplot(data.norm, aes(x = Trophic.Niche, y = l.w.ratio, color = Trophic.Niche)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(quantile(data.norm$l.w.ratio, 0.0), quantile(data.norm$l.w.ratio, 0.999)))
#In here, we added some lines to clean up the view & rotate the text on the x axis


#Mutated the df again, this time adding a body size to l/w ratio column 
data.updated <- data.norm %>% 
  mutate(bsize.bodymass.ratio = l.w.ratio / Mass)
  data.updated$bsize.bodymass.ratio

#Plot of the new body size to mass ratio separated by color and log transformed
ggplot(data.updated, aes(x = Trophic.Level, y = bsize.bodymass.ratio, color = Trophic.Level)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(quantile(data.updated$bsize.bodymass.ratio, 0.5), quantile(data.updated$bsize.bodymass.ratio, 0.979)))
 


#This graph below seems repetitive/ basically the same as the one above
#Investigating the L/W ration over mass, colored by tropic level
ggplot(data.updated, aes(x = l.w.ratio, y = Mass, color = Trophic.Level, type="n")) +
  geom_smooth(method="lm")+
  scale_y_continuous(trans='log10')+
  scale_x_continuous(trans='log10') 




# beak length = height of pyramid, beak depth = length of side, beak width = width of pyramid
#data


# 
# Don't have volume rn... 
# 
# ggplot(data.norm, aes(x = l.w.ratio, y = beak.volume, color = Trophic.Level, type="n")) +
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
ggplot(data.updated,aes(x = Beak.Width, y = Trophic.Level, color = Trophic.Level)) +
  geom_smooth(method="lm")
#Length vs. Trophic Level
ggplot(data.updated, aes(x = Beak.Length_Culmen, y = Trophic.Level, color = Trophic.Level)) +
  geom_smooth(method="lm")

#I think we should maybe try to normalize at least the length compared to tail lengths or something

unique(data$Habitat.Density)

#Three-dimensional scatter plot using the scatterplot3d() function
# scatterplot3d(x = data.updated$Beak.Length_Culmen, 
#               y = data.updated$Mass, 
#               z = data.updated$Beak.Width)
# plot(df, type="n") + # suppress the plotting of the individual points
# lines(df)  # plot the best fit lines

 
#  stat_density2d(aes(fill = ..density..), geom = 'tile', contour = F) +
#  scale_fill_distiller(palette = 'RdYlBu')


ggplot(data.norm, mapping = aes(x = l.w.ratio, y = Trophic.Niche, color = Trophic.Level)) + 
  geom_point() +
  facet_wrap(~Habitat) 



