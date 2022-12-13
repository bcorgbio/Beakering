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


data <- read_csv("AVONETdata_BirdTree_Pigot2020.csv") 

data.norm <- data %>% 
  mutate(l.w.ratio = Beak.Length_Culmen / Beak.Width)

data.updated <- data.norm[!is.na(data$Trophic.Level),]
data.updated <- data.updated[!is.na(data.updated$Trophic.Niche),]
#updated has na omitted, updated 2 has bsize/mass ratio
data.updated2 <- data.updated %>% 
  mutate(bsize.bodymass.ratio = l.w.ratio / Mass)

#data.log has all the transformed values
data.log <- data.updated2 %>% 
  mutate(l.log = log10(data.updated2$Beak.Length_Culmen)) %>% 
  mutate(w.log = log10(data.updated2$Beak.Width)) %>% 
  mutate(mass.log = log10(data.updated2$Mass)) %>% 
  mutate(l.w.log = l.log/w.log) %>% 
  mutate(lw.mass.log = l.w.log/mass.log)

