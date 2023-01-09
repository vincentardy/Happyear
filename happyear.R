
rm(list=ls())

# choose file directory --- --- --- --- --- --- --- --- ---- --- --- --- --- ---
setwd("C:/.../HappyeaR")

# Package loading --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
library(tidyr)
library(ggplot2)
library(gganimate)
library(viridis)
library(patchwork)
library(png)     

set.seed(2023)

# dataframe with "Happy New Year" coordinates  --- --- --- --- --- --- --- --- -
happyear<-read.table("happyear.csv", sep = ";", dec=",", header = TRUE)

# plot "Happy New Year" --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
plot(y~x, data=happyear)

# Random simulation of each point trajectories --- --- --- --- --- --- --- --- -
ligne=1
dataframe_x=data.frame()
dataframe_y=data.frame()

for(i in 1:nrow(happyear)){

if (happyear[i,"id"] %% 2 == 0) x=sort(runif(9, min=happyear[i,"x"], max = happyear[i,"x"]+10)) 
else x=sort(runif(9, min=happyear[i,"x"]-10, max = happyear[i,"x"]), decreasing =TRUE)

data_x=data.frame()
data_x[ligne, "points"]  <- happyear[i, "points"]
data_x[ligne, "letters"] <- happyear[i, "letters"]
data_x[ligne, "t0.5"]    <- x[9]
data_x[ligne, "t1"]      <- x[8]
data_x[ligne, "t1.5"]    <- x[7]
data_x[ligne, "t2"]      <- x[6]
data_x[ligne, "t2.5"]    <- x[5]
data_x[ligne, "t3"]      <- x[4]
data_x[ligne, "t3.5"]    <- x[3]
data_x[ligne, "t4"]      <- x[2]
data_x[ligne, "t5"]      <- x[1]
data_x[ligne, "t6"]      <- happyear[i,"x"]
data_x[ligne, "t7"]      <- happyear[i,"x"]
data_x[ligne, "t8"]      <- happyear[i,"x"]


if (happyear[i,"id"] %% 2 == 0) y=sort(runif(9, min=happyear[i,"y"], max = happyear[i,"y"]+10)) 
else y=sort(runif(9, min=happyear[i,"y"]-10, max = happyear[i,"y"]), decreasing =TRUE)

data_y=data.frame()
data_y[ligne, "points"]  <- happyear[i, "points"]
data_y[ligne, "letters"] <- happyear[i, "letters"]
data_y[ligne, "t0.5"]    <- y[9]
data_y[ligne, "t1"]      <- y[8]
data_y[ligne, "t1.5"]    <- y[7]
data_y[ligne, "t2"]      <- y[6]
data_y[ligne, "t2.5"]    <- y[5]
data_y[ligne, "t3"]      <- y[4]
data_y[ligne, "t3.5"]    <- y[3]
data_y[ligne, "t4"]      <- y[2]
data_y[ligne, "t5"]      <- y[1]
data_y[ligne, "t6"]      <- happyear[i,"y"]
data_y[ligne, "t7"]      <- happyear[i,"y"]
data_y[ligne, "t8"]      <- happyear[i,"y"]


dataframe_x=rbind.data.frame(dataframe_x, data_x)
dataframe_y=rbind.data.frame(dataframe_y, data_y)

remove(x);remove(y);remove(data_x);remove(data_y)}

# Create new dataframe with coordinates trajectoreies of each points --- --- ---
dataframe_x<-pivot_longer(dataframe_x, cols = t0.5:t8, names_to = "time_point", values_to = "x")
dataframe_y<-pivot_longer(dataframe_y, cols = t0.5:t8, names_to = "time_point", values_to = "y")
data_xy<-cbind.data.frame(dataframe_x, dataframe_y[,"y"])
data_xy$time<-gsub("t", "",as.character(data_xy$time_point))
data_xy$time<-as.numeric(data_xy$time)
data_xy$letters<-as.factor(data_xy$letters)

# Add logo company --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
img <- readPNG("BioSEC_logo.png", native = TRUE)

# Plotting all points --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
ggplot(data_xy, aes(x=x, y=y, fill=letters, color=letters))+
  geom_point(shape=21, size=3.5, alpha=0.8)+   
  labs(x = NULL, y = NULL) +
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  annotation_raster(img, xmin = 0, xmax = 14, ymin = 32, ymax = 37)+
  theme_bw()+
  theme(legend.position='none',
        panel.border = element_rect(size=2, color = "#2b80A1"),
        panel.background = element_rect(fill="#ffffff"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Add animation --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
HappyR<-ggplot(data_xy, aes(x=x, y=y, fill=letters, color=letters))+
  geom_point(shape=21, size=3.5, alpha=0.8)+   
  labs(x = NULL, y = NULL) +
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  annotation_raster(img, xmin = 0, xmax = 14, ymin = 32, ymax = 37)+
  theme_bw()+
  theme(legend.position='none',
        panel.border = element_rect(size=2, color = "#2b80A1"),
        panel.background = element_rect(fill="#ffffff"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  transition_time(time) +
  ease_aes('linear')

animate(HappyR, renderer = gifski_renderer(), width=400, height=300)
anim_save('HappyR.gif')

dev.off()

