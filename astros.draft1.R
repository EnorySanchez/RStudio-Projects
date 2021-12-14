library(tidyverse)
library(ggplot2)
library(dplyr)
View(X2018_Pitch_Data)
str(X2018_Pitch_Data)
astros2018data<-X2018_Pitch_Data %>%
  select(pitcher_id,pitch_type,pitcher_throws,spin_direction,spin_rate,
         release_x,release_z,horzbreak,vertbreak,
         release_speed,vx0,vy0,vz0,ax0,ay0,az0)
astros2018data.lh<-astros2018data%>%
  filter(pitcher_throws=="L")%>%
  arrange(pitch_type)
astros2018data.rh<-astros2018data%>%
  filter(pitcher_throws=="R")%>%
  arrange(pitch_type)
View(astros2018data.lh)
ggplot(astros2018data.lh,aes(x=pitch_type,y=spin_direction))+
  geom_boxplot()
ggplot(astros2018data.rh,aes(x=pitch_type,y=spin_direction))+
  geom_boxplot(fill="pink")
ggplot(astros2018data.lh,aes(x=pitch_type,y=release_speed))+
  geom_boxplot()
ggplot(astros2018data.rh,aes(x=pitch_type,y=release_speed))+
  geom_boxplot(fill="pink")
ggplot(astros2018data.lh,aes(x=pitch_type,y=horzbreak))+
  geom_boxplot()
ggplot(astros2018data.rh,aes(x=pitch_type,y=horzbreak))+
  geom_boxplot(fill="pink")

View(r_l_pitch_classification_data_2018)
rl18data<-r_l_pitch_classification_data_2018 %>%
  arrange(r_l_type,release_speed)
View(rl18data)
str(rl18data)
ggplot(rl18data,aes(r_l_type,release_x,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,release_z,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,release_speed,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,horzbreak,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,vertbreak,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,extension,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,spin_rate,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,spin_direction,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,vx0,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,vy0,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,vz0,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,ax0,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,ay0,fill=pitch_type))+
  geom_boxplot()
ggplot(rl18data,aes(r_l_type,az0,fill=pitch_type))+
  geom_boxplot()

rl18data.rhch<-r_l_pitch_classification_data_2018 %>%
  filter(pitcher_throws=="R") %>%
  filter(pitch_type=="CH")
  View(rl18data.rhch)

View(rl18data)
ggplot(rl18data,aes(spin_direction,spin_rate,fill=pitch_type))+
  geom_point()
