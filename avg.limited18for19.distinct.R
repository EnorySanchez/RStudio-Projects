library(tidyverse)
library(dplyr)
library(ggplot2)
View(limited18for19)
names(limited18for19)
ggplot(limited18for19,aes(r_l_type,release_x,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,release_z,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,release_speed,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,horzbreak,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,vertbreak,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,spin_rate,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,spin_direction,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,vx0,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,vy0,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,vz0,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,ax0,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,ay0,fill=pitcher_throws))+
  geom_boxplot()
ggplot(limited18for19,aes(r_l_type,az0,fill=pitcher_throws))+
  geom_boxplot()
limited18for19.lh<-limited18for19%>%
  filter(pitcher_throws=="L")%>%
  select(pitcher_id,pitch_type,release_x,release_z,release_speed,horzbreak,vertbreak,extension,spin_rate,spin_direction,vx0,vy0,vz0,ax0,ay0,az0)
View(limited18for19.lh)
ggplot(limited18for19.lh,aes(release_x,horzbreak,col=pitch_type))+
  geom_point()
ggplot(limited18for19.lh,aes(spin_direction,spin_rate,col=pitch_type))+
  geom_point()
limited18for19%>%
  arrange(pitcher_id,pitch_type)
View(limited18for19)
id_type_2018<-limited18for19%>%
  group_by(pitcher_id,pitch_type)
by_id_2018<-id_type_2018%>%
  summarise(n=n())
with_pitcher_throws_by_id_2018<-limited18for19%>%
  mutate(unique_pitch_thrown=paste(pitcher_id,pitch_type))
View(with_pitcher_throws_by_id_2018)
with_idtype_by_id_2018<-by_id_2018%>%
  mutate(unique_pitch_thrown=paste(pitcher_id,pitch_type))
View(limited18for19)
ordered_with_pitcher_throws_by_id_2018<-with_pitcher_throws_by_id_2018%>%
  arrange(unique_pitch_thrown)
View(ordered_with_pitcher_throws_by_id_2018)
names(ordered_with_pitcher_throws_by_id_2018)
avg.release_x_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.release_x=mean(release_x))
View(avg.release_x_per_pitcher_by_type_2018)
avg.release_z_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.release_z=mean(release_z))
View(avg.release_z_per_pitcher_by_type_2018)
avg.release_speed_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.release_speed=mean(release_speed))
avg.horzbreak_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.horzbreak=mean(horzbreak))
avg.vertbreak_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.vertbreak=mean(vertbreak))
avg.extension_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.extension=mean(extension))
avg.spin_rate_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.spin_rate=mean(spin_rate))
avg.spin_direction_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.spin_direction=mean(spin_direction))
avg.vx0_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.vx0=mean(vx0))
avg.vy0_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.vy0=mean(vy0))
avg.vz0_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.vz0=mean(vz0))
avg.ax0_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.ax0=mean(ax0))
avg.ay0_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.ay0=mean(ay0))
avg.az0_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.az0=mean(az0))
View(avg.ax0_per_pitcher_by_type_2018)
s.avg.release_x_limited18for19<-avg.release_x_per_pitcher_by_type_2018%>%
  select(year,pitcher_id,pitcher_throws,pitch_type,unique_pitch_thrown,m.release_x)
View(s.avg.release_x_limited18for19)
s.avg.release_z_limited18for19<-avg.release_z_per_pitcher_by_type_2018%>%
  select(m.release_z)
View(s.avg.release_z_limited18for19)

avg.units_per_pitcher_by_type_2018<-ordered_with_pitcher_throws_by_id_2018%>%
  group_by(unique_pitch_thrown)%>%
  mutate(m.release_x=mean(release_x),m.release_z=mean(release_z),m.release_speed=mean(release_speed),
         m.horzbreak=mean(horzbreak),m.vertbreak=mean(vertbreak),m.extension=mean(extension),
         m.spin_rate=mean(spin_rate),m.spin_direction=mean(spin_direction),
         m.vx0=mean(vx0),m.vy0=mean(vy0),m.vz0=mean(vz0),
         m.ax0=mean(ax0),m.ay0=mean(ay0),m.az0=mean(az0))
View(avg.units_per_pitcher_by_type_2018)
s.avg_units_limited18for19<-avg.units_per_pitcher_by_type_2018%>%
  select(year,pitcher_id,pitcher_throws,m.release_x,m.release_z,m.release_speed,m.horzbreak,m.vertbreak,m.extension,m.spin_rate,m.spin_direction,m.vx0,m.vy0,m.vz0,m.ax0,m.ay0,m.az0,pitch_type,r_l_type,unique_pitch_thrown)
View(s.avg_units_limited18for19)

#Creation of Final Table for Drawing Conclusions
f.avg18for19<-s.avg_units_limited18for19%>%distinct()
View(f.avg18for19)

r.f.avg18for19<-f.avg18for19%>%
  filter(pitcher_throws=="R")

l.f.avg18for19<-f.avg18for19%>%
  filter(pitcher_throws=="L")
names(l.f.avg18for19)

ggplot(l.f.avg18for19,aes(pitch_type,m.release_x))+
  geom_boxplot()

ggplot(l.f.avg18for19,aes(pitch_type,m.release_z))+
  geom_boxplot()

ggplot(l.f.avg18for19,aes(pitch_type,m.release_speed))+
  geom_boxplot()

ggplot(f.avg18for19,aes(pitch_type,m.release_x,col=pitcher_throws))+
  geom_boxplot()

ggplot(f.avg18for19,aes(pitch_type,m.horzbreak,col=pitcher_throws))+
  geom_boxplot()

ggplot(f.avg18for19,aes(pitch_type,m.vertbreak,col=pitcher_throws))+
  geom_boxplot()

ggplot(f.avg18for19,aes(pitch_type,m.extension,col=pitcher_throws))+
  geom_boxplot()

ggplot(f.avg18for19,aes(pitch_type,m.spin_rate,col=pitcher_throws))+
  geom_boxplot()

ggplot(f.avg18for19,aes(pitch_type,m.spin_direction,col=pitcher_throws))+
  geom_boxplot()

ggplot(f.avg18for19,aes(pitch_type,m.az0,col=pitcher_throws))+
  geom_boxplot()

write.csv(f.avg18for19,file="f.avg18for19.csv")

ggplot(l.f.avg18for19,aes(m.vz0,m.az0,col=pitch_type))+
  geom_point()

ggplot(f.avg18for19,aes(pitch_type,m.spin_rate,col=pitcher_throws))+
  geom_boxplot()

ggplot(f.avg18for19,aes(pitch_type,m.az0,col=pitcher_throws))+
  geom_boxplot()

ggplot(f.avg18for19,aes(pitch_type,m.release_speed,col=pitcher_throws))+
  geom_boxplot()
