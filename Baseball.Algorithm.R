library(ggplot2)
library(tidyverse)
library(ggridges)
library(readr)
library(tidyr)
library(dplyr)
library(vctrs)
library(viridisLite)
library(scales)
library(hrbrthemes)
library(stats)

#Load & View original Baseball Data
data_2018 <- read_csv("C:/Users/enory/OneDrive/Desktop/Astros Data/pitch_classification_data_2018.csv")
data_2019 <- read_csv("C:/Users/enory/OneDrive/Desktop/Astros Data/pitch_classification_data_2019.csv")
View(data_2018)
View(data_2019)

#See structure
str(data_2018)
str(data_2019)

#Get dimensions
dim(data_2018)
dim(data_2019)

#Filter out NA
baseball_2018 <- data_2018 %>%
  drop_na(az0)
baseball_2019 <- data_2019 %>%
  drop_na(az0)

str(baseball_2018)
dim(baseball_2018)
dim(baseball_2019)  

#Update the data type
baseball_2018$year <- as.character(baseball_2018$year) 
baseball_2018$pitcher_id <- as.factor(baseball_2018$pitcher_id)
baseball_2018$pitcher_throws <- as.factor(baseball_2018$pitcher_throws)
baseball_2018$pitch_type <- as.factor(baseball_2018$pitch_type)

baseball_2019$year <- as.character(baseball_2019$year) 
baseball_2019$pitcher_id <- as.factor(baseball_2019$pitcher_id)
baseball_2019$pitcher_throws <- as.factor(baseball_2019$pitcher_throws)

#Add variables with appropriate units
baseball_2018 <- baseball_2018 %>%
  #convert mph to ft/s
  mutate(release_speed_ft_s=release_speed*5280/3600,
  #convert rev/min to rev/sec
         spin_rate_rev_s=spin_rate/60,
  #convert inches to ft
        horzbreak_ft=horzbreak/12,
  #convert inches to ft
        vertbreak_ft=vertbreak/12)

baseball_2019 <- baseball_2019 %>%
  #convert mph to ft/s
  mutate(release_speed_ft_s=release_speed*5280/3600,
         #convert rev/min to rev/sec
         spin_rate_rev_s=spin_rate/60,
         #convert inches to ft
         horzbreak_ft=horzbreak/12,
         #convert inches to ft
         vertbreak_ft=vertbreak/12)

#Separate left-handed and right-handed data
#2018 Left & Right
baseball_2018_l <- baseball_2018 %>%
  filter(pitcher_throws == "L")
baseball_2018_r <- baseball_2018 %>%
  filter(pitcher_throws == "R")
#2019 Left & Right
baseball_2019_l <- baseball_2019 %>%
  filter(pitcher_throws == "L")
baseball_2019_r <- baseball_2019 %>%
  filter(pitcher_throws == "R")


dim(baseball_2018)
dim(baseball_2019)
dim(baseball_2018_l)
dim(baseball_2018_r)
dim(baseball_2019_l)
dim(baseball_2019_r)


summary(baseball_2018_l)

#Number of Pitchers
vec_unique_count(baseball_2018$pitcher_id)
vec_unique_count(baseball_2019$pitcher_id)
#left-handed pitchers
vec_unique_count(baseball_2018_l$pitcher_id)
vec_unique_count(baseball_2019_l$pitcher_id)
#right-handed pitchers
vec_unique_count(baseball_2018_r$pitcher_id)
vec_unique_count(baseball_2019_r$pitcher_id)

#Contingency tables
with(baseball_2018, table(pitcher_throws, pitch_type))


#Analyzing Overall 2018 data
#Release_x
ggplot(baseball_2018, aes(x=release_x, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=release_x, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=release_x, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Release_z
ggplot(baseball_2018, aes(x=release_z, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=release_z, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=release_z, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Release_Speed_ft_s
ggplot(baseball_2018, aes(x=release_speed_ft_s, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=release_speed_ft_s, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=release_speed_ft_s, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Horzbreak_ft
ggplot(baseball_2018, aes(x=horzbreak_ft, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=horzbreak_ft, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=horzbreak_ft, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Vertbreak_ft
ggplot(baseball_2018, aes(x=vertbreak_ft, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=vertbreak_ft, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=vertbreak_ft, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Extension
ggplot(baseball_2018, aes(x=extension, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=extension, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=extension, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Spin_Rate_rev_s
ggplot(baseball_2018, aes(x=spin_rate_rev_s, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=spin_rate_rev_s, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=spin_rate_rev_s, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.05)+
  scale_fill_viridis_d(option = "B")

#Spin_Direction
ggplot(baseball_2018, aes(x=spin_direction, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=spin_direction, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=spin_direction, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Vx0
ggplot(baseball_2018, aes(x=vx0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=vx0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=vx0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Vy0
ggplot(baseball_2018, aes(x=vy0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=vy0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=vy0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Vz0
ggplot(baseball_2018, aes(x=vz0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=vz0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=vz0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Ax0
ggplot(baseball_2018, aes(x=ax0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=ax0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=ax0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Ay0
ggplot(baseball_2018, aes(x=ay0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=ay0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=ay0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

#Az0
ggplot(baseball_2018, aes(x=az0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_l, aes(x=az0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")

ggplot(baseball_2018_r, aes(x=az0, y=pitch_type, fill=pitch_type))+
  geom_density_ridges(rel_min_height=0, alpha=0.5, bandwidth=0.15)+
  scale_fill_viridis_d(option = "B")


#Creating models
#Wrong has 7 output variables
full_mdl_ch <- glm(ch~release_x+release_z+release_speed_ft_s+horzbreak_ft+vertbreak_ft+extension+spin_rate_rev_s+spin_direction+vx0+vy0+vz0+ax0+ay0+az0,
                data = baseball_2018_l_throw, family=binomial)
summary(full_mdl_ch)
mdl_ch <- glm(ch~release_x+release_z+release_speed_ft_s+horzbreak_ft+vertbreak_ft+extension+spin_rate_rev_s+spin_direction+vy0+vz0+ax0+ay0+az0,
                   data = baseball_2018_l_throw, family="binomial")
summary(mdl_ch)

#Adding onto data set to make binomial model
baseball_2018_l_throw <- baseball_2018_l %>%
  mutate(ch=ifelse(pitch_type=="CH", 1, 0),
         cu=ifelse(pitch_type=="CU",1,0),
         fc=ifelse(pitch_type=="FC",1,0),
         ff=ifelse(pitch_type=="FF", 1,0),
         fs=ifelse(pitch_type=="FS",1,0),
         ft=ifelse(pitch_type=="FT",1,0),
         sl=ifelse(pitch_type=="SL",1,0))
View(baseball_2018_l_throw[4:29])

baseball_2018_descriptives <- baseball_2018 %>%
  group_by(pitcher_throws, pitch_type) %>%
  summarize(rx_mean=mean(release_x),
            rx_sd=sd(release_x),
            rz_mean=mean(release_z),
            rz_sd=sd(release_z),
            rs_mean=mean(release_speed_ft_s),
            rs_sd=sd(release_speed_ft_s),
            hb_mean=mean(horzbreak_ft),
            hb_sd=sd(horzbreak_ft),
            vb_mean=mean(vertbreak_ft),
            vb_sd=sd(vertbreak_ft),
            ex_mean=mean(extension),
            ex_sd=sd(extension),
            sr_mean=mean(spin_rate_rev_s),
            sr_sd=sd(spin_rate_rev_s),
            sd_mean=mean(spin_direction),
            sd_sd=sd(spin_direction),
            vx_mean=mean(vx0),
            vx_sd=sd(vx0),
            vy_mean=mean(vy0),
            vy_sd=sd(vy0),
            vz_mean=mean(vz0),
            vz_sd=sd(vz0),
            ax_mean=mean(ax0),
            ax_sd=sd(ax0),
            ay_mean=mean(ay0),
            ay_sd=sd(ay0),
            az_mean=mean(az0),
            az_sd=sd(az0))

View(baseball_2018_descriptives)

#descriptive per right or left handed
baseball_2018_descriptives_l <- baseball_2018_descriptives %>%
  filter(pitcher_throws=="L")
baseball_2018_descriptives_l[1,15:30]

baseball_2018_descriptives_r <- baseball_2018_descriptives %>%
  filter(pitcher_throws=="R")
baseball_2018_descriptives_r

baseball_2019_output_l <- baseball_2019_l %>%
  mutate(ch_score=(abs(release_speed_ft_s-122)/4.66)+(abs(horzbreak_ft-1.16)/0.342)+(abs(vertbreak_ft-0.634)/0.395)+
           (abs(spin_rate_rev_s-27.8)/5.89)+(abs(spin_direction-118)/17.9)+(abs(vy0+121)/4.63)+(abs(ax0-12.6)/3.39)+
           (abs(ay0-23.1)/2.5)+(abs(az0+25.5)/3.9),
         cu_score=(abs(release_speed_ft_s-112)/4.59)+(abs(horzbreak_ft+0.852)/0.423)+(abs(vertbreak_ft+1.01)/0.482)+
           (abs(spin_rate_rev_s-25.9)/8.38)+(abs(spin_direction-311)/45.2)+
           (abs(vy0+111)/4.57)+(abs(ax0+5.35)/3.17)+(abs(ay0-21.1)/2.3)+(abs(az0+39.4)/3.62),
         fc_score=(abs(release_speed_ft_s-128)/3.21)+(abs(horzbreak_ft+0.278)/0.274)+(abs(vertbreak_ft-.701)/.295)+
           (abs(spin_rate_rev_s-14.2)/5.23)+(abs(spin_direction-198)/24.7)+
           (abs(vy0+127)/3.17)+(abs(ax0+.93)/2.96)+(abs(ay0-23.9)/2.13)+(abs(az0+24.4)/3.24),
         ff_score=(abs(release_speed_ft_s-135)/3.75)+(abs(horzbreak_ft-0.642)/0.372)+(abs(vertbreak_ft-1.35)/.26)+
           (abs(spin_rate_rev_s-32.3)/5.34)+(abs(spin_direction-153)/14.5)+
           (abs(vy0+134)/3.69)+(abs(ax0-9.54)/4.52)+(abs(ay0-28.2)/2.74)+(abs(az0+15.4)/3.38),
         fs_score=(abs(release_speed_ft_s-123)/3.33)+(abs(horzbreak_ft-0.96)/.311)+(abs(vertbreak_ft-.341)/.393)+
           (abs(spin_rate_rev_s-22.7)/5.12)+(abs(spin_direction-108)/26.2)+
           (abs(vy0+122)/3.31)+(abs(ax0-10.8)/2.96)+(abs(ay0-23.1)/1.81)+(abs(az0+28)/3.85),
         ft_score=(abs(release_speed_ft_s-134)/3.65)+(abs(horzbreak_ft-1.22)/0.274)+(abs(vertbreak_ft-.774)/.419)+
           (abs(spin_rate_rev_s-32.7)/5.37)+(abs(spin_direction-121)/16.4)+
           (abs(vy0+132)/3.64)+(abs(ax0-15.8)/3.28)+(abs(ay0-27.7)/2.59)+(abs(az0+22.8)/4.97),
         sl_score=(abs(release_speed_ft_s-121)/4.9)+(abs(horzbreak_ft+.581)/.49)+(abs(vertbreak_ft+.0103)/.377)+
           (abs(spin_rate_rev_s-13.2)/8.07)+(abs(spin_direction-253)/57.7)+
           (abs(vy0+120)/4.87)+(abs(ax0+3.55)/4.18)+(abs(ay0-22.7)/2.46)+(abs(az0+31.5)/3.54)
  ) %>%
  mutate(levelone=ifelse(ch_score<cu_score,ch_score,cu_score),levelonewhich=ifelse(ch_score<cu_score, "CH","CU"),
         leveltwo=ifelse(levelone<fc_score,levelone, fc_score), leveltwowhich=ifelse(levelone<fc_score,levelonewhich,"FC"),
         levelthree=ifelse(leveltwo<ff_score, leveltwo, ff_score),levelthreewhich=ifelse(leveltwo<ff_score, leveltwowhich, "FF"),
         levelfour=ifelse(levelthree<fs_score, levelthree, fs_score), levelfourwhich=ifelse(levelthree<fs_score, levelthreewhich, "FS"),
         levelfive=ifelse(levelfour<ft_score,levelfour, ft_score), levelfivewhich=ifelse(levelfour<ft_score,levelfourwhich, "FT"),
         levelsix=ifelse(levelfive<sl_score,levelfive, sl_score), pitch_type=ifelse(levelfive<sl_score,levelfivewhich, "SL"))

baseball_2019_output_r <- baseball_2019_r %>%
  mutate(ch_score=(abs(release_speed_ft_s-125)/4.83)+(abs(horzbreak_ft+1.23)/0.327)+(abs(vertbreak_ft-.551)/.443)+
           (abs(spin_rate_rev_s-27)/6.42)+(abs(spin_direction-244)/21.5)+
           (abs(vy0+124)/4.8)+(abs(ax0+12.5)/3.62)+(abs(ay0-23.8)/2.61)+(abs(az0+26)/4.44),
         cu_score=(abs(release_speed_ft_s-117)/5.16)+(abs(horzbreak_ft-.721)/.412)+(abs(vertbreak_ft+.971)/.428)+
           (abs(spin_rate_rev_s-26.4)/7.81)+(abs(spin_direction-46.7)/42.9)+
           (abs(vy0+116)/5.15)+(abs(ax0-5.91)/3.37)+(abs(ay0-22.5)/2.52)+(abs(az0+39.8)/3.4),
         fc_score=(abs(release_speed_ft_s-131)/3.92)+(abs(horzbreak_ft-.143)/.29)+(abs(vertbreak_ft-.704)/.318)+
           (abs(spin_rate_rev_s-14.8)/5.95)+(abs(spin_direction-160)/25.2)+
           (abs(vy0+129)/3.89)+(abs(ax0-1.16)/3.2)+(abs(ay0-25.1)/2.43)+(abs(az0+23.9)/3.65),
         ff_score=(abs(release_speed_ft_s-137)/3.81)+(abs(horzbreak_ft+.689)/.356)+(abs(vertbreak_ft-1.38)/.247)+
           (abs(spin_rate_rev_s-32.5)/5.56)+(abs(spin_direction-204)/14)+
           (abs(vy0+136)/3.78)+(abs(ax0+8.71)/4.53)+(abs(ay0-29.2)/2.76)+(abs(az0+15)/3.27),
         fs_score=(abs(release_speed_ft_s-125)/3.82)+(abs(horzbreak_ft+.951)/.387)+(abs(vertbreak_ft-.332)/.396)+
           (abs(spin_rate_rev_s-20.2)/7.7)+(abs(spin_direction-247)/26.6)+
           (abs(vy0+124)/3.78)+(abs(ax0+9.46)/3.95)+(abs(ay0-23.7)/2.19)+(abs(az0+28.1)/4.11),
         ft_score=(abs(release_speed_ft_s-136)/4.18)+(abs(horzbreak_ft+1.34)/.245)+(abs(vertbreak_ft-.828)/.403)+
           (abs(spin_rate_rev_s-33.8)/5.09)+(abs(spin_direction-238)/15.8)+
           (abs(vy0+135)/4.16)+(abs(ax0+16.2)/3.15)+(abs(ay0-28.5)/2.71)+(abs(az0+21.7)/4.99),
         sl_score=(abs(release_speed_ft_s-124)/4.78)+(abs(horzbreak_ft+.46)/.432)+(abs(vertbreak_ft-.812)/.369)+
           (abs(spin_rate_rev_s-13.3)/7.45)+(abs(spin_direction-110)/48.1)+
           (abs(vy0+123)/4.75)+(abs(ax0-3.98)/3.9)+(abs(ay0-23.4)/2.44)+(abs(az0+30.7)/3.61)
  )%>%
  mutate(levelone=ifelse(ch_score<cu_score,ch_score,cu_score),levelonewhich=ifelse(ch_score<cu_score, "CH","CU"),
         leveltwo=ifelse(levelone<fc_score,levelone, fc_score), leveltwowhich=ifelse(levelone<fc_score,levelonewhich,"FC"),
         levelthree=ifelse(leveltwo<ff_score, leveltwo, ff_score),levelthreewhich=ifelse(leveltwo<ff_score, leveltwowhich, "FF"),
         levelfour=ifelse(levelthree<fs_score, levelthree, fs_score), levelfourwhich=ifelse(levelthree<fs_score, levelthreewhich, "FS"),
         levelfive=ifelse(levelfour<ft_score,levelfour, ft_score), levelfivewhich=ifelse(levelfour<ft_score,levelfourwhich, "FT"),
         levelsix=ifelse(levelfive<sl_score,levelfive, sl_score), pitch_type=ifelse(levelfive<sl_score,levelfivewhich, "SL"))

View(baseball_2019_output_r)

#Trying to predict data
prediction_data <- baseball_2019_l %>%
  mutate(is_ch=predict(mdl_ch, baseball_2019_l, type = "response"), ch_most_likely=round(is_ch))
View(prediction_data)


#One Var
summary(glm(ch~release_x,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~release_z,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~release_speed_ft_s,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~horzbreak_ft,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~vertbreak_ft,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~extension,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~spin_rate_rev_s,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~spin_direction,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~vx0,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~vy0,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~vz0,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~ax0,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~ay0,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~az0,data = baseball_2018_l_throw, family="binomial"))
#Two Var
summary(glm(ch~release_x,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~release_z+release_x,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~release_speed_ft_s+release_x,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~horzbreak_ft,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~vertbreak_ft,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~extension,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~spin_rate_rev_s,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~spin_direction,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~vx0,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~vy0,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~vz0,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~ax0,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~ay0,data = baseball_2018_l_throw, family="binomial"))
summary(glm(ch~az0,data = baseball_2018_l_throw, family="binomial"))

baseball_2019_output <- baseball_2019 %>%
  mutate(cu_speed_rate=ifelse(release_speed_ft_s<110,"CU","OTHER"))
View(baseball_2019_output)


