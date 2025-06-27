##installing and loading usefull libraries
## cleaning data wuth "tidyverse" and data source is from "datasets" package

install.packages("tidyverse")
library(tidyverse)
install.packages("datasets")
library(datasets)
install.packages("GGally")
library(GGally)
library(dbplyr)
library(tidyr)

#in New York from May to September 1973
data("airquality")

view(airquality)

#previewing the data 
str(airquality)
summary(airquality)
head(airquality)

#checking null values
colSums(is.na(airquality))

#cleaning missing Null values
#"Due to the relatively small size of the dataset (153 rows),
#rows with missing values in key variables (Ozone and Solar.R) were retained.
#Missing values were imputed using the column mean to preserve data integrity for exploratory KPI analysis.

cleaned <- airquality %>%
  mutate(
      Ozone =ifelse(is.na(Ozone),mean(Ozone,na.rm=TRUE),Ozone),
      Solar.R =ifelse(is.na(Solar.R),mean(Solar.R,na.rm=TRUE),Solar.R),
  )

view(cleaned)

avg_ozone_pm <- cleaned %>%
  group_by(Month) %>%
  summarise(
    avg_ozone = mean(Ozone),
    high_risk_days = sum(Ozone > 100),
    avg_wind = mean(Wind),
  )

avg_ozone_pm_plot <- ggplot(avg_ozone_pm,aes(x=Month,y=avg_ozone,colour = high_risk_days))+
  geom_smooth(se=TRUE)+
  labs(title = "Avarage Ozone by Month",x="Month",y="Avg Ozone")+


hrd <- ggplot(avg_ozone_pm)+
  geom_smooth(mapping =aes(x=Month,y=high_risk_days))+
  labs(title = "high Risk Days by Month",x="Month",y="high Risk Days")+


  
GGally::ggpairs(cleaned[, c("Ozone", "Temp", "Wind", "Solar.R")])
cor(cleaned[c("Ozone","Temp","Wind")])
#Ozone and Wind: Negative correlation → Higher wind = lower ozone
#Ozone and Temp: Positive correlation → Higher temp = higher ozone

wind_ozone <- ggplot(cleaned,aes(x=Wind,y=Ozone))+
  geom_point(color = "blue",alpha =0.6)+
  geom_smooth(method = "lm",se=FALSE,color ="red")+
  labs(title = "Ozone vs Wind",x="Wind Speed (mph)",y="Ozone")
 

wind_ozone <- ggplot(cleaned,aes(x=Wind,y=Ozone,colour=factor(Ozone>100),shape=Ozone>100))+
  geom_point(size=3,alpha =0.6)+
  labs(title = "Ozone vs Wind",x="Wind Speed (mph)",y="Ozone",color = "Ozone > 100")


wind_temp <- ggplot(cleaned,aes(x=Temp,y=Wind,colour=factor(Ozone>100),shape=Ozone>100))+
  geom_point(size=3,alpha =0.6)+
  scale_colour_viridis_d()+
  labs(title = "Temprature vs Wind",x="Temperature (°F)",y="Wind Speed (mph)",color = "Ozone > 100")


#"How Ozone affects the Temperature"
Temp_ozone <- ggplot(cleaned,aes(x=Temp,y=Ozone))+
  geom_point(color = "orange",alpha =0.6)+
  geom_smooth(method = "lm",se=FALSE,color ="darkred")+
  labs(title = "Ozone vs Temperature",x="Temperature (°F)",y="Ozone")+
 


#“Does ozone go up when wind is low and temp is high?”
Temp_ozone_wind <- ggplot(cleaned,aes(x=Temp,y=Ozone,colour=Wind))+
  geom_point(size=2.5,alpha =0.7)+
  scale_color_viridis_c()+
  geom_smooth(method = "lm",se=FALSE,color ="black")+
  labs(title = "Ozone vs Temperature coloured by Wind Speed",x="Temperature (°F)",y="Ozone",color = "Wind Speed (mph)")







