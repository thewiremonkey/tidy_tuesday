if(getwd() != "/Users/alyssa/TidyTuesday/2019_44_squirrels"){setwd("/Users/alyssa/TidyTuesday/2019_44_squirrels")}

library(tidyverse)
library(plotly)
library(corrplot)
library(randomForest)

raw<-read_csv("squirrels.csv") %>% 
  mutate(height = case_when(
    above_ground_sighter_measurement == "FALSE" ~"0",
    is.na(above_ground_sighter_measurement)~"0",
    TRUE ~ above_ground_sighter_measurement
  )) %>% 
  mutate(height = as.numeric(height))

df<-raw %>% select(unique_squirrel_id,lat, long,shift,age,primary_fur_color,location,above_ground_sighter_measurement,running:foraging,kuks:runs_from) %>% 
  filter(complete.cases(.),
         age !="?") %>% 
  mutate(height = as.numeric(case_when(
    location == "Ground Plane" ~'0',
    above_ground_sighter_measurement !=FALSE ~ above_ground_sighter_measurement,
    is.na(above_ground_sighter_measurement) ~'0'
  )),)%>% 
  select(-above_ground_sighter_measurement) %>% 
  mutate_if(., is.logical, as.numeric) %>% 
  mutate_if(., is.character, as.factor) %>% 
  mutate_if(., is.factor, as.numeric) %>% 
  rename(id=unique_squirrel_id)

ggplot(df, aes(x=long, y=lat, alpha=height))+
  geom_point()
