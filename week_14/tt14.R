library(tidyverse)
library(zoo)

setwd("./week_14")
c_raw<-read.csv("contraception.csv", skip = 4, stringsAsFactors = FALSE ) %>%
  select(-X) %>%
  mutate_if(is.logical, as.numeric) %>%
  mutate_if(is.integer, as.numeric)


c<-c_raw %>%
  gather(key=year, value=contraceptive_prevalence, 5:62) %>%
  mutate(year=gsub("X", "", year)) %>%
  select(-Indicator.Name, -Indicator.Code) %>%


c_empty<-c %>% group_by(year) %>%
  summarise(complete=sum(!is.na(contraceptive_prevalence)))


f_raw<-read.csv("female_life_expectancy.csv", skip = 4, stringsAsFactors = FALSE) %>%
  select(-X)

f<-f_raw %>%
  gather(key=year, value=life_expectancy, 5:62) %>%
  mutate(year=gsub("X","", year)) %>%
  select(-Indicator.Name, -Indicator.Code)

df<-merge(c,f) %>%
  mutate(year=as.numeric(year))


p<-ggplot(subset(df, as.numeric(year)>1970), aes(x=contraceptive_prevalence, y=life_expectancy))+
  geom_point(aes(color=Country.Code), show.legend = FALSE)+
  facet_wrap(~year)
