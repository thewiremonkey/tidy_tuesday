library(tidyverse)
library(ggplot2)

setwd("./week_14")

g<-gapminder::gapminder %>%
  select(country, continent) %>%
  mutate_if(is.factor, as.character) %>%
  distinct()

c_raw<-read.csv("contraception.csv", skip = 4, stringsAsFactors = FALSE ) %>%
  select(-X) %>%
  mutate_if(is.logical, as.numeric) %>%
  mutate_if(is.integer, as.numeric)


c<-c_raw %>%
  gather(key=year, value=contraceptive_prevalence, 5:62) %>%
  mutate(year=gsub("X", "", year)) %>%
  select(-Indicator.Name, -Indicator.Code)

f_raw<-read.csv("female_life_expectancy.csv", skip = 4, stringsAsFactors = FALSE) %>%
  select(-X)

f<-f_raw %>%
  gather(key=year, value=life_expectancy, 5:62) %>%
  mutate(year=gsub("X","", year)) %>%
  select(-Indicator.Name, -Indicator.Code)

df<-merge(c,f) %>%
  mutate(year=as.numeric(year)) %>%
  left_join(g, by=c("Country.Name" = "country")) %>%
  filter(!is.na(continent),!is.na(contraceptive_prevalence)) %>%
  mutate(semi_decade=floor(year/5)*5) %>%
  group_by(continent,Country.Name,semi_decade) %>%
  summarise(contraceptive=mean(contraceptive_prevalence, na.rm=T),
            life=mean(life_expectancy, na.rm=T)) %>%
  filter(!is.nan(contraceptive), continent !="Oceania") %>%
  ungroup()

ggplot(subset(df, semi_decade>=1995 & semi_decade<=2010), aes(x=contraceptive, y=life))+
  geom_point(show.legend = F)+
  geom_smooth(aes(color=continent),method='lm', se=FALSE)+
  labs(x="contraceptive prevalance, percent",
       y="female life expectancy, years",
       title="Higher Female Life Expectancy Correlated\nWith Higher Contraceptive Prevalence",
       caption="data: https://ourworldindata.org/maternal-mortality
https://data.worldbank.org/indicator/SP.DYN.CONU.ZS?end=2017&start=1995
       viz: Alyssa Goldberg @WireMonkey
       part of the #TidyTuesday challenge by @Thomas_Mock")+
  facet_wrap(~ semi_decade)



