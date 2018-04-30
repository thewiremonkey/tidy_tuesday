library(tidyverse)
library(acs)

here::here()
url<-"https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/acs2015_county_data.csv"

dat<-read_csv(url) %>% 
  janitor::clean_names()

summary(dat)

nat_commute<-dat %>% 
  summarise(avg_commute=mean(mean_commute), median_commute=median(mean_commute))

commute<-dat %>% group_by(county, state) %>% 
  summarise(avg_commute=mean(mean_commute), median_commute=median(mean_commute)) %>% 
  mutate(avg_nat_commute=mean(dat$mean_commute), median_nat_commute=median(dat$mean_commute))


