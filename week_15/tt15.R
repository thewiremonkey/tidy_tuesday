setwd("./week_15")

library(tidyverse)
library(readxl)
library(tidytext)

beers <- read_excel("beers.xlsx", sheet = "beers") %>% 
  rename_all(.funs = funs(paste0("beer_", .))) %>% 
  left_join(read_excel("beers.xlsx", sheet="breweries"),by=c("beer_brewery_id"="id"))

bitterness<-beers %>% filter(!is.na(beer_ibu))

ibu_text <-bitterness %>% 
  select(beer_abv, beer_ibu, beer_name) %>% 
  mutate(beer_name=gsub("[[:digit:]]", "", beer_name)) %>% 
  unnest_tokens("words", beer_name) %>% 
  filter(!words %in% c("beer", "a", "the","on","of","style")) %>% 
  group_by(words) %>% 
  summarise(freq=n(), min_ibu=min(beer_ibu), max_ibu=max(beer_ibu), min_abv=min(beer_abv), max_abv=max(beer_abv), avg_ibu=mean(beer_ibu), avg_abv=mean(beer_abv)) %>% 
  top_n(50,freq)

ggplot(ibu_text)+
  geom_point(aes(x=avg_ibu, y=avg_abv),show.legend = F)+
  labs(x="less bitter<----Average IBU--->more bitter/hoppier", y="Average Alcohol by Volume", title="A Guide for Choosing Beer by Its Name", caption="data: data.world
       viz: Alyssa Goldberg @WireMonkey
       part of the #TidyTuesday project by @thomas_mock")+
  ggrepel::geom_text_repel(aes(x=avg_ibu, y=avg_abv, label=words),show.legend = F)+
  theme_bw()+
  scale_y_continuous(labels = scales::percent)
