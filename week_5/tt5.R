library(tidyverse)
library(acs)
library(corrplot)
library(ggrepel)
library(noncensus)
library(magrittr)

here::here()
url<-"https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/acs2015_county_data.csv"

dat<-read_csv(url) %>%
  janitor::clean_names()

summary(dat)

dat <- dat[complete.cases(dat), ]

data("states")
states %<>%mutate_at(c("area", "population"), as.numeric) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(pop_dens=population/area,
         division=case_when(
    state %in% c("AS", "GU", "MP") ~ "Pacific Territory",
    state %in% c("PR", "VI") ~ "Caribbean Territory",
    TRUE ~ region
  ))

data("counties")
counties%<>%mutate_if(is.factor, as.character)

all<-left_join(states, counties, by="state") %>%
  select(-CSA, -CBSA) %>%
  mutate(s_pop=population.x, c_pop=population.y, county=gsub(" County| Parish| Municipio|  Municipality, County", "", county_name)) %>%
  left_join(dat, by=c("name"="state", "county")) %>%
  select(-population.x, -population.y)

division<-all %>% group_by(division) %>%
  summarise(n_counties=n(),dens=mean(pop_dens, na.rm=T))

p<-ggplot(region, aes(label=region))+
  geom_text(aes(x=pop_dens, y=income_per_cap))

# ##---- old stuff---
#
# dat2<-dat %>%
#   select_if(.,is.numeric) %>%
#   colMeans() %>%
#   as.tibble() %>%
#   mutate(cols= row.names(.)) %>%
#   filter(value <=100)
#
# dat3<-dat %>% select(state, county, dat2$cols)
#
#
# corrplot(cor(dat[,4:37]),"shade",type = "lower", is.corr = FALSE)
#
# ##using https://chasemc.github.io/post/tidy-tuesday-4-17-2018/ to learn how to do PCA
# ##
# dat4<-dat %>% mutate(id=paste0(state,county,sep=""), men=(men/total_pop)*100, women=(women/total_pop)*100) %>% sample_n(50)
#
#
#
# a2 <- dat4 %>% group_by(id) %>%
#            summarize_if(is.numeric,mean)
#
#
# a <- a2 %>% select(4:36) %>% replace(is.na(.), 0)
#
#
#
# principleComponents <- prcomp(a) %>%
#   .$x %>%
#   as_tibble %>%
#   bind_cols(name=a2$id, .)
#
# ggplot(principleComponents) +
#   geom_label(aes(x=PC1,y=PC2,label=name)) +
#   ggtitle("First and Second Principle Components")
#
# principleComponents2 <- prcomp(a) %>%
#   .$rotation %>%
#   as_tibble %>%
#   bind_cols(name=colnames(a), .)
#
# ggplot(principleComponents2) +
#   geom_label(aes(x=PC1,y=PC2
#                  ,label=name)) +
#   ggtitle("First and Second Principle Components, Variable Contributions")
#
# unoriginal<-dat %>% group_by(county) %>%
#   count() %>%
#   filter(n>1)
#
# original<-dat %>% group_by(county) %>%
#   count() %>%
#   filter(n==1)
#
# sum(unoriginal$n)
#
# un<-dat %>%
#   group_by(state) %>%
#   filter(county %in% unoriginal$county) %>%
#   summarise(u=n()) %>%
#   left_join(dat %>%
#               group_by(state) %>%
#               filter(!county %in% unoriginal$county) %>%
#               summarise(o=n())
#             )%>%
#   mutate(perc=u/(o+u), t=o+u)
#
#
#
# tot<-dat %>%
#   split(.$state) %>%
#   map(~nrow(.))
# ##----New Stuff
