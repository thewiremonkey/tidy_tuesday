library(tidyverse)
library(readxl)
library(acs)


setwd("./week_18")

df<-df <- read_excel("week18_dallas_animals.xlsx",sheet = "simple", col_types = c("text", "text", "text", "text", "numeric",
                                                           "text", "text", "text", "date", "text",
                                                           "date", "text", "text"))


df<-df %>% filter(!is.na(animal_type)) %>% select(-animal_id) %>%
  mutate(census_tract=str_pad(string = census_tract, width = 6, side = "left", pad = "0"))

dallas<-geo.make(state="TX", county="Dallas", tract="*")
income<-acs.fetch(endyear=2015,geography = dallas, table.number="B19013")
d_tracts<-income@geography$tract
d_est<-as.data.frame(income@estimate, row.names=F)
d_income<-bind_cols(tracts=d_tracts, est=d_est)

df<-df %>% left_join(d_income, by=c("census_tract"="tracts"))

by_income<-df %>% group_by(B19013_001, animal_type, intake_type) %>%
  filter(!is.na(B19013_001),animal_type %in% c("DOG", "CAT")) %>%
  count()


ggplot(by_income %>% filter(intake_type=="STRAY"))+
  aes(x=B19013_001/1000, y=n)+
  geom_point(aes(color=animal_type))+
  facet_wrap(~animal_type)+
  geom_vline(xintercept = median(d_income$B19013_001/1000, na.rm=T), lty="dashed", color="red")+
  labs(x="census tract median income", y="count", title="70% of Stray Dogs and Cats are Picked Up in the\nPoorest 50% of Neighborhoods", caption="data: Dallas Open Data & ACS Table B19013\nviz: @WireMonkey - Alyssa Goldberg\npackages: tidyverse, acs, ggthemes ")+
  # ggthemes::theme_economist()+
  NULL

time_in<-df %>%
  mutate(span=(outcome_date-intake_date)) %>%
  filter(!is.na(span), span !=0)

outcome<-df %>% group_by(intake_type,outcome_type) %>%
  count()
most<-outcome %>% ungroup() %>%
  group_by(animal_breed) %>%
  filter(n==max(n))

acs<-time_in %>% group_by(animal_type, census_tract) %>%
  summarise(num=n(),med=median(span/(60*60*24))) %>%
  filter(num>1)

intake<-time_in %>% group_by(intake_type) %>%
  summarise(num=n(), med=median(span/(60*60*24)))


ggplot(time_in)+
  ggridges::geom_density_ridges(aes(x=span/(60*60*24), y=animal_type, fill=chip_status), alpha=0.3)


library(ggalluvial)
library(ggrepel)
library(paletteer)

ggplot(as.data.frame(outcome),
       aes(y = n, axis1 = intake_type, axis2 = outcome_type)) +
  geom_alluvium( aes(fill=intake_type),width = 1/5, show.legend = FALSE) +
  geom_stratum(width = 1/5, fill = "white", color = "darkgrey") +
  geom_text_repel(stat = "stratum", label.strata = TRUE, size=3) +
  # scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  ggtitle("Outcome by Intake type")+
  theme_void()

api.key.install("03bc1cdd083b1aed79c0062575b8e7eecb9003bb", file="acdkey.rda")



breeds<-df %>% ungroup() %>%
  filter(animal_type %in% c("DOG", "CAT","BIRD")) %>%
  group_by(animal_breed) %>%
  filter(!is.na(B19013_001)) %>%
  summarize(num=n(),inc=mean(B19013_001)) %>%
  arrange(desc(inc))


time_income<-left_join(time_in, d_income, by=c("census_tract"="tracts"))

ggplot(by_income)+
  aes(x=B19013_001/1000, y=n)+
  geom_point(aes(color=animal_type), alpha=0.25)+
  geom_vline(xintercept = median(by_income$B19013_001/1000))+
  geom_hline(yintercept = median(by_income$n))+
  facet_wrap(~intake_type)+
  labs(x="census tract median income", y="count")

library(rpart)
part_df<-df %>% select(animal_type, animal_breed, census_tract, intake_type, outcome_type) %>% filter(!is.na(animal_breed), !is.na(census_tract), !is.na(animal_type), !is.na(intake_type),!is.na(outcome_type))

prt<-rpart(formula = outcome_type~., data = part_df, method = 'class', )


