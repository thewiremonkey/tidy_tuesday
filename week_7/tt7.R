library(fivethirtyeight)
library(tidyverse)

#get data
URL<-"https://raw.githubusercontent.com/fivethirtyeight/data/master/star-wars-survey/StarWars.csv"
raw_data<-read.csv(URL, stringsAsFactors = FALSE,header = TRUE) %>% 
  clean_names()

names(raw_data)<-c("id", "have_seen", "fan", unlist(raw_data[1, 4:9]), paste0("rank ", unlist(raw_data[1, 10:15])), unlist(raw_data[1,16:29]), "shot", "expanded", "fan_expanded", "trek", "gender", "age", "income", "ed", "location")

sw_names<-function(df, cols){names(df) <-  c("id", unlist(df[1,cols]))}


df_fan<-raw_data %>% select(id, have_seen, fan)

df_which<-raw_data %>% select(id, 4:9 ) %>% 
  gather(key=ep, value=seen, 2:7) %>% 
  mutate(seen=ifelse(seen == "", 0,1)) %>% 
  filter(!is.na(id))

df_rank<-raw_data %>% select(id, 10:15) %>% 
  gather(key=ep, value=rank, 2:7) %>% 
  mutate(ep=gsub("rank ", "", ep)) %>% 
  filter(rank !="" ) %>% 
  filter(!is.na(id))

df_chars<-raw_data %>% select(id, 16:29) %>%
  filter(!is.na(id)) %>% 
  gather(key=character, value=score, 2:15) %>% 
  filter(score !="") %>% 
  mutate(score=case_when(
    score=="Very favorably" ~2,
    score=="Somewhat favorabley" ~1,
    score=="Neither favorably nor unfavorably (neutral)" ~0,
    score=="Somewhat unfavorably" ~ -1,
    score=="Very unfavorably" ~ -2,
    TRUE ~0
  ))

df_demo<-raw_data %>% select(id, 30:38) %>% 
  filter(!is.na(id))

df_which_rank<-merge(df_which, df_rank) %>% 
  mutate(id=as.character(id), rank=as.numeric(rank))


ggplot(df_which_rank, aes(x=rank, y=ep))+
  ggridges::geom_density_ridges(aes(fill=as.factor(seen)), alpha=0.5)
