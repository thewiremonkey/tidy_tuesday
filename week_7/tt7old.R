library(fivethirtyeight)
library(tidyverse)
library(ggrepel)


#get data
URL<-"https://raw.githubusercontent.com/fivethirtyeight/data/master/star-wars-survey/StarWars.csv"
raw_data<-read.csv(URL, stringsAsFactors = FALSE,header = TRUE) %>% 
  clean_names() %>% 
  mutate(respondent_id=as.character(respondent_id))

#get the names  of raw_data

#brute force renaming of columns
names(raw_data)<-c("id", "have_seen", "fan", unlist(raw_data[1, 4:9]), paste0("rank ", unlist(raw_data[1, 10:15])), unlist(raw_data[1,16:29]), "shot", "expanded", "fan_expanded", "trek", "gender", "age", "income", "ed", "location")

#remove the first line that used to held the selection items
raw_data<-filter(raw_data, !is.na(id))

#pull to see who is a fan and who isn't
df_fan<-raw_data %>% select(id, ep,have_seen, fan)

df_which<-raw_data %>% select(id, 4:9 ) %>% 
  gather(key=ep, value=seen, 2:7) %>% 
  mutate(seen=ifelse(seen == "", 0,1)) 

df_rank<-raw_data %>% select(id, 10:15) %>% 
  gather(key=ep, value=rank, 2:7) %>% 
  mutate(ep=gsub("rank ", "", ep)) %>% 
  filter(rank !="" )

df_chars<-raw_data %>% select(id, 16:29) %>%
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

df_demo<-raw_data %>% select(id, 30:38)

df_which_rank<-merge(df_which, df_rank) %>% 
  mutate(id=as.character(id), rank=as.numeric(rank))


ggplot(df_which_rank, aes(x=rank, y=ep))+
  ggridges::geom_density_ridges(aes(fill=as.factor(seen)), alpha=0.5)+
  scale_fill_manual(name="seen", values=c("red", "green"),labels=c("no", "yes"))

count_seen<-df_which_rank %>% 
  group_by(id) %>% 
  summarise(seen=sum(seen))

demo_seen<-left_join(count_seen, df_demo %>% mutate(id=as.character(id))) %>% 
  filter_all(all_vars(. !=""))

mf<-df_demo %>% select(id, gender) %>% 
  left_join(df_which_rank) %>% 
  filter(!is.na(ep)) %>% 
  group_by(ep, gender) %>% summarise(cnt=sum(seen == 1), avg_rank=mean(rank)) %>% 
  mutate(gender=ifelse(gender=="", "refused", gender))

notseen<-df_which_rank %>% filter(seen==0) %>% 
  left_join(df_demo %>% select(id, gender)) %>% 
  mutate(ep=gsub("Star Wars: ", "", ep),gender=ifelse(gender=="", "Refused", gender)) %>% 
  left_join(df_fan %>% select(id, fan))

ggplot(notseen %>% filter(fan=="No"), aes(x=gender, y=rank))+
  geom_jitter(aes(color=gender), alpha=.4,width = .1)+
  scale_color_manual(values=c('Refused'="DarkOliveGreen", "Female"="Purple", "Male"="Goldenrod"))+
  labs(title="Trollpinions", subtitle='Those who have not seen the films and answered "No" to whether they are fans\nappear to not understand the naming/numbering convention for the franchise')+
  coord_flip()+
  facet_wrap(~ep)+
  theme_bw()

seen<-df_which_rank %>% filter(seen==1) %>% 
  left_join(df_demo %>% select(id, gender)) %>% 
  mutate(ep=gsub("Star Wars: ", "", ep),gender=ifelse(gender=="", "Refused", gender)) %>% 
  left_join(df_fan %>% select(id, fan))

ggplot(notseen %>% filter(fan=="Yes"), aes(x=gender, y=rank))+
  geom_jitter(aes(color=gender), alpha=.4,width = .1, height=.1)+
  scale_color_manual(values=c('Refused'="DarkOliveGreen", "Female"="Purple", "Male"="Goldenrod"))+
  labs(title="Trollpinions", subtitle='Those who have not seen the films and answered "No" to whether they are fans\nappear to still be able to rank films in  the franchise')+
  coord_flip()+
  facet_wrap(~ep)+
  theme_bw()
