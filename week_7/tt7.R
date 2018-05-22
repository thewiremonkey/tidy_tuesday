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

#pull and make long the responses to which films seen
df_which<-raw_data %>% select(id, 4:9 ) %>% 
  gather(key=ep, value=seen, 2:7) %>% 
  mutate(seen=ifelse(seen == "", 0,1)) 

#pull and make long the responses to ranks of films
df_rank<-raw_data %>% select(id, 10:15) %>% 
  gather(key=ep, value=rank, 2:7) %>% 
  mutate(ep=gsub("rank ", "", ep)) %>% 
  filter(rank !="" )

#pull and make long the responses to characters
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

#general demographics
df_demo<-raw_data %>% select(id, 30:38)

#find which movies respondants have seen and how they ranked them
df_which_rank<-merge(df_which, df_rank) %>% #joins on both the id and the episode name
  mutate(id=as.character(id), rank=as.numeric(rank))


ggplot(df_which_rank, aes(x=rank, y=ep))+
  ggridges::geom_density_ridges(aes(fill=as.factor(seen)), alpha=0.5)+
  scale_fill_manual(name="seen", values=c("red", "green"),labels=c("no", "yes"))

#find out who has not seen the films, join with the gender variable from the demo
#data frame to explore whether there are any differences in rankings by the various
#genders
notseen<-df_which_rank %>% filter(seen==0) %>% 
  left_join(df_demo %>% select(id, gender)) %>% 
  mutate(ep=gsub("Star Wars: ", "", ep),gender=ifelse(gender=="", "Refused", gender)) %>% 
  left_join(df_fan %>% select(id, fan))

#plot the rankings of those who have not seen the films, are not fans
# but have ranked them anyway
ggplot(notseen %>% filter(fan=="No"), aes(x=gender, y=rank))+
  geom_jitter(aes(color=gender), alpha=.4,width = .1)+
  scale_color_manual(values=c('Refused'="DarkOliveGreen", "Female"="Purple", "Male"="Goldenrod"))+
  labs(title="Trollpinions", subtitle='Those who have not seen the films and answered "No" to whether they are fans\nappear to have thoughts re: ranking')+
  coord_flip()+
  facet_wrap(~ep)+
  theme_bw()

#find out who has seen the films, join with the gender variable from the demo
#data frame and explore whether there are any differences in rankings by the 
#various genders
seen<-df_which_rank %>% filter(seen==1) %>% 
  left_join(df_demo %>% select(id, gender)) %>% 
  mutate(ep=gsub("Star Wars: ", "", ep),gender=ifelse(gender=="", "Refused", gender)) %>% 
  left_join(df_fan %>% select(id, fan))

#plot the fan rankings
ggplot(seen %>% filter(fan=="Yes"), aes(x=gender, y=rank))+
  geom_jitter(aes(color=gender), alpha=.4,width = .1, height=.3)+
  scale_color_manual(values=c('Refused'="DarkOliveGreen", "Female"="Purple", "Male"="Goldenrod"))+
  labs(title="Fanpinions", subtitle='Those who have seen the films and answered "Yes" to whether they are fans\ntend to agree that both Attack of the Clones and Revenge of the Sith sucked,\nbut with slightly more agreement on Attack of the Clones.')+
  coord_flip()+
  facet_wrap(~ep)+
  theme_bw()

#explore rankings of films by age demo
age_pref<-df_which_rank %>% 
  left_join(df_demo %>% select(id, age)) %>% 
  filter(age !="")%>% group_by(age, ep) %>% 
  summarise(rank=mean(rank)) %>% 
  arrange(age, rank)


age_chars<-df_chars %>% 
  left_join(df_demo %>% select(id, age)) %>% 
  group_by(age, character) %>% 
  summarise(score=mean(score)) %>% 
  filter(age !="") %>% 
  ungroup() %>% 
  mutate(age=factor(x = age,levels = c("18-29","30-44","45-60","> 60")))



ggplot(age_chars, aes(x=fct_reorder(character, score, fun=mean, .desc=F), y=score)) +
  geom_col(stat="identity", position="dodge")+
  coord_flip()+
  facet_wrap(~age, scales="free_y")+
  theme_bw()
