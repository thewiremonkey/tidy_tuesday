library(tidyverse)
library(readxl)
library(rvest)
library(httr)
library(fredr)
library(gganimate)
library(tweenr)

if(!grepl("2019_7", getwd())){setwd("./2019_7")}
# Sys.setenv(FRED_API_KEY="a54d41bb49c73d7c407b37b29d730a80")
#
yeardf<-data.frame(year=seq_along(2000:2017)+1999)
lgadmin<-function(x)(ifelse(is.na(x),lag(x,1),x))
#get table of party control
admin<-read_csv("admins.csv") %>%
  filter(xstart %in% gdp$year) %>%
  mutate(year=xstart) %>%
  right_join(yeardf)%>%
  mutate(hse=lgadmin(hse),
         sen=lgadmin(sen),
         pres=lgadmin(pres)) %>%
  select(-xstart, -xend)

sr<-fredr_series_search_text(search_text   = "research development")

f_get<-function(x){
  fredr(series_id = x,
        observation_start = as.Date("2000-01-01"),
        observation_end=Sys.Date()
        ) %>%
    mutate(year=lubridate::year(date))
}
#Total Gross Domestic Product
gdp<-f_get("GDPCA")

#Gross Domestic Product: Research and Development
rs_gdp<-f_get("Y694RC1A027NBEA")

#Total Federal Spending
spend<-f_get("W019RC1A027NBEA")

#Total Spending on R&D per AAAS
rd_tot<-read_csv("./cleaned/def_non.csv") %>%
  mutate(year=as.numeric(year)) %>%
  filter(year %in% gdp$year)

#Total Spending on climate Science per AAAS
##millions, divide by 1000 to get billions
rd_cc<-read_csv("USGCRP.csv") %>%
  gather(key=year, value=value, 2:19) %>%
  mutate(year=as.numeric(year), value=value/1000) %>%
  filter(year %in% gdp$year)

rd_cc_change<-rd_cc %>%
  mutate(lg=0) %>%
  split(.$Agency) %>%
  map_df(., ~(.$value-lag(.$value, 1))*1000) %>%
  mutate(year=seq_along(2000:2017)+1999) %>%
  gather(key=agency, value=value, 1:7) %>%
  mutate(value=ifelse(is.na(value),0,value)) %>%
  left_join(admin) %>%
  filter(!is.na(pres)) %>%
  gather(key=pol, value=party, 4:6)

cc_edit<-rd_cc_change %>%
  arrange(agency, year) %>%
  select(agency,year,value,pol, party) %>%
  rename(x=year,y=value,time=year,id=agency) %>%
  mutate(ease="linear")

cc_tween<-tween_elements(cc_edit,
               "time", "id", "ease", nframes = 300) %>%
  mutate(year = round(time), agency = .group) %>%
  left_join(rd_cc_change, by=c("agency","year","pol","party"))



p<-ggplot(subset(rd_cc_change, agency!="NASA"), aes(x=year, y=value))+
  geom_col(aes(fill=party,alpha=forcats::fct_relevel(pol,"pres", after=3)),  position="dodge", width=.8, color="lightgray") +
  scale_fill_manual(values=c("Democrat" = "blue", "Republican"="red"))+
  scale_alpha_manual(values=c("hse"=.5, "sen"=.75, "pres"=1),labels=c("house", "senate", "president"))+
  labs(y="USD Millions", x="", title="agency", alpha="branch")+
  theme_bw()+
  theme(title = element_text(size=14),
        panel.background = element_rect(fill=NA),
        panel.grid = element_blank())

anim<-p+
  transition_states(agency,transition_length = 1, state_length = 4)+
  ease_aes('cubic-in-out')+
  labs(title='{closest_state} Climate Change R&D')

animate(anim, renderer = av_renderer("agency.gif"))
animate(anim, renderer = gifski_renderer("gganim.gif"))

#plot R&D
ggplot(rd_tot, aes(x=year, y=val, color=cat))+geom_line()

#plot CC R&D
ggplot(rd_cc, aes(x=year, y=value, color=Agency))+geom_line()

#compare both
pct_rd_cc <- rd_cc %>% group_by(year) %>%
  summarise(value=sum(value)) %>%
  left_join(rd_tot %>% filter(cat=="Nondefense")) %>%
  mutate(pct=value/val)

#remove NASA to see other data
pct_rd_cc_non_nasa<-rd_cc %>%
  filter(Agency !="NASA") %>%
  group_by(year) %>%
  summarise(value=sum(value)) %>%
  left_join(rd_tot %>% filter(cat=="Nondefense")) %>%
  mutate(pct=value/val)

pct_rd_cc_ag<-rd_cc %>% left_join(rd_tot %>% filter(cat=="Nondefense")) %>%
  mutate(pct=value/val)

  ggplot()+
    geom_line(data=subset(pct_rd_cc_ag, Agency !="NASA"), aes(x=year, y=pct, color=Agency))+
    geom_line(data=pct_rd_cc_non_nasa, aes(x=year, y=pct), color="red")+
    annotate("text",size=3, label="Aggregate Climate Change %", y=max(pct_rd_cc_non_nasa$pct)+.001, x=2010, hjust=0)+
    scale_y_continuous(label=function(x){paste0(x*100, "%")})+
    labs(subtitle="Non-NASA climate change research peaked in 2010", title="Climate Change R & D Funding as a Percent of All Federal Non-defense R & D")+
    theme_bw()


