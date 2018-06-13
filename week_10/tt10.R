library(tidyverse)
library(lubridate)

if(!grepl("tidy_tuesday/week_10", getwd())){setwd("../Desktop/tidy_tuesday/week_10")}

#get a list of all raw files
files<-list.files(path = "PublicTripData",full.names = TRUE)

#read and combine them
raw<-map_df(files, ~read.csv(.[1])) %>%
  filter(!is.na(Duration), Duration !="")



#set data types, create Pair field by concatenating start and end hubs
df<-raw %>%
  mutate(RouteID=as.character(RouteID),
         StartDate=lubridate::mdy(StartDate),
         StartTime=lubridate::hm(StartTime),
         EndDate=lubridate::mdy(EndDate),
         EndTime=lubridate::hm(EndTime),
         BikeID=as.character(BikeID),
         Duration=as.numeric(as.difftime(Duration)),
         BikeType=trimws(gsub("[[:digit:]-]+", "", BikeName)),
         Pair=case_when(
           StartHub==EndHub ~ "same",
           TRUE ~ paste0(StartHub,"-",  EndHub)
         )) %>%
  filter(Distance_Miles<90, EndDate < "2018-05-01", StartHub !="", EndHub !="")

#get the count and mean milage for each pair of start and end hub points
pair<-df %>% group_by(Pair, PaymentPlan) %>%
  summarise(avg_dist=mean(Distance_Miles), count=n())


#get the top ten routes used by casual payers
casual_route<-pair %>% ungroup() %>%
  filter(PaymentPlan=="Casual",Pair !="same") %>%
  arrange(desc(count)) %>%
  top_n(20) %>%
  left_join(df %>% select(StartHub, EndHub,StartLongitude, StartLatitude, EndLongitude, EndLatitude, Pair) %>% distinct(), by="Pair")

#get the top ten routes used by subscription payers
subscriber_route<-pair %>% ungroup() %>%
  filter(PaymentPlan=="Subscriber", Pair !="same") %>%
  arrange(desc(count)) %>%
  top_n(20)%>%
  left_join(df %>% select(StartHub, EndHub,StartLongitude, StartLatitude, EndLongitude, EndLatitude, Pair) %>% distinct(), by="Pair")

same_route=df %>% ungroup() %>%
  filter(Pair=="same") %>%
  group_by(Pair,StartHub, StartLongitude, StartLatitude) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(20, n)

#get a map of Portland
portland<-ggmap::get_map("Portland, Oregon", zoom=12, source="stamen", maptype = "toner-lite", crop=TRUE)

#create a map of Portland, plot segments between start and end hubs, crop the image
rides<-ggmap::ggmap(portland)+
  geom_point(data=casual_route, aes(x=StartLongitude, y=StartLatitude), size=3, color="DarkGreen")+
  geom_point(data=subscriber_route, aes(x=StartLongitude, y=StartLatitude), size=3, color="red")+
  geom_point(data=casual_route, aes(x=EndLongitude, y=EndLatitude), size=3, color="DarkGreen")+
  geom_point(data=subscriber_route, aes(x=EndLongitude, y=EndLatitude), size=3, color="red")+
  geom_point(data=same_route, aes(x=StartLongitude, y=StartLatitude), size=3, color="blue", shape=21)+
  geom_segment(data=casual_route, aes(x=StartLongitude, xend=EndLongitude, y=StartLatitude, yend=EndLatitude), arrow = arrow(ends = "last",length = unit(0.2, "cm")),color="DarkGreen")+
  geom_segment(data=subscriber_route,aes(x=StartLongitude, xend=EndLongitude, y=StartLatitude, yend=EndLatitude), color="red", arrow = arrow(ends = "last",length = unit(0.2, "cm")))+
  # scale_y_continuous(limits=c(45.49, 45.55))+
  # scale_x_continuous(limits=c(-122.7, -122.65))+
  # theme_void()+
  ggtitle("Casual Riders Appear to Prefer the Waterfront", subtitle = "red=subscription\ngreen=casual")

png("tt10.png", type = "cairo")
rides
dev.off()

top_1_percent<-quantile(pair$count, seq(0,1, 0.01))[[100]]
top_quant_pair<-pair %>% filter(count>top_1_percent) %>%
  left_join(df %>% select(StartHub, EndHub,StartLongitude, StartLatitude, EndLongitude, EndLatitude, Pair) %>% distinct(), by="Pair")

ggmap::ggmap(portland)+
  geom_segment(data=top_quant_pair, aes(x=StartLongitude, xend=EndLongitude, y=StartLatitude, yend=EndLatitude, color=PaymentPlan), arrow = arrow(ends = "last",length = unit(0.2, "cm")), show.legend = TRUE)+
  scale_y_continuous(limits=c(45.5, 45.565))+
  scale_x_continuous(limits=c(-122.7, -122.60))
