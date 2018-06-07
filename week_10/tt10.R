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
  top_n(10) %>%
  left_join(df %>% select(StartHub, EndHub,StartLongitude, StartLatitude, EndLongitude, EndLatitude, Pair) %>% distinct(), by="Pair")

#get the top ten routes used by subscription payers
subscriber_route<-pair %>% ungroup() %>%
  filter(PaymentPlan=="Subscriber", Pair !="same") %>%
  arrange(desc(count)) %>%
  top_n(10)%>%
  left_join(df %>% select(StartHub, EndHub,StartLongitude, StartLatitude, EndLongitude, EndLatitude, Pair) %>% distinct(), by="Pair")

same_route=df %>% ungroup() %>%
  filter(Pair=="same") %>%
  group_by(Pair,StartHub, StartLongitude, StartLatitude) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(10)

#get a map of Portland
portland<-ggmap::get_map("Portland, Oregon", zoom=13, source="stamen", maptype = "toner-lite", crop=TRUE)

#create a map of Portland, plot segments between start and end hubs, crop the image
rides<-ggmap::ggmap(portland)+
  geom_point(data=casual_route, aes(x=StartLongitude, y=StartLatitude), size=3, color="DarkGreen")+
  geom_point(data=subscriber_route, aes(x=StartLongitude, y=StartLatitude), size=3, color="red")+
  geom_point(data=casual_route, aes(x=EndLongitude, y=EndLatitude), size=3, color="DarkGreen")+
  geom_point(data=subscriber_route, aes(x=EndLongitude, y=EndLatitude), size=3, color="red")+
  geom_point(data=same_route, aes(x=StartLongitude, y=StartLatitude), size=3, color="blue", shape=21)+
  geom_segment(data=casual_route, aes(x=StartLongitude, xend=EndLongitude, y=StartLatitude, yend=EndLatitude), arrow = arrow(ends = "last",length = unit(0.2, "cm")),color="DarkGreen")+
  geom_segment(data=subscriber_route,aes(x=StartLongitude, xend=EndLongitude, y=StartLatitude, yend=EndLatitude), color="red", arrow = arrow(ends = "last",length = unit(0.2, "cm")))+
  scale_y_continuous(limits=c(45.49, 45.55))+
  scale_x_continuous(limits=c(-122.7, -122.65))+
  theme_void()+
  ggtitle("Casual Riders Appear to Prefer the Waterfront", subtitle = "red=subscription\ngreen=casual")

png("tt10.png", type = "cairo")
rides
dev.off()

age<-df %>% group_by(BikeID,BikeType) %>%
  summarise(start=min(StartDate), end=max(EndDate), miles=sum(Distance_Miles, na.rm=TRUE), uses=n()) %>%
  filter(uses>0, miles>0) %>%
  mutate(age=as.numeric(difftime(time1 = end, time2 = start, units = "days")), pop=uses/age) %>%
  filter(!grepl("LOST", BikeType, ignore.case = T),
         !grepl("quar", BikeType, ignore.case = T),
         !grepl("rec", BikeType, ignore.case=T),
         !grepl("police", BikeType, ignore.case = T),
         !grepl("damag", BikeType, ignore.case = T),
         !grepl("wait", BikeType, ignore.case = T),
         BikeType !="",
         age>0)

mean_age<-age %>% group_by(BikeType) %>%
  summarise(avg_age=mean(age), avg_uses=mean(uses),start=min(start), end=max(end), avg_miles=mean(miles)) %>%
  mutate(popular=avg_uses/avg_age)


ggplot(mean_age)+
  geom_segment(aes(x=start, xend=end, y=avg_miles, yend=avg_miles, color=BikeType))+
  geom_point(aes(x=start, y=avg_miles), color="blue")+
  geom_point(aes(x=end, y=avg_miles), color="red")

ggplot(mean_age)+
  geom_segment(aes(x=start, xend=end, y=popular, yend=popular, color=BikeType), show.legend = FALSE)+
  geom_point(aes(x=start, y=popular), color="blue")+
  geom_point(aes(x=end, y=popular), color="red")+
  geom_text(aes(x=start, y=popular, label=BikeType), size=2, hjust=1)

ggplot(age) +
  geom_boxplot(aes(x=BikeType, y=pop))+
  geom_hline(aes(yintercept=mean(pop)))+
  theme(axis.text.x=element_text(angle=90,hjust=1))
