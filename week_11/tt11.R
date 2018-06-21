library(tidyverse)
library(fivethirtyeight)
library(maps)
library(forcats)
# devtools::install_github("rundel/timezone")
library(timezone)
library(htmltab)

#set raw data frame from fivethirtyeight data
df<-fifa_audience

#get list of city capitals as proxy for countries' time zone.
#Obviously bigger countries have multiple time zones, but the data is
#grouped by country so we have to pick just one

cities<-world.cities %>% filter(capital==1)

#country names don't match up between the data sets.  Find out which ones
#do and which ones don't
df_cities<-cities %>% filter(country.etc %in% df$country)

#list of countries  that don't appear in the df_cities data frame
#many of them are disputed territories.  Rather than get into an unending Twitter
#battle by assigning them to another country, I've left them out.  Most don't reach the .2 audience share anyway
bad_cities<-df %>% filter(!country %in% df_cities$country.etc )

#rename countries so that they'll be available to find the timezone
df<-df %>% mutate(
  country=
    case_when(
    country=="United States" ~ "USA",
    country=="Dominican Republic" ~ "Dominica",
    country=="United Kingdom" ~ "UK",
    country=="South Korea" ~"Korea South",
    country=="Serbia" ~"Serbia and Montenegro",
    country=="Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
    country=="Trinidad &Tobago" ~"Trinidad and Tobago",
    country=="Kosovo" ~"Bosnia and Herzogovina",
    country=="North Korea" ~ "Korea North",
    country=="Montenegro" ~ "Serbia and Montenegro",
    country=="Turks & Caicos" ~"Turks and Caicos",
    country=="Congo DR"  ~ "Congo Democratic Republic",
    country=="Fiji" ~"French Polynesia",
    country=="Curacao" ~ "Netherlands Antilles",
    country=="St. Lucia" ~"Saint Lucia",
    country=="Congo, Rep." ~ "Congo",
    country=="Antigua & Barbuda" ~ "Antigua and Barbuda",
    country=="St. Vincent" ~ "Saint Vincent and The Grenadines",
    country=="Timor" ~ "East Timor",
    country== "St. Kitts" ~ "Saint Kitts and Nevis",
    TRUE ~ country
  )
) %>%
  left_join(cities, by=c("country"="country.etc"))

#use purrr's safely to allow the function to run without  throwing an error
safe_tz<-safely(find_tz, otherwise = NA_real_)

#filter out any records that don't have a longitude and latitude
#find the timezone of the remaining countries
tz_df<-df %>% filter(!is.na(long), !is.na(lat), population_share > 0) %>%
  mutate(tzs=find_tz(long, lat))

#tutorial for scraping tables from wikipedia
#https://stackoverflow.com/questions/7407735/importing-wikipedia-tables-in-r?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
# install.packages("htmltab")


#get wikipedia table of timezone offsets
#clean the offset to remove the plus sign.  I thought I was going to be able
#to do some math with this, but abandoned that idea
wiki_tz<-htmltab("https://en.wikipedia.org/wiki/List_of_tz_database_time_zones",1) %>%
  mutate(coordinates=iconv(`Coordinates*`, from = "UTF-8", to="windows-1252"),
         offset=iconv(`UTC offset`, from="UTF-8", to= "windows-1252")) %>%
  mutate(offset=ifelse(grepl("\\+", offset), gsub("\\+", "", offset),offset)  )

#create the table we'll use for the viz
tz_df_plot<-tz_df %>% left_join(wiki_tz, by=c("tzs"="TZ*")) %>%
  filter(!is.na(offset), tv_audience_share > 0) %>%
  mutate(offset=as.factor(offset)) %>%
  mutate(offset=forcats::fct_relevel(offset, "-06:00","-05:00", "-04:00","-03:00"))
#use forcats to refactor the negatives, which originally came in from -3:00 to -6:00

#create the sum of audience share per timezone
sum_tz_df<-tz_df_plot %>% group_by(offset) %>%
  summarise(share=sum(tv_audience_share))


#Tidy Tuesday Plot
ggplot(sum_tz_df, aes(x=offset, y=share))+
  geom_point(color="blue", size=3)+
  geom_jitter(data=tz_df_plot, aes(x=offset, y=tv_audience_share, color=confederation))+
  geom_vline(aes(xintercept=7), linetype="dashed", color="red")+
  annotate(geom = "text", x = 7, y = 10, label = "Johannesburg", color = "red",
           angle = 90, vjust=0)+
  ggtitle(label = "Cumulative Audience Share by Timezone", subtitle="blue dot = sum of audience share")+
  labs(x="UTC Offset", y="Audience Share", caption="data source: fivethirtyeight.com\nviz: Alyssa Goldberg @WireMonkey 2018\n#TidyTuesday @thomas_mock")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))


