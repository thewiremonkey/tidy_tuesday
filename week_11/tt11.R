library(tidyverse)
library(fivethirtyeight)
library(maps)

# devtools::install_github("rundel/timezone")
library(timezone)
cities<-world.cities %>% filter(capital==1)
df<-fifa_audience


df_cities<-cities %>% filter(country.etc %in% df$country)

bad_cities<-df %>% filter(!country %in% df_cities$country.etc )

df<-df %>% mutate(
  country=
    case_when(
    country=="United States" ~ "USA",
    country=="Dominican Republic" ~ "Dominica",
    country=="United Kingdom" ~ "UK",
    country=="South Korea" ~"Korea South",
    country=="Serbia" ~"Serbia and Montenegro",
    country=="Hong Kong" ~ "China",
    country=="Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
    country=="Trinidad &Tobago" ~"Trinidad and Tobago",
    country=="Kosovo" ~"Bosnia and Herzogovina",
    country=="North Korea" ~ "Korea North",
    country=="Palestine" ~"Israel",
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
safe_tz<-safely(find_tz, otherwise = NA_real_)

tz_df<-df %>% filter(!is.na(long), !is.na(lat), population_share > 0) %>%
  mutate(tzs=find_tz(long, lat))

joburg<-find_tz(28.060, -26.179)

share<-tz_df %>% group_by(tzs) %>%
  summarise(mean_tv=mean(tv_audience_share)) %>%
  filter(mean_tv>0, !is.na(tzs)) %>%
  ungroup() %>%
  mutate(time=as.POSIXct("2010-06-11 12:00", tz = "Africa/Johannesburg"))


#https://stackoverflow.com/questions/7407735/importing-wikipedia-tables-in-r?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
# install.packages("htmltab")
library(htmltab)
wiki_tz<-htmltab("https://en.wikipedia.org/wiki/List_of_tz_database_time_zones",1) %>%
  mutate(coordinates=iconv(`Coordinates*`, from = "UTF-8", to="windows-1252"),
         offset=iconv(`UTC offset`, from="UTF-8", to= "windows-1252")) %>%
  mutate(offset=ifelse(grepl("\\+", offset), gsub("\\+", "", offset),offset)  )


tz_df_plot<-tz_df %>% left_join(wiki_tz, by=c("tzs"="TZ*")) %>%
  filter(!is.na(offset), tv_audience_share > 0) %>%
  mutate(offset=as.factor(offset)) %>%
  mutate(offset=forcats::fct_relevel(offset, "-06:00","-05:00", "-04:00","-03:00"))



ggplot(tz_df_plot, aes(x=offset, y=gdp_weighted_share))+
  geom_boxplot()+
  geom_vline(aes(xintercept=7))


ggplot(tz_df_plot, aes(x=log10(pop), y=tv_audience_share))+
  geom_point()

kclust<-kmeans(tz_df_plot$tv_audience_share, 3)

k_df<-broom::augment(kclust, tz_df_plot)

ggplot(k_df, aes(x=pop, y=tv_audience_share))+
  geom_point(aes(color=.cluster))
