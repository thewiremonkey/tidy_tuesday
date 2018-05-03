library(tidyverse)
library(acs)
library(noncensus)
library(magrittr)
library(readxl)
library(maps)
library(ghibli)
library(RColorBrewer)

here::here()
url<-"https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/acs2015_county_data.csv"

dat<-read_csv(url) %>%
  janitor::clean_names()

county_map<-map_data('county')
state_map<-map_data('state')

pal<-RColorBrewer::brewer.pal(n = 9,name = "Set1")

data("states")
data("counties")
counties %<>% mutate(census_id=paste(as.character(state_fips), as.character(county_fips),sep=""))

#data from: https://www.census.gov/support/USACdataDownloads.html#LND
#Metafile from: www2.census.gov/prod2/statcomp/usac/excel/Mastdata.xls
LND01 <- read_excel("week_5/LND01.xls") %>% select(STCOU,LND110210D,Areaname) %>%
  rename(census_id=STCOU)


dat <- dat[complete.cases(dat), ]
dat <-dat %>%
    mutate(census_id=as.character(census_id))%>%
  mutate(chars=nchar(census_id))%>%
  mutate(census_id=case_when(
    chars==4 ~ paste0("0", census_id, sep=""),
    chars==5 ~ census_id,
    TRUE ~ census_id
  ))%>%
  left_join(LND01)

gg_dat<-dat %>% select(census_id,state, county, area=LND110210D, pop=total_pop, drive, carpool, transit, walk, other_transp, work_at_home, mean_commute,native) %>%
  mutate(dens=pop/area, region=tolower(state), subregion=tolower(county)) %>%
  # filter(!is.na(dens)) %>%
  left_join(states %>% select(abbr=state,state=name,  division)) %>%
  mutate_if(is.factor, as.character())

#get division names
div_names<-unique(gg_dat$division)[1:9]
#bind div names to pal to ensure  consistency
names(pal)<-div_names

all_map<-county_map %>%
  left_join(gg_dat, by=c("region", "subregion"))

ggplot()+
  geom_polygon(data = all_map, aes(x=long, y=lat, group=group, fill=division), color=NA)+
  geom_polygon(data=state_map,aes(x=long, y=lat, group=group), fill=NA, color="black")+
  theme_void()+
  scale_fill_manual(values=pal)+
  labs(title="Region Reference, or The Importance of Consistency in Place Names", subtitle="NA are counties whose names do not match between map_data('county') and the given data set", caption="viz by @WireMonkey\ncounty data: ACS 2015 County Data\nland area: USACS LND table\ntopic selection: @ThomasMock")


# p<-ggplot(gg_dat, aes(x=dens))
# p1<-p+
#   geom_point(aes(y=mean_commute, color=division))+
#   geom_smooth(aes(y=mean_commute),method='loess')+
#   geom_hline(aes(yintercept=median(gg_dat$mean_commute)))+
#   scale_x_log10()

ggplot(gg_dat, aes(x=dens, y=mean_commute, group=division))+
  # geom_point(aes(color=division))+
  geom_smooth(aes(y=mean_commute, color=division),se=F,method='loess')+

  scale_x_log10()+
  scale_color_manual(values=pal)+
  labs(title="De-spaghettified, Trends are a Little Easier to See",
       x="Population Density\nlog10 transformation", y="Mean Commute Time",
       caption="viz by @WireMonkey\ncounty data: ACS 2015 County Data\nland area: USACS LND table\ntopic selection: @ThomasMock")+
  theme_bw()+
  facet_wrap(~division)

sweet<-gg_dat %>% filter(mean_commute>23.5, mean_commute<24.5)

ggplot(gg_dat, aes(x=dens, y=mean_commute, group=division))+
  # geom_point(aes(color=division))+
  geom_smooth(aes(y=mean_commute, color=division),se=F,method='loess')+
  geom_vline(aes(xintercept=mean(sweet$dens, na.rm=T)))+
  scale_x_continuous(trans="log",breaks=c(1,10,100,250,500,1000,10000,100000),
                     labels=c("1", "10", "100", "250","500","1,000", "10,000", "100,000"))+
  scale_color_manual(values=pal)+
  labs(title="As Density Increases, Mean Commute Time Tends to Increase",
       subtitle="However, there appears to be a sweet spot, around 225 people/square mile where mean commute time dips",
       x="Population Density\nlog10 transformation", y="Mean Commute Time",
       caption="viz by @WireMonkey\ncounty data: ACS 2015 County Data\nland area: USACS LND table\ntopic selection: @ThomasMock")+
  theme_bw()+
  theme(plot.caption = element_text(size=10) )


