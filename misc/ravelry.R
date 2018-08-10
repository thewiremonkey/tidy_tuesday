library(httr)
library(XML)
library(jsonlite)
library(paletteer)

if(!grepl("/misc", getwd())){setwd("./misc")}
# user_rav.txt contains API username and password
credentials <- readLines("user_rav.txt")
names(credentials) <- c("user","access_key","secret_key")

OpenConnection <- function(credentials){
  # Args: login info for the Ravelry API
  # Returns oauth token
  # Open connection to Ravelry API and return token
  reqURL <- "https://www.ravelry.com/oauth/request_token"
  accessURL <- "https://www.ravelry.com/oauth/access_token"
  authURL <- "https://www.ravelry.com/oauth/authorize"

  ravelry.app <- oauth_app("ravelry", key=credentials["access_key"],
                           secret=credentials["secret_key"])
  ravelry.urls <- oauth_endpoint(reqURL, authURL, accessURL)

  return(oauth1.0_token(ravelry.urls, ravelry.app))
}

# Quick test of API connection by getting connected user info
TestConnection <- function(ravelry.token) {
  # Arg: API token
  # Returns name of the user connected with this token
  test <- GET("https://api.ravelry.com/current_user.json",
              config=config("token"=ravelry.token))
  print(content(test)$user$username)
}

ravelry.token <- OpenConnection(credentials)
TestConnection(ravelry.token)

## Build dataset from Ravelry API: pattern permalink, pattern category, pattern text description
# Get url to patterns of interest from API search
pat0 <- GET("https://api.ravelry.com/patterns/search.json?page_size=20&craft=knitting", config=config("token"=ravelry.token))
pat <- content(pat0)


color_fam<-GET("https://api.ravelry.com/color_families.json", config=config("token"=ravelry.token))
color_fam<-content(color_fam, as="text", encoding="UTF-8") %>%
  fromJSON(flatten=TRUE)
color_fame<-color_fam$color_families

# colors<-sapply(color_fam$color_families, function(x) x$permalink)
colors<-unlist(map(color_fam$color_families, `[[`,2))

#use the number of  ratings
#get top rated wool
wool_raw<-GET("https://api.ravelry.com/yarns/search.json?fiber-content=wool&fiberc=1&ratings=5&sort=rating&photo=yes&discontinued=no&page_size=2000", config=config("token"=ravelry.token))

wool<-content(wool_raw, as="text", encoding="UTF-8") %>%
  fromJSON(flatten=TRUE)
wool<-wool$yarns

more<-GET("https://api.ravelry.com/yarns/search.json?page=1&fiber-content=wool&fiberc=1&ratings=3&sort=rating&photo=yes&discontinued=no&page_size=2000", config=config("token"=ravelry.token))
more<-content(more, as="text", encoding="UTF-8") %>%
  fromJSON(flatten=TRUE)
more<-more$yarns

more_wool<-rbind(more_wool,  more) %>% distinct() %>%
  mutate(fiber="wool")

cotton_raw<-GET("https://api.ravelry.com/yarns/search.json?fiber-content=cotton&fiberc=1&ratings=3&sort=rating&photo=yes&discontinued=no&page_size=2000", config=config("token"=ravelry.token))
cotton<-content(cotton_raw, as="text", encoding="UTF-8") %>%
  fromJSON(flatten=TRUE)
cotton<-cotton$yarns
all_cotton<-rbind(cotton, all_cotton) %>% distinct() %>%
  mutate(fiber="cotton")

acrylic_raw<-GET("https://api.ravelry.com/yarns/search.json?page=2&fiber-content=acrylic&fiberc=1&ratings=4&sort=rating&photo=yes&discontinued=no&page_size=2000", config=config("token"=ravelry.token))
acrylic<-content(acrylic_raw, as="text", encoding="UTF-8") %>%
  fromJSON(flatten=TRUE)
acrylic<-acrylic$yarns

all_acrylic<-rbind(acrylic, all_acrylic) %>% distinct() %>%
  mutate(fiber="acrylic")


silk_raw<-(GET("https://api.ravelry.com/yarns/search.json?fiber-content=silk&fiberc=1&photo=yes&discontinued=no&page_size=2000", config=config("token"=ravelry.token)))
silk<-content(silk_raw, as="text", encoding="UTF-8") %>%
  fromJSON(flatten=TRUE)
silk<-silk$yarns
silk<-silk %>% filter(!is.na(yarn_weight.name), !is.na(grams), !is.na(yardage)) %>%
  mutate(fiber="silk")

projects<-GET("https://api.ravelry.com/projects/search.json?query=dalek", config=config("token"=ravelry.token))
prj<-content(projects, as="text", encoding="UTF-8") %>% fromJSON(flatten=TRUE)
prj<-prj$projects


p_df<-more_wool %>% filter(!is.na(yarn_weight.wpi), !is.na(grams), !is.na(yardage), yardage<5000, grams<600)

ggplot(p_df, aes(x=grams, y=yardage, color=yarn_weight.name,group=yarn_weight.name))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()

p_cotton_df<-all_cotton %>% filter(!is.na(yarn_weight.wpi), !is.na(grams), !is.na(yardage), yardage<5000, grams<600)
ggplot(p_cotton_df, aes(x=grams, y=yardage, color=yarn_weight.name, group=yarn_weight.name))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()

all_yarn<-bind_rows(more_wool, all_cotton, all_acrylic, silk) %>%
  distinct()

all_yarn_fit<-all_yarn %>%
  filter(!is.na(grams), !is.na(yardage)) %>%
  group_by(fiber, yarn_weight.name) %>%
  nest() %>%
  mutate(mod=map(.$data, ~lm(.x$grams~.x$yardage)),
         slope=map_dbl(mod, ~pluck(coef(.x),".x$yardage"))) %>%
  arrange(desc(slope))

all_yarn_levels<-all_yarn_fit %>%
  select(yarn_weight.name, slope) %>%
  group_by(yarn_weight.name) %>%
  summarise(slope=mean(slope)) %>%
  arrange(slope)

reorder(all_yarn_levels$yarn_weight.name, all_yarn_levels$slope)


p_all_df<-all_yarn %>% filter(!is.na(yarn_weight.name),!is.na(grams), !is.na(yardage), yardage<5000, grams>0,grams<=600, yarn_weight.name !="Aran / Worsted") %>%
  mutate(yarn_weight.name=factor(yarn_weight.name, reorder(all_yarn_levels$yarn_weight.name, all_yarn_levels$slope)))

f_plot<-ggplot(p_all_df, aes(x=grams, y=yardage, color=fiber, group=fiber))+
  geom_point(alpha=0.3)+
  geom_smooth(method="glm", se=FALSE)+
  theme_bw()+
  # theme(panel.background = element_rect(fill = "#FBEEE6"), strip.background = element_rect(fill="white"))+
  scale_color_manual(values=c("#41668e", "#229954", "#F1C40F","#A93226"))+
  facet_wrap(~yarn_weight.name)+
  ggtitle("Yardage by Fiber and Weight")+
  labs(caption="data: Ravelry.com API\nViz: Alyssa Goldberg @WireMonkey")

ggsave(f_plot,device = "png",filename = "yarnplot.png",path = getwd())

ggplot(p_all_df, aes(x=grams, y=yarn_weight.wpi, color=fiber, group=fiber))+
  geom_point(alpha=0.3)+
  geom_smooth(method="glm", se=FALSE)+
  theme_bw()+
  theme(panel.background = element_rect(fill = "#FBEEE6"), strip.background = element_rect(fill="white"))+
  scale_color_manual(values=c("#41668e", "#229954", "#f8621b"))+
  facet_wrap(~yarn_weight.name)+
  ggtitle("Yardage by Fiber and Weight")
