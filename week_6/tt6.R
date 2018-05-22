
library(readxl)
library(tidyverse)
library(janitor)



wd<-paste0(here::here(), "/week_6")

if(getwd()!=wd) setwd(wd)

sbx<- read_excel("week6_coffee_chains.xlsx",
                 sheet = "starbucks",
                 trim_ws = TRUE
                    ) %>%
  clean_names() %>%
  dplyr::select(brand, country, state=state_province, city, long=longitude, lat=latitude)

tho<-read_excel("week6_coffee_chains.xlsx",
                sheet="timhorton",
                trim_ws = TRUE) %>%
  janitor::clean_names()

tho$addy<-paste(tho$address, tho$city, tho$state, tho$country, sep=" ")




ddo<-read_excel("week6_coffee_chains.xlsx",
                sheet="dunkin",
                trim_ws=TRUE) %>%
  clean_names()
