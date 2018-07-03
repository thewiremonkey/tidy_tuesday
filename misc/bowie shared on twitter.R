library(spotifyr)#http://www.rcharlie.com/spotifyr/
library(tidyverse)
library(rpart)
library(dendextend) #for colors and whatnot
library(circlize) #https://stats.stackexchange.com/questions/4062/how-to-plot-a-fan-polar-dendrogram-in-r

#follow http://www.rcharlie.com/spotifyr/ instructions for setting up a spotify account to get access to api.
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxx')

bowie<-if(!file.exists("bowie.rds")){
  bowie<- spotifyr::get_artist_audio_features('David Bowie')
  saveRDS(bowie, "bowie.rds")}else{
    readRDS("bowie.rds")
  }
#there are a few options for the name, type the one you want in the console.

#pull the numeric stats, rebind to the album names.  I didn't factorize the string #fields, such as mode and key because those are on the track level and I don't know how useful they'd be as means. Do some cleanup on the names--most of the Bowie albums on Spotify are "([year] Remastered)" Also Peter and the Wolf had several variants, as did Ziggy Stardust.

hc_bowie<-bowie %>% select_if(is.numeric) %>%
  bind_cols(bowie %>% select(album_name)) %>%
  mutate(album_name=gsub( " *\\(.*?\\) *| *\\[.*?\\] *", "", album_name),
         album_name=ifelse(grepl("Peter", album_name, ignore.case=TRUE), "Peter And The Wolf", album_name),
         album_name=ifelse(grepl("Ziggy",album_name), "...Ziggy Stardust...", album_name) )%>%
  group_by(album_name) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  ungroup() %>%
  column_to_rownames("album_name") %>%
  as.data.frame()

#create the clusters, make 'em pretty
top_dend<-as.dendrogram(hclust(dist(hc_bowie))) %>%
  color_branches(k=8) %>%
  color_labels(k=8)

#circlize  it
circlize_dendrogram(top_dend,labels_track_height = 0.3,dend_track_height = .3)

key_of_B<-bowie %>% group_by(key_mode) %>% 
  count()

ggplot(bowie, aes(x = valence, y = album_name)) + 
  ggjoy::geom_joy() + 
  ggjoy::theme_joy() +
  ggtitle("Joyplot of Joy Division's joy distributions", subtitle = paste0("Based on valence pulled from Spotify's Web API with spotifyr"))
#> Picking joint bandwidth of 0.112
#
kmeans_bowie<-bowie %>%select(-c(album_uri, track_uri, album_img)) %>% 
  mutate_if(is.character, as.factor)

kb<-kmeans_bowie %>% select(track_popularity, key_mode) %>% 
  mutate(key_mode=as.numeric(key_mode))

cluster<-kmeans(kb[,1:2],10)
auggie_stardust<-broom::augment(cluster, bowie)
