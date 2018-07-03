library(spotifyr)
library(geniusr)
library(tidyverse)

Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxx')

bowie<- get_artist_audio_features('David Bowie')

bowie<-bowie %>%
  mutate(year=lubridate::year(lubridate::ymd(bowie$album_release_year)))

bowiecor<-bowie %>% select_if(is.numeric) %>%
  mutate_if(is.character, as.factor)

levels(as.factor(bowie$key))
corrplot::corrplot(cor(bowiecor),type = "lower", order="hclust")

library(rpart)
bowietree<-rpart(track_popularity ~ ., bowiecor %>% select(-album_popularity))
plot(bowietree)
text(bowietree,  use.n = TRUE, all=FALSE)

library(cluster)
hclust(dist(bowiecor))


# library(ape)
library(cluster)
library(dendextend)
library(circlize)


bowie_dend<-bowie %>% select(album_name, danceability, energy, key,speechiness, acousticness, instrumentalness, liveness, valence, tempo, album_popularity, mode)

# bowie_dend<-bowie %>% select(album_name, key)

bowie_full<-bowie_dend %>%
  mutate(album_name=gsub( " *\\(.*?\\) *| *\\[.*?\\] *", "", album_name),
         album_name=ifelse(grepl("Peter", album_name, ignore.case=TRUE), "Peter And The Wolf", album_name),
         album_name=ifelse(grepl("Ziggy",album_name), "...Ziggy Stardust...", album_name) )%>%
  group_by(album_name) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  # filter(grepl("remastered", album_name, ignore.case=TRUE)) %>%
  as.data.frame()

rownames(bowie_full)<-bowie_full[,1]

bowie_full<-bowie_full %>% select(-1)

#make hierarchical cluster
b_hc<-hclust(dist(bowie_full))

# plot(as.phylo(b_hc),type="fan")


# install.packages("dendextend")
# install.packages("circlize")


# create a dendrogram
b_dend <- as.dendrogram(b_hc)

# modify the dendrogram to have some colors in the branches and labels
b_dend <- b_dend %>%
  color_branches(k=8) %>%
  color_labels(k=8)

# plot the radial plot
# par(mar = rep(6,6))
circlize_dendrogram(b_dend, labels_track_height = 0.4,dend_track_height = .4)
# circlize_dendrogram(b_dend, labels_track_height = NA, dend_track_height = .4)


bowie<-bowie %>% select_if(is.numeric) %>%
  bind_cols(bowie %>% select(album_name)) %>%
  mutate(album_name=gsub( " *\\(.*?\\) *| *\\[.*?\\] *", "", album_name),
         album_name=ifelse(grepl("Peter", album_name, ignore.case=TRUE), "Peter And The Wolf", album_name),
         album_name=ifelse(grepl("Ziggy",album_name), "...Ziggy Stardust...", album_name) )%>%
  group_by(album_name) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  # top_n(20, album_popularity) %>%
  ungroup() %>%
  select(-album_popularity) %>%
  column_to_rownames("album_name") %>%
  as.data.frame()



top_dend<-as.dendrogram(hclust(dist(bowie))) %>%
  color_branches(k=8) %>%
  color_labels(k=8)
par(mar=c(0,5,0,5))
circlize_dendrogram(top_dend,labels_track_height = 0.4,dend_track_height = .4)


ggplot(bowie)
