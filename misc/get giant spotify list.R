<<<<<<< HEAD
library(tidyverse)
library(spotifyr)
library(httr)
library(jsonlite)


sysvars<-read.csv("spotify_env.csv", stringsAsFactors = FALSE, header = FALSE)

# Sys.setenv(SPOTIFY_CLIENT_ID = sysvars$V1)
# Sys.setenv(SPOTIFY_CLIENT_SECRET = sysvars$V2)
#
#
# # playlist<-get_user_playlists("thesoundsofspotify")
# # saveRDS(playlist, "spotify_playlist.rds")
# # playlist_count<-get_user_playlist_count("thesoundsofspotify")
#
#
# playlist<-readRDS("spotify_playlist.rds")
# username <- 'thesoundsofspotify'
# # playlist_count <- get_user_playlist_count(username)
# # num_loops <- ceiling(playlist_count / 50)
# # offset <- 0
# #
# # pb <- txtProgressBar(min = 0, max = num_loops, style = 3)
# #
# # playlist_list <- map(1:ceiling(num_loops), function(this_loop) {
# #   endpoint <- paste0('https://api.spotify.com/v1/users/', username, '/playlists')
# #   res <- GET(endpoint, query = list(access_token = get_spotify_access_token(),
# #                                     offset = offset,
# #                                     limit = 50)) %>% content
# #
# #   if (!is.null(res$error)) {
# #     stop(paste0(res$error$message, ' (', res$error$status, ')'))
# #   }
# #
# #   content <- res$items
# #
# #   total <- content$total
# #   offset <<- offset + 50
# #   setTxtProgressBar(pb, this_loop)
# #   return(content)
# # })
# #
# # playlist_df <- parse_playlist_list_to_df(playlist_list)
#
# needle_list<-playlist %>% filter(grepl("Needle", playlist_name))
# sound_us<-playlist %>% filter(grepl("^The Sound of.*US$",playlist_name))
# sound_us_tracks<-get_playlist_tracks(sound_us)
# sound_us_features<-get_track_audio_features(sound_us_tracks)
#
# # needle_tracks<-get_playlist_tracks(needle_list)
# # saveRDS(needle_tracks, file = "needle_tracks.rds")
# needle_tracks<-readRDS("needle_tracks.rds") %>%
#   select(-playlist_name, -playlist_img) %>%
#   distinct()
# #
# # needle_features<-get_track_audio_features(needle_tracks) %>%
# #   left_join(needle_tracks) %>%
# #   left_join(get_track_popularity(needle_tracks))
# #
# # saveRDS(needle_features, "needle_features.rds")
# #
# # sound_us_features<-get_track_audio_features(sound_us_tracks) %>%
# #   left_join(sound_us_tracks) %>%
# #   left_join(get_track_popularity(sound_us_tracks))
#
# # saveRDS(sound_us_features, "sound_us_features.rds")
# features_us<-readRDS("sound_us_features.rds")
#
# features<-readRDS("needle_features.rds")
#
# make_us_playlists<-features_us %>%
#   group_by()
#
#
# make_playlists<-features%>%
#   group_by(key) %>%
#   arrange(desc(track_popularity)) %>%
#   top_n(100) %>%
#   split(.$key) %>%
#   map(~tail(., 50)) %>%
#   map(~paste0("spotify:track:", .$track_uri, collapse=","))
#
# key_list<-names(make_playlists)
#
# for(i in 1:length(make_playlists)){
#  x<-paste0(names(make_playlists[i]), "2.csv")
#  print(x)
#
#  write.csv(make_playlists[i], x, row.names=FALSE,append = FALSE)
# }
#
# twm<-get_user_playlists(username = "thewiremonkey")
seeds<-paste0("https://api.spotify.com/v1/recommendations/available-genre-seeds?",get_spotify_access_token())
access_token<-get_spotify_access_token()
seeds_url<-GET("https://api.spotify.com/v1/recommendations/available-genre-seeds", query = list(access_token = get_spotify_access_token()))

seed_genres<-read_json(seeds_url$url) %>%
  unlist()
get_artists("David Bowie")
genres<-seed_genres


get_recommendations<-function(genre){
  endpoint<-"https://api.spotify.com/v1/recommendations/"
  artist<-get_artists
  res<-GET(endpoint, query=list(access_token = get_spotify_access_token(), seed_genres=genre ))
 fromJSON(res$url)

}

y<-get_recommendations(genre = "goth")
albums<-y$tracks$album
artists<-y$tracks$album$artists
=======
library(tidyverse)
library(spotifyr)
library(httr)
library(jsonlite)


sysvars<-read.csv("spotify_env.csv", stringsAsFactors = FALSE, header = FALSE)

# Sys.setenv(SPOTIFY_CLIENT_ID = sysvars$V1)
# Sys.setenv(SPOTIFY_CLIENT_SECRET = sysvars$V2)
#
#
# # playlist<-get_user_playlists("thesoundsofspotify")
# # saveRDS(playlist, "spotify_playlist.rds")
# # playlist_count<-get_user_playlist_count("thesoundsofspotify")
#
#
# playlist<-readRDS("spotify_playlist.rds")
# username <- 'thesoundsofspotify'
# # playlist_count <- get_user_playlist_count(username)
# # num_loops <- ceiling(playlist_count / 50)
# # offset <- 0
# #
# # pb <- txtProgressBar(min = 0, max = num_loops, style = 3)
# #
# # playlist_list <- map(1:ceiling(num_loops), function(this_loop) {
# #   endpoint <- paste0('https://api.spotify.com/v1/users/', username, '/playlists')
# #   res <- GET(endpoint, query = list(access_token = get_spotify_access_token(),
# #                                     offset = offset,
# #                                     limit = 50)) %>% content
# #
# #   if (!is.null(res$error)) {
# #     stop(paste0(res$error$message, ' (', res$error$status, ')'))
# #   }
# #
# #   content <- res$items
# #
# #   total <- content$total
# #   offset <<- offset + 50
# #   setTxtProgressBar(pb, this_loop)
# #   return(content)
# # })
# #
# # playlist_df <- parse_playlist_list_to_df(playlist_list)
#
# needle_list<-playlist %>% filter(grepl("Needle", playlist_name))
# sound_us<-playlist %>% filter(grepl("^The Sound of.*US$",playlist_name))
# sound_us_tracks<-get_playlist_tracks(sound_us)
# sound_us_features<-get_track_audio_features(sound_us_tracks)
#
# # needle_tracks<-get_playlist_tracks(needle_list)
# # saveRDS(needle_tracks, file = "needle_tracks.rds")
# needle_tracks<-readRDS("needle_tracks.rds") %>%
#   select(-playlist_name, -playlist_img) %>%
#   distinct()
# #
# # needle_features<-get_track_audio_features(needle_tracks) %>%
# #   left_join(needle_tracks) %>%
# #   left_join(get_track_popularity(needle_tracks))
# #
# # saveRDS(needle_features, "needle_features.rds")
# #
# # sound_us_features<-get_track_audio_features(sound_us_tracks) %>%
# #   left_join(sound_us_tracks) %>%
# #   left_join(get_track_popularity(sound_us_tracks))
#
# # saveRDS(sound_us_features, "sound_us_features.rds")
# features_us<-readRDS("sound_us_features.rds")
#
# features<-readRDS("needle_features.rds")
#
# make_us_playlists<-features_us %>%
#   group_by()
#
#
# make_playlists<-features%>%
#   group_by(key) %>%
#   arrange(desc(track_popularity)) %>%
#   top_n(100) %>%
#   split(.$key) %>%
#   map(~tail(., 50)) %>%
#   map(~paste0("spotify:track:", .$track_uri, collapse=","))
#
# key_list<-names(make_playlists)
#
# for(i in 1:length(make_playlists)){
#  x<-paste0(names(make_playlists[i]), "2.csv")
#  print(x)
#
#  write.csv(make_playlists[i], x, row.names=FALSE,append = FALSE)
# }
#
# twm<-get_user_playlists(username = "thewiremonkey")
seeds<-paste0("https://api.spotify.com/v1/recommendations/available-genre-seeds?",get_spotify_access_token())
access_token<-get_spotify_access_token()
seeds_url<-GET("https://api.spotify.com/v1/recommendations/available-genre-seeds", query = list(access_token = get_spotify_access_token()))

seed_genres<-read_json(seeds_url$url) %>%
  unlist()
get_artists("David Bowie")
genres<-seed_genres


get_recommendations<-function(genre){
  endpoint<-"https://api.spotify.com/v1/recommendations/"
  artist<-get_artists
  res<-GET(endpoint, query=list(access_token = get_spotify_access_token(), seed_genres=genre ))
 fromJSON(res$url)

}

y<-get_recommendations(genre = "goth")
albums<-y$tracks$album
artists<-y$tracks$album$artists
>>>>>>> 11a465cc45338fd0907a298bf7c4a5419da5acd0
