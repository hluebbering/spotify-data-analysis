# load libraries
library(jsonlite)
library(lubridate)
library(gghighlight)
library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)
library(tm)
library(genius)
library(tidytext)
library(kableExtra)


# user information
Sys.setenv(SPOTIFY_CLIENT_ID = '4cf3afdca2d74dc48af9999b1b7c9c61')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f6ca08ad37bb41a0afab5ca1dc74b208')
get_spotify_authorization_code()


#get_playlist_audio_features()
plTracks <- get_playlist_tracks('37i9dQZEVXbLRQDuF5jeBp')

# get each track name, number, and id
plTracksName <- plTracks$track.name # track name
totalTracks <- length(plTracksName) # total number
plTracksID <- plTracks$track.id # track id

# get each track artist
myArtists <- plTracks$track.artists
myArtistsName <- c()
myArtistsID <- c()

for(n in 1:totalTracks){
  if(length(myArtists[[n]][[3]])){
    myArtistsName <- append(
      myArtistsName, 
      myArtists[[n]][[3]][[1]])
    
    myArtistsID <- append(
      myArtistsID,
      myArtists[[n]][[2]][[1]])
  }
  else{
    myArtistsName <- append(
      myArtistsName, 
      myArtists[[n]][[3]])
    myArtistsID <- append(
      myArtistsID, 
      myArtists[[n]][[2]])
  }
}


# get each track album name, ID, release date
plTracksAlbum <- plTracks$track.album.name
plTracksAlbumID <- plTracks$track.album.id
plTracksAlbumRelease <- plTracks$track.album.release_date

# get each track popularity and color
plTracksPopularity <- plTracks$track.popularity
plTracksColor <- plTracks$primary_color

# get each track duration
plTrackDuration <- plTracks$track.duration_ms

# get each track info
plTracksInfo1 <- get_track_audio_features(plTracksID[0:totalTracks])
plTracksInfo <- rbind.data.frame(plTracksInfo1)
plTrackDanceability <- plTracksInfo[, "danceability"]
plTrackEnergy <- plTracksInfo[, "energy"]
plTrackAcousticness <- plTracksInfo[, "acousticness"]
plTrackKey <- plTracksInfo[, "key"]
plTrackLoudness <- plTracksInfo[, "loudness"]
plTrackMode <- plTracksInfo[, "mode"]
plTrackSpeechiness <- plTracksInfo[, "speechiness"]
plTrackInstrumentalness <- plTracksInfo[, "instrumentalness"]
plTrackLiveness <- plTracksInfo[, "liveness"]
plTrackValence <- plTracksInfo[, "valence"]
plTrackTempo <- plTracksInfo[, "tempo"]



df_playlist1 <- data.frame(
  "track.id" = plTracksID,
  "track.name" = plTracksName,
  "track.artist" = myArtistsName,
  "track.artist.id" = myArtistsID,
  "track.album.id" = plTracksAlbumID,
  "track.album.name" = plTracksAlbum,
  "track.popularity" = plTracksPopularity,
  "track.album.release" = plTracksAlbumRelease,
  "danceability" = plTrackDanceability,
  "energy" = plTrackEnergy,
  "key" = plTrackKey,
  "loudness" = plTrackLoudness,
  "mode" = plTrackMode,
  "speechiness" = plTrackSpeechiness,
  "acousticness" = plTrackAcousticness,
  "instrumentalness" = plTrackInstrumentalness,
  "liveness" = plTrackLiveness,
  "valence" = plTrackValence,
  "tempo" = plTrackTempo,
  "duration_ms" = plTrackDuration 
)





###################



artist_id_list <- df_playlist1$track.artist.id
total_artist_list <- rbind(get_artists(artist_id_list[1:50])) # get_artists doesn't allow for more than 50 requests at a time

#genre_list <- total_artist_list$genres #unlist(total_artist_list$genres)
df_playlist1$genres <- total_artist_list$genres
df_playlist1$artist.followers <- total_artist_list$followers.total
df_playlist1$artist.popularity <- total_artist_list$popularity





########################################### 


playlistTracks <- df_playlist1 %>%
  mutate(position = rank(-track.popularity, ties.method = "first")) %>%
  arrange(position) %>%
  select(
    c(
      position,
      track.name,
      track.album.name,
      track.artist,
      track.album.release,
      duration_ms,
      track.popularity
    )
  )

colnames(playlistTracks) <- c("rank", "track", "album", "artists", "release", "duration", "popularity")

trackFeatures <- df_playlist1 %>%
  mutate(position = rank(-track.popularity, ties.method = "first")) %>%
  arrange(position) %>% 
  select(c(position, track.name, danceability, energy,
           key, loudness, mode, speechiness, acousticness,
           instrumentalness, liveness, valence, tempo, duration_ms))



