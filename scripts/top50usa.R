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

# user information
Sys.setenv(SPOTIFY_CLIENT_ID = '4cf3afdca2d74dc48af9999b1b7c9c61')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f6ca08ad37bb41a0afab5ca1dc74b208')
get_spotify_authorization_code()

# -----------------------------------------------------------------

## GET PLAYLIST GENRES
pl <- get_playlist('37i9dQZEVXbLRQDuF5jeBp')
tr <- pl$tracks
tr_items <- tr$items

# Removing unwanted information
pl_list <- tr_items[, c('track.artists', 'track.id', 'track.name', 'track.album.name')]

# Removing nested data frame with artist information
i <- pl_list$track.artists
artist_names <- sapply(i, '[[', "name")
artist_id <- sapply(i, '[[', "id")

pl_list$artist.names <- artist_names
pl_list$artist.id <- artist_id


# Re-ordering and renaming columns
pl_list <- pl_list[, c('artist.names', 'track.name', 'track.album.name', 'track.id', 'artist.id')]
colnames(pl_list) <- c('artist', 'track', 'album', 'track_id', 'artist_id')


# Getting genres - get_artists() generates dataframe of artists and characteristics
artist_id_list <- unlist(artist_id)
total_artist_list <- rbind(get_artists(artist_id_list[1:50]), # get_artists doesn't allow for more than 50 requests at a time
                           get_artists(artist_id_list[51:55]))
genre_list <- unlist(total_artist_list$genres)


# Transforming the list of genres
docs <- Corpus(VectorSource(genre_list)) # Setting list of genres as a Corpus
SpaceToUnderscore <- content_transformer(function (x , pattern ) gsub(pattern, "_", x))
docs <- tm_map(docs, SpaceToUnderscore, " ")
g <- TermDocumentMatrix(docs)

# Setting up final data frame with genres
m <- as.matrix(g)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v) %>% head(15)




# -----------------------------------------------


top50USA <- get_playlist("37i9dQZEVXbLRQDuF5jeBp")

topTracks <- get_playlist_tracks(playlist_id = "37i9dQZEVXbLRQDuF5jeBp")




# get each track name, number, and id
topTracksName <- topTracks$track.name # track name
totalTracks <- length(topTracksName) # total number
topTracksID <- topTracks$track.id # track id

# get each track artist
myArtists <- topTracks$track.artists
myArtistsName <- c()

# get each track artist name
for(n in 1:totalTracks){
  if(length(myArtists[[n]][[3]])){
    myArtistsName <- append(
      myArtistsName, 
      myArtists[[n]][[3]][[1]])
  }
  else{
    myArtistsName <- append(
      myArtistsName, 
      myArtists[[n]][[3]])
  }
}

# get each track album name, ID, release date
topTracksAlbum <- topTracks$track.album.name
topTracksAlbumID <- topTracks$track.album.id
topTracksAlbumRelease <- topTracks$track.album.release_date

topTracksPopularity <- topTracks$track.popularity
topTracksColor <- topTracks$primary_color

# myPlaylists dataframe
myPlaylists <- data.frame(
  "Track" = topTracksName,
  "Artist" = myArtistsName,
  "Album" = topTracksAlbum,
  "Popularity" = topTracksPopularity,
  "Primary Color" = topTracksColor
  
)


# get each track duration
topTrackDuration <- topTracks$track.duration_ms

# get each track info
topTracksInfo1 <- get_track_audio_features(topTracksID[0:totalTracks])


topTracksInfo <- rbind.data.frame(topTracksInfo1)

topTrackDanceability <- topTracksInfo[, "danceability"]
topTrackEnergy <- topTracksInfo[, "energy"]
topTrackAcousticness <- topTracksInfo[, "acousticness"]
topTrackKey <- topTracksInfo[, "key"]
topTrackLoudness <- topTracksInfo[, "loudness"]
topTrackMode <- topTracksInfo[, "mode"]
topTrackspeechiness <- topTracksInfo[, "speechiness"]
topTrackInstrumentalness <- topTracksInfo[, "instrumentalness"]
topTrackLiveness <- topTracksInfo[, "liveness"]
topTrackValence <- topTracksInfo[, "valence"]
topTrackTempo <- topTracksInfo[, "tempo"]




df_usaTop50 <- data.frame(
  "track.id" = topTracksID,
  "track.name" = topTracksName,
  "track.artist" = myArtistsName,
  "track.album.id" = topTracksAlbumID,
  "track.album.name" = topTracksAlbum,
  "track.popularity" = topTracksPopularity,
  "track.album.release" = topTracksAlbumRelease,
  "danceability" = topTrackDanceability,
  "energy" = topTrackEnergy,
  "key" = topTrackKey,
  "loudness" = topTrackLoudness,
  "mode" = topTrackMode,
  "speechiness" = topTrackspeechiness,
  "acousticness" = topTrackAcousticness,
  "instrumentalness" = topTrackInstrumentalness,
  "liveness" = topTrackLiveness,
  "valence" = topTrackValence,
  "tempo" = topTrackTempo,
  "duration_ms" = topTrackDuration 
)

  
  


# -----------------------------------------------------------


track_features <-
  select(df_usaTop50,
         liveness,
         speechiness,
         energy,
         danceability,
         acousticness,
         valence) %>%
  pivot_longer(cols = liveness:valence,
               names_to = "feature",
               values_to = "value")

density1 <-
  ggplot(track_features, aes(x = value, y = feature, fill = feature)) +
  geom_density_ridges() +
  theme_ridges(font_size = 12, font_family = "Roboto Condensed") +
  scale_fill_manual(values = c(
    "#F230AA",
    "#EEAEC6",
    "#D3FB67",
    "#4FE383",
    "#468B78",
    "#171C18"
  )) +
  theme(
    legend.position = "none",
    text = element_text(size = 11, family = "Roboto Condensed"),
    axis.text.y = element_text(size = 10, family = "Roboto Condensed"),
    axis.text.x = element_text(size = 9)
  )





