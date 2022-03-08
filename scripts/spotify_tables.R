library(reactable)
library(kableExtra)
library(dplyr)

####################################################################

# **Variables:** 
# `album`, `album_id`, `track_name`, `track_id`, 
# `artists`, `artists_ids`, `main_artist`, 
# `main_artist_id`, `acousticness`, `danceability`, 
# `energy`, `instrumentalness`, `key`, `liveness`, 
# `loudness`, `mode`, `speechiness`, `tempo`, 
# `valence`, `duration`, `explicit`, `time_signature`, `genre`.


df.description <- data.frame(
  "Variable" = c(
    "date",
    "track_id",
    "track_name",
    "all_artists",
    "main_artist",
    "main_artist_id",
    "rank",
    "streams",
    "acousticness",
    "danceability",
    "energy",
    "instrumentalness",
    "key",
    "liveness",
    "loudness",
    "mode",
    "speechiness",
    "tempo",
    "valence",
    "duration",
    "explicit",
    "genre"
  ),
  
  "Type" = c(
    "Categorical, date",
    "Categorical, str",
    "Categorical, str",
    "Categorical, str",
    "Categorical, str",
    "Categorical, str",
    "Quantitative, int",
    "Quantitative, int",
    "Quantitative, float",
    "Quantitative, float",
    "Quantitative, float",
    "Quantitative, float",
    "Categorical, int",
    "Quantitative, float",
    "Quantitative, float",
    "Categorical, int",
    "Quantitative, float",
    "Categorical, int",
    "Quantitative, float",
    "Quantitative, int",
    "Categorical, boolean",
    "Categorical, str"
  ),
  
  "Description" = c(
    "Date of the spotify chart",
    "Unique identifier for each track",
    "Title of the track",
    "List of all artist names that appeared on the track",
    "Name of the main artist",
    "Unique identifier for each artist",
    "Rank from 1-200 (1 is the most streamed track that day)",
    "Total number of global streams that day",
    "Confidence measure of sound through acoustic (1.0 is the most acoustic)",
    "Dance friendly measurement (1.0 is most danceable)",
    "Perceptual measure of intensity and activity",
    "Variety of instruments appeared",
    "Overall key of the track, sets of sharp or flat",
    "Detection of whether a track was peformed live with an audience",
    "Overall loudness of a track in decibels (dB)",
    "Modality (major or minor) of a track, the type of scale",
    "Measures the number of spoken words",
    "Estimated tempo of a track in beats per minute (BPM)",
    "Measure from 0.0 to 1.0 describing the musical positiveness",
    "Duration of track in milliseconds",
    "True or false if contains explicit content",
    "Name of the genre associated with that track"
  )
)


##########################################


spotify_theme <- function() {
  text_color <- "hsl(0, 0%, 95%)"
  text_color_light <- "hsl(0, 0%, 70%)"
  text_color_lighter <- "hsl(0, 0%, 55%)"
  bg_color <- "hsl(0, 0%, 10%)"
  
  reactableTheme(
    color = text_color,
    backgroundColor = bg_color,
    borderColor = "hsl(0, 0%, 16%)",
    borderWidth = "1px",
    highlightColor = "rgba(255, 255, 255, 0.1)",
    cellPadding = "10px 8px",
    style = list(
      fontFamily = "Work Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
      fontSize = "12px",
      "a" = list(
        color = text_color,
        "&:hover, &:focus" = list(
          textDecoration = "none",
          borderBottom = "1px solid currentColor"
        )
      ),
      ".number" = list(
        color = text_color_light,
        fontFamily = "Source Code Pro, Consolas, Monaco, monospace"
      ),
      ".tag" = list(
        padding = "2px 4px",
        color = "hsl(0, 0%, 40%)",
        fontSize = "12px",
        border = "1px solid hsl(0, 0%, 24%)",
        borderRadius = "2px"
      )
    ),
    headerStyle = list(
      color = text_color_light,
      fontWeight = 400,
      fontSize = "10px",
      letterSpacing = "1px",
      textTransform = "uppercase",
      "&:hover, &:focus" = list(color = text_color)
    ),
    rowHighlightStyle = list(
      ".tag" = list(color = text_color, borderColor = text_color_lighter)
    ),
    paginationStyle = list(color = text_color_light),
    pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)")
  )
}




spotify_theme2 <- function() {
  text_color <- "#333"
  text_color_light <- "#555"
  text_color_lighter <- "#777"
  bg_color <- "#faf9fc"
  
  reactableTheme(
    color = text_color,
    backgroundColor = bg_color,
    borderColor = "#ddd",
    borderWidth = "1px",
    highlightColor = "rgba(255, 255, 255, 0.1)",
    cellPadding = "10px 8px",
    style = list(
      fontFamily = "Work Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
      fontSize = "12px",
      "a" = list(
        color = text_color,
        "&:hover, &:focus" = list(
          textDecoration = "none",
          borderBottom = "1px solid currentColor"
        )
      ),
      ".number" = list(
        color = text_color_light,
        fontFamily = "Source Code Pro, Consolas, Monaco, monospace"
      ),
      ".tag" = list(
        padding = "2px 4px",
        color = "hsl(0, 0%, 40%)",
        fontSize = "12px",
        border = "1px solid hsl(0, 0%, 24%)",
        borderRadius = "2px"
      )
    ),
    headerStyle = list(
      color = text_color_light,
      fontWeight = 400,
      fontSize = "10px",
      letterSpacing = "1px",
      textTransform = "uppercase",
      "&:hover, &:focus" = list(color = text_color)
    ),
    rowHighlightStyle = list(
      ".tag" = list(color = text_color, borderColor = text_color_lighter)
    ),
    paginationStyle = list(color = text_color_light),
    pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)")
  )
}


spotify_theme3 <- function() {
  text_color <- "#333"
  text_color_light <- "#555"
  text_color_lighter <- "#777"
  bg_color <- "#faf9fc"
  
  reactableTheme(
    color = text_color,
    backgroundColor = bg_color,
    borderColor = "#ddd",
    borderWidth = "1px",
    highlightColor = "rgba(255, 255, 255, 0.1)",
    cellPadding = "10px 8px",
    style = list(
      fontFamily = "Work Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
      fontSize = "12px",
      "a" = list(
        color = text_color,
        "&:hover, &:focus" = list(
          textDecoration = "none",
          borderBottom = "1px solid currentColor"
        )
      ),
      ".number" = list(
        color = text_color_light,
        fontFamily = "Source Code Pro, Consolas, Monaco, monospace"
      ),
      ".tag" = list(
        padding = "2px 4px",
        color = "hsl(0, 0%, 40%)",
        fontSize = "12px",
        border = "1px solid hsl(0, 0%, 24%)",
        borderRadius = "2px"
      )
    ),
    headerStyle = list(
      color = text_color_light,
      fontWeight = 400,
      fontSize = "10px",
      letterSpacing = "1px",
      textTransform = "uppercase",
      "&:hover, &:focus" = list(color = text_color)
    ),
    rowHighlightStyle = list(
      ".tag" = list(color = text_color, borderColor = text_color_lighter)
    ),
    paginationStyle = list(color = text_color_light),
    pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)")
  )
}


