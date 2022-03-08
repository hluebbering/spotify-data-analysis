library(imager)
library(rvest)
library(magick)
library(spotifyr)

# user information
Sys.setenv(SPOTIFY_CLIENT_ID = '4cf3afdca2d74dc48af9999b1b7c9c61')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f6ca08ad37bb41a0afab5ca1dc74b208')
get_spotify_authorization_code()

img1 <- get_playlist_cover_image(playlist_id = '4qfffs5EG6ikw9YkhCGGDl')
img2 <- get_playlist_cover_image(playlist_id = '4lSykOrQfnAiCgtHKVudTT')
img3 <- get_playlist_cover_image(playlist_id = '2JaV4TSXPB3RGuqvzwUuhb')
img4 <- get_playlist_cover_image(playlist_id = '3NqHrY8dm9DBq29GowDFtw')
img5 <- get_playlist_cover_image(playlist_id = '7IX71g0Qyn5E8z7zyGz3nm')
img6 <- get_playlist_cover_image(playlist_id = '5EJyFuoHCQAgWCpF8ShZcM')
img7 <- get_playlist_cover_image(playlist_id = '4G4tqvKkP23btdmk3VtL6K')
img8 <- get_playlist_cover_image(playlist_id = '1cMIBIh038eqYtqKTh30LY')
img9 <- get_playlist_cover_image(playlist_id = '3OrOgwT6m7wNytBUoxZGjg')
img10 <- get_playlist_cover_image(playlist_id = '2QVbD9Qhnytfui7b2Y47kX')
img11 <- get_playlist_cover_image(playlist_id = '0oMxi01bMztCl8EWzQMCWI')
img12 <- get_playlist_cover_image(playlist_id = '5uOpPcmlqGoLt1B5YQVQus')
img13 <- get_playlist_cover_image(playlist_id = '09GJ0uEndlrfFWZXX423Jm')
img14 <- get_playlist_cover_image(playlist_id = '2GEeRuMNWDDb9vxIPBA0zX')
img15 <- get_playlist_cover_image(playlist_id = '188MSp2FBw0756l8umMQiW')
img16 <- get_playlist_cover_image(playlist_id = '5X1wMRk5esxItcVeKQeCKJ')



# get image url
img1 <- img1$url[1]
img2 <- img2$url[1]
img3 <- img3$url[1]
img4 <- img4$url[1]
img5 <- img5$url[1]
img6 <- img6$url[1]
img7 <- img7$url[1]
img8 <- img8$url[1]
img9 <- img9$url[1]
img10 <- img10$url[1]
img11 <- img11$url[1]
img12 <- img12$url[1]
img13 <- img13$url[1]
img14 <- img14$url[1]
img15 <- img15$url[1]
img16 <- img16$url[1]

# read image url
cover1 <- image_read(img1)
cover2 <- image_read(img2)
cover3 <- image_read(img3)
cover4 <- image_read(img4)
cover5 <- image_read(img5)
cover6 <- image_read(img6)
cover7 <- image_read(img7)
cover8 <- image_read(img8)
cover9 <- image_read(img9)
cover10 <- image_read(img10)
cover11 <- image_read(img11)
cover12 <- image_read(img12)
cover13 <- image_read(img13)
cover14 <- image_read(img14)
cover15 <- image_read(img15)      
cover16 <- image_read(img16)  

row1 <- c(cover1, cover2, cover3, cover4,
          cover5, cover6, cover7, cover8, 
          cover9, cover10, cover11, cover12, 
          cover13, cover14, cover15, cover16)
image_append(image_scale(row1, "x580"))
# image_write(R2, path = "R2.svg", format = "svg")
