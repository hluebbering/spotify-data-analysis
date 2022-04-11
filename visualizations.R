# Load Libraries
library(Rmisc)
library(plotrix)
library(gridExtra)
library(ggridges)
library(dplyr)
library(kableExtra)
library(reactable)
library(htmltools)
library(reactablefmtr)
library(ggplot2)
library(hrbrthemes)
library(ggcorrplot)
library(reactablefmtr)
import_roboto_condensed()

# Read in Data.rds
df <- readRDS(file = "assets/data/spotify_data.rds")

######################################################
################## QUESTION 1 PLOTS ##################

dfQ1 <- df[df$genre == c('pop', 'rock', 'rap', 'hip hop', 'r&b'), ] %>% 
  group_by(Date,genre) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

dfQ1$Date <- as.Date(dfQ1$Date)

pQ1 <- ggplot(data = dfQ1, mapping = aes(x = Date, y = Freq, group = genre, color = genre, text = Freq)) +
  geom_line() +
  ggtitle("Proportions of Popular Genres Over Time") +
  scale_color_manual(values = c("#215732", "#32fbad", 
                                "#C724B1", "#2CC84D", 
                                "#EFB661")) + #32fbad
  theme_ipsum_rc(plot_title_size = 13) +
  theme(axis.text.x=element_text(hjust=1), axis.title.y = element_blank(),
        axis.title.x = element_blank(), plot.title = element_text(vjust = -50))

################## QUESTION 1A ##################

dfQ1$Day <- weekdays(as.Date(dfQ1$Date))
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday')
dfQ1$day_type <- factor((weekdays(dfQ1$Date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))


pQ1A <- ggplot(data = dfQ1, mapping=aes(x=genre, y = Freq, fill = day_type)) +
  geom_bar(position = "dodge", stat = "identity", width=0.8) +
  scale_fill_manual(values = c("#C724B1", "#215732")) +
  theme_ipsum_rc(plot_title_size = 13) + ggtitle("Proportions of Popular Genres on Weekends vs. Weekdays") +
  theme(axis.text.x=element_text(hjust=1), axis.title.y = element_blank())


################## QUESTION 1B ##################


dfQ1$Date2 <- format(dfQ1$Date, "%m-%d")

dfQ1 <- dfQ1 %>% mutate(holiday_season = ifelse(Date2 >= "11-25" & Date2 <= "12-31", 
                                         "holiday_season", "not_holiday_season"))

pQ1B <- ggplot(data = dfQ1, mapping=aes(x=genre, y=Freq, fill = holiday_season)) +
  geom_bar(position = "dodge", stat = "identity", width=0.8) +
  scale_fill_manual(values = c("#32fbad", "#2CC84D")) +
  theme_ipsum_rc(plot_title_size = 13) + ggtitle("Proportions of Popular Genres on Holiday Seasons") +
  theme(axis.text.x=element_text(hjust=1), axis.title.y = element_blank())

################## QUESTION 1C ##################


dfQ1$Month <- months(as.Date(dfQ1$Date))
spring <- c('March', 'April', 'May')
summer <- c('June', 'July', 'August')
fall <- c('September', 'October', 'November')
winter <- c('December', 'January', 'February')

dfQ1 <- dfQ1 %>%
  mutate(
    season = case_when(
      Month %in% spring ~ "Spring",
      Month %in%  summer  ~ "Summer",
      Month %in%  fall  ~ "Fall",
      Month %in% winter ~ "Winter"))


pQ1C <- ggplot(data = dfQ1, mapping=aes(x=genre, y=Freq, fill = season)) +
  geom_bar(position = "dodge", stat = "identity", width=0.85) +
  scale_fill_manual(values = c("#2CC84D", "#215732", "#C724B1", "#e69e19")) +
  theme_ipsum_rc(plot_title_size = 13) + ggtitle("Proportions of Popular Genres on Different Seasons") +
  theme(axis.text.x=element_text(hjust=1), axis.title.y = element_blank())


################## QUESTION 1 TABLES ##################


preview.Q1 <- kable(
  data.frame(
    "track_id" = c("OwbnC9AlJenxp613TYalsGK", "71J1100y21WplE<ib2ErSA", "lhy6kKvsPbv7VTcllCw", "5uCalC9HTNlzGyblSt03vOh"),
    "Date" = c("2018-04-03", "2021-12-07", "2018-12-23", "2019-08-13"),
    "Weekday" = c("TRUE", "TRUE", "FALSE", "TRUE"),
    "Holiday" = c("FALSE", "TRUE", "TRUE", "FALSE"),
    "Season" = c("spring", "winter", "winter", "summer"),
    "Pop" = c("FALSE", "TRUE", "TRUE", "TRUE"),
    "Rap" = c("TRUE", "FALSE", "FALSE", "FALSE"),
    "Hiphop" = c("FALSE", "FALSE", "FALSE", "FALSE"),
    "Rb" = c("FALSE", "FALSE", "FALSE", "FALSE"),
    "Rock" = c("FALSE", "FALSE", "FALSE", "FALSE"))) %>%
  kable_styling(bootstrap_options = c("hover", "striped"), full_width = T, html_font = "Roboto", font_size = 12) %>%
  row_spec(0:4, hline_after = T, extra_css = "text-align: left;") %>%
  column_spec(1, color = "#777777", extra_css = "font-weight: 400; text-transform: uppercase; letter-spacing: 1.25px; font-size: 11px;") %>%
  column_spec(2:10, color = "#555", extra_css = "font-weight: 300 !important; font-family: Roboto Condensed;") %>%
  row_spec(0, color = "#111111", extra_css = "text-transform: uppercase; letter-spacing: 1.25px; font-weight: 400; font-size: 11px;") %>%
  kable_paper() %>%
  scroll_box(width = "96%", extra_css = "border: none !important; padding: 2pt !important; overflow-x: overlay !important;")


dfQ1AR1 <- data.frame("Genre" = c("Pop", "Rap", "Hip Hop", "R&B", "Rock"),
                      "Weekday" = c(0.3607, 0.3969, 0.1489, 0.0186, 0.0340),
                      "Weekend" = c(0.3512, 0.4042, 0.1523, 0.0176, 0.0363),
                      "Difference" = c(0.0096, -0.0073, -0.0034, 0.0010, -0.0024),
                      "% Difference" = c("2.65%", "-1.84%", "-2.25%", "5.17%", "-7.02%"),
                      "$Z$ Value" = c(12.8944, -9.6403, -6.0738, 4.6519, -8.3912),
                      "P-Value" = c("4.8375e-38", "5.4052e-22", "1.2491e-09", "3.2891e-06", "4.8109e-17"))

dfQ1AR1 <- kable(dfQ1AR1, col.names = c("Genre", "Weekday", "Weekend", "Max $\\Delta$", "% Difference",
                                        "$Z$ Value", "P-Value"), escape = F) %>%
  kable_styling(bootstrap_options = c("hover", "striped"), full_width = T, html_font = "Roboto", font_size = 12) %>%
  row_spec(0:5, hline_after = T, extra_css = "text-align: left;") %>%
  column_spec(1, color = "#777777", extra_css = "font-weight: 400; text-transform: uppercase; letter-spacing: 1.25px; font-size: 11px;") %>%
  column_spec(2:7, color = "#555", extra_css = "font-weight: 300 !important; font-family: Roboto Condensed;") %>%
  row_spec(0, color = "#111111", extra_css = "text-transform: uppercase; letter-spacing: 1.25px; font-weight: 400; font-size: 11px;") %>%
  column_spec(3, background = "#215732",color = "white", extra_css = "font-weight: 300 !important;") %>%
  column_spec(2, background = "#C724B1",color = "white", extra_css = "font-weight: 300 !important;")%>%
  scroll_box(width = "100%", extra_css = "border: none !important; padding: 2pt !important; overflow-x: overlay !important;") %>%
  kable_paper()



dfQ1BR1 <- data.frame("Genre" = c("Pop", "Rap", "Hip Hop", "R&B", "Rock"),
                      "Not Holiday" = c(0.3614, 0.4094, 0.1547, 0.0184, 0.0312),
                      "Holiday" = c(0.3118, 0.3116, 0.1096, 0.0159, 0.0710),
                      "Difference" = c(0.0496, 0.0978, 0.0450, 0.0026, -0.0398),
                      "% Difference" = c("13.73%", "23.90%", "29.12%", "13.87%", "-127.76%"),
                      "$Z$ Value" = c(-39.927, -76.965, -48.5640, -7.3751, 83.4927),
                      "P-Value" = c("$\\lt$ 2.2e-16", "$\\lt$ 2.2e-16", 
                                    "$\\lt$ 2.2e-16", "1.6425e-13", "$\\lt$ 2.2e-16"))

dfQ1BR1 <- kable(dfQ1BR1, col.names = c("Genre", "Not Holiday", "Holiday", "Max $\\Delta$", "% Difference",
                                        "$Z$ Value", "P-Value"), escape = F) %>%
  kable_styling(bootstrap_options = c("hover", "striped"), full_width = T, html_font = "Roboto", font_size = 12) %>%
  row_spec(0:5, hline_after = T, extra_css = "text-align: left;") %>%
  column_spec(1, color = "#777777", extra_css = "font-weight: 400; text-transform: uppercase; letter-spacing: 1.25px; font-size: 11px;") %>%
  column_spec(2:7, color = "#555", extra_css = "font-weight: 300 !important; font-family: Roboto Condensed;") %>%
  row_spec(0, color = "#111111", extra_css = "text-transform: uppercase; letter-spacing: 1.25px; font-weight: 400; font-size: 11px;") %>%
  column_spec(3, background = "#32fbad", color = "black", extra_css = "font-weight: 300 !important;") %>%
  column_spec(2, background = "#2CC84D", color = "black", extra_css = "font-weight: 300 !important;")%>%
  scroll_box(width = "100%", extra_css = "border: none !important; padding: 2pt !important; overflow-x: overlay !important;") %>%
  kable_paper()
  


dfQ1CR1 <- data.frame("Genre"	 = c("Pop", "Rap", "Hip Hop", "R&B", "Rock"),
                      "Spring" = c(0.3624, 0.4137, 0.1541, 0.0190, 0.0285),
                      "Summer"= c(0.3629, 0.4069, 0.1542, 0.0152, 0.0355),
                      "Fall" = c(0.3535, 0.4047, 0.1504, 0.0208, 0.0371),
                      "Winter" = c(0.3476, 0.3751, 0.1428, 0.0178, 0.0394),
                      "Max Difference" = c(0.0153, 0.0386, 0.0114, 0.0056, 0.0109),
                      "% Increase" = c("4.39%", "10.28%", "7.98%", "36.65%", "38.19%"),
                      "$X^2$ Value" = c(147.82, 701.28, 218.88, 359.73, 827.5),
                      "P-Value" = c("$\\lt$ 2.2e-16", "$\\lt$ 2.2e-16", "$\\lt$ 2.2e-16", "$\\lt$ 2.2e-16", "$\\lt$ 2.2e-16"))
                                       

# "#2CC84D", "#215732", "#C724B1", "#e69e19"

dfQ1CR1 <- kable(dfQ1CR1, col.names = c("Genre", "Spring", "Summer", "Fall", "Winter", "Max $\\Delta$", "% Difference",
                                        "$X^2$ Value", "P-Value"), escape = F) %>%
  kable_styling(bootstrap_options = c("hover", "striped"), full_width = T, html_font = "Roboto", font_size = 12) %>%
  row_spec(0:5, hline_after = T, extra_css = "text-align: left;") %>%
  column_spec(1, color = "#777777", extra_css = "font-weight: 400; text-transform: uppercase; letter-spacing: 1.25px; font-size: 11px;") %>%
  column_spec(2:9, color = "#555", extra_css = "font-weight: 300 !important; font-family: Roboto Condensed;") %>%
  row_spec(0, color = "#111111", extra_css = "text-transform: uppercase; letter-spacing: 1.25px; font-weight: 400; font-size: 11px;") %>%
  column_spec(2, background = "#215732",color = "white", extra_css = "font-weight: 300 !important;") %>%
  column_spec(3, background = "#C724B1",color = "white", extra_css = "font-weight: 300 !important;")%>%
  column_spec(4, background = "#2CC84D",color = "white", extra_css = "font-weight: 300 !important;")%>%
  column_spec(5, background = "#e69e19",color = "white", extra_css = "font-weight: 300 !important;") %>%
  scroll_box(width = "100%", extra_css = "border: none !important; padding: 2pt !important; overflow-x: overlay !important;") %>% 
  kable_paper()



###################################################### 
################## QUESTION 2 PLOTS ##################

dfQ2 <- df %>% 
  group_by(Date) %>%
  summarise(valence.mean = mean(valence))
dfQ2$Date <- as.Date(dfQ2$Date)
dfQ2 <- dfQ2 %>% mutate(cov = ifelse(Date < "2020-03-13", "before", "after"))


pQ2 <- ggplot(data = dfQ2, mapping = aes(x = Date, y = valence.mean, color=cov)) +
  geom_line() +
  ggtitle("Daily Mean Valence Over Time") +
  scale_color_manual(values = c("#215732", "#20d761")) +  
  theme_ipsum_rc(plot_title_size = 13) +
  theme(axis.text.x=element_text(hjust=1), 
        axis.title.y = element_blank()) +
  theme(legend.position="bottom") + labs(color = "COVID-19")

################## QUESTION 2 TABLES ##################

df$Date <- as.Date(df$Date) # Convert character to date
df <- df %>%
  mutate(covid = case_when(Date < "2020-03-13" ~ "before",
                           Date >= "2020-03-13" ~ "after")) # Adding column based on other column

df.valence <- df %>% 
  select(Track.Name, valence, Date, covid)
df.valence <- data.frame(df.valence[sample(1:nrow(df.valence)), ], row.names = NULL)
df.valence <- head(df.valence, 5)
colnames(df.valence) <- c("track", "valence", "date", "covid")

preview.Q2 <- kable(df.valence, escape = F) %>%
  kable_styling(bootstrap_options = c("hover", "striped"), full_width = T, html_font = "Roboto", font_size = 12) %>%
  row_spec(0:5, hline_after = T, extra_css = "text-align: left;") %>%
  column_spec(1, color = "#777777", extra_css = "font-weight: 400; text-transform: uppercase; letter-spacing: 1.25px; font-size: 11px;") %>%
  column_spec(2:4, color = "#555", extra_css = "font-weight: 300 !important; font-family: Roboto Condensed;") %>%
  row_spec(0, color = "#111111", extra_css = "text-transform: uppercase; letter-spacing: 1.25px; font-weight: 400; font-size: 11px;") %>%
  kable_paper()




###################################################### 
################## QUESTION 3 PLOTS ##################



dfQ3 <- df %>% select(c(track_id, acousticness, danceability, energy, duration, instrumentalness, key, liveness, loudness, mode, speechiness, tempo, valence))
dfQ3$updated_rank <- 201 - df$Rank


dfQ3A <- dfQ3 %>% 
  group_by(track_id) %>%
  summarise(
    acousticness = mean(acousticness, na.rm=T),
    danceability = max(danceability, na.rm=T),
    energy = mean(energy, na.rm=T), 
    duration = mean(duration, na.rm=T), 
    instrumentalness = mean(instrumentalness, na.rm=T), 
    key = mean(key, na.rm=T), 
    liveness = mean(liveness, na.rm=T), 
    loudness = mean(loudness, na.rm=T), 
    mode = mean(mode, na.rm=T), 
    speechiness = mean(speechiness, na.rm=T), 
    tempo = mean(tempo, na.rm=T), 
    valence = mean(valence, na.rm=T),
    popularity_score = sum(updated_rank))


pQ3A <- ggplot(data = dfQ3A, mapping=aes(x=danceability, y=popularity_score)) +
  geom_point(colour = "#20d761") +
  theme_ipsum_rc(plot_title_size = 13) + ggtitle("Song Popularity versus Danceability") +
  xlab("Danceability") + ylab("Popularity Score") + 
  theme(axis.text.x=element_text(hjust=1))


dfQ3B <- dfQ3A %>% 
  pivot_longer(
    cols = c('acousticness', 'energy', 'duration', 'instrumentalness', 
             'key', 'liveness', 'loudness', 'mode', 'speechiness', 'tempo', 
             'valence'), 
    names_to = "audio_feature", 
    values_to = "audio_feature_vals"
  )



pQ3B <- ggplot(data = dfQ3B, mapping = aes(x = audio_feature_vals, y = popularity_score)) +
  geom_point(colour = "#20d761",  alpha = .3) +
  ylab("Popularity Score") +
  facet_wrap(~audio_feature, scales="free_x", switch = "x") + 
  theme_ipsum_rc() +
  theme(axis.text.x=element_text(hjust=1), 
        axis.title.y = element_blank()) +
  theme(axis.text.x=element_text(hjust=1), 
        axis.title.x = element_blank(),
        legend.key = element_rect(linetype = 0),
        strip.background = element_rect(color="transparent", fill="transparent", size=1),
        strip.placement = "outside")


