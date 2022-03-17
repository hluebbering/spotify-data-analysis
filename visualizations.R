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

# Read in Data.rds
df <- readRDS(file = "assets/data/spotify_data.rds")

######################################################
################## QUESTION 1 PLOTS ##################

dfQ1 <- df[df$genre == c('pop', 'rock', 'rap', 'hip hop', 'r&b'), ] %>% 
  group_by(Date,genre) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

dfQ1$Date <- as.Date(dfQ1$Date)

pQ1 <- ggplot(data = dfQ1, mapping = aes(x = Date, y = Freq, group = genre, color = genre)) +
  geom_line() +
  ggtitle("Proportions of Popular Genres Over Time") + ylab("Proportion") +
  scale_color_manual(values = c("#4D49BE", "#C8E379","#EEDAEA","#2CC84D", "#E277CD")) +  
  theme_ipsum(base_size = 11, base_family = "Roboto Condensed") + 
  theme(axis.text.x=element_text(angle=60, hjust=1, size = 8), 
        axis.text.y = element_text(size = 8),
        plot.title = element_text(color = "white", size = 12, face = "bold"),
        axis.text = element_text(color = "white", size = 10),
        axis.title.x = element_text(color = "white", size = 10),
        axis.title.y = element_text(color = "white", size = 10),
        legend.background = element_rect(fill = "#3F3F3F"),
        legend.key = element_rect(fill = "#3F3F3F", linetype = 0),
        legend.text = element_text(color = "white", size = 10),
        legend.title = element_text(color = "white", size = 10),
        panel.background = element_rect(fill = "#3F3F3F"),
        panel.grid = element_line(color = "#262626"),
        plot.background = element_rect(fill = "#1D1D1D"))


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
  kable_styling(bootstrap_options = c("hover", "striped"), full_width = FALSE, html_font = "Roboto", font_size = 11)  %>%
  row_spec(row = c(0:4), hline_after = F, extra_css = "border-bottom: 1px solid #FEFEFE;")  %>%
  column_spec(1:10, color = "#555", extra_css = "font-weight: 300;") %>%
  row_spec(0, color = "#111111", extra_css = "text-transform: uppercase; letter-spacing: 1.25px; font-weight: 400; font-size: 10.5px")



###################################################### 
################## QUESTION 2 PLOTS ##################

dfQ2 <- df %>% 
  group_by(Date) %>%
  summarise(valence.mean = mean(valence))
dfQ2$Date <- as.Date(dfQ2$Date)
dfQ2 <- dfQ2 %>% mutate(cov = ifelse(Date < "2020-03-13", "before", "after"))


pQ2 <- ggplot(data = dfQ2, mapping = aes(x = Date, y = valence.mean, color=cov)) +
  geom_line() +
  ggtitle("Daily Mean Valence Over Time") + ylab("Proportion") +
  scale_color_manual(values = c("#C8E379", "#79D97C")) +  
  theme_ipsum(base_size = 11, base_family = "Roboto Condensed") +
  theme(axis.text.x=element_text(angle=60, hjust=1, size = 8), 
        axis.text.y = element_text(size = 8),
        plot.title = element_text(color = "white", size = 12, face = "bold"),
        axis.text = element_text(color = "white", size = 10),
        axis.title.x = element_text(color = "white", size = 10),
        axis.title.y = element_text(color = "white", size = 10),
        legend.background = element_rect(fill = "#3F3F3F"),
        legend.key = element_rect(fill = "#3F3F3F", linetype = 0),
        legend.text = element_text(color = "white", size = 10),
        legend.title = element_text(color = "white", size = 10),
        panel.background = element_rect(fill = "#3F3F3F"),
        panel.grid = element_line(color = "#262626"),
        plot.background = element_rect(fill = "#1D1D1D")) + labs(color = "COVID-19")

################## QUESTION 2 TABLES ##################

df$Date <- as.Date(df$Date) # Convert character to date
df <- df %>%
  mutate(covid = case_when(Date < "2020-03-13" ~ "before",
                           Date >= "2020-03-13" ~ "after")) # Adding column based on other column

df.valence <- df %>% 
  select(Track.Name, valence, Date, covid)
df.valence <- data.frame(df.valence[sample(1:nrow(df.valence)), ], row.names = NULL)
df.valence <- head(df.valence, 10)
colnames(df.valence) <- c("track", "valence", "date", "covid")

preview.Q2 <- kable(df.valence, escape = F) %>%
  kable_styling(bootstrap_options = c("hover", "striped"), full_width = F, html_font = "Roboto", font_size = 12.5) %>%
  row_spec(0:10, hline_after = F, extra_css = "border-bottom: 1px solid #FEFEFE;") %>%
  column_spec(1, color = "#111111", extra_css = "font-weight: 400;") %>%
  column_spec(2:4, color = "#555", extra_css = "font-weight: 300;") %>%
  row_spec(0, color = "#111111", extra_css = "text-transform: uppercase; letter-spacing: 1.25px; font-weight: 400; font-size: 11.5px")


