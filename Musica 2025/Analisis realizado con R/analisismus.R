library(readr)
library(dbplyr)
library(tidyverse)
library(ggplot2)

Music <- read_csv("~/Downloads/Albums_2025.csv")

table_genre <- Music %>%
  group_by(Genre) %>%
  summarise(
    Promedio_Rating_Personal = mean(Rating_Personal, na.rm = TRUE),
    Promedio_Rating_RYM = mean(Rating_RYM, na.rm = TRUE),
    Cuenta_Genre = n(),
    Cuenta_Ranking_On_Spotify = sum(Ranking_On_Spotify != "N/A")
    ) %>%
  arrange(desc(Promedio_Rating_Personal))

table_artist <- Music %>%
  group_by(Artist_Name) %>%
  summarise(
    Promedio_Rating_Personal = mean(Rating_Personal, na.rm = TRUE),
    Promedio_Rating_RYM = mean(Rating_RYM, na.rm = TRUE),
    Cuenta_Artist = n(),
    Cuenta_Ranking_On_Spotify = sum(Ranking_On_Spotify != "N/A")
  ) %>%
  arrange(desc(Promedio_Rating_Personal))

table_country <- Music %>%
  group_by(Country) %>%
  summarise(
    Promedio_Rating_Personal = mean(Rating_Personal, na.rm = TRUE),
    Promedio_Rating_RYM = mean(Rating_RYM, na.rm = TRUE),
    Cuenta_Country = n(),
    Cuenta_Ranking_On_Spotify = sum(Ranking_On_Spotify != "N/A")
  ) %>%
  arrange(desc(Promedio_Rating_Personal))

table_year <- Music %>%
  group_by(Release_Year) %>%
  summarise(
    Promedio_Rating_Personal = mean(Rating_Personal, na.rm = TRUE),
    Promedio_Rating_RYM = mean(Rating_RYM, na.rm = TRUE),
    Cuenta_Year = n(),
    Cuenta_Ranking_On_Spotify = sum(Ranking_On_Spotify != "N/A")
  ) %>%
  arrange(desc(Promedio_Rating_Personal))

#table about which countries have more listened albums in the ranking of spotify
top_countries <- table_country %>%
  arrange(desc(Cuenta_Ranking_On_Spotify)) %>%
  head(5)

ggplot(top_countries, aes(x = reorder(Country, Cuenta_Ranking_On_Spotify), y = Cuenta_Ranking_On_Spotify, fill = Country)) + 
  geom_col() + 
  coord_flip() + 
  labs(x = "País", y = "Cantidad de álbumes en ranking de Spotify") + 
  theme_minimal() + 
  theme(legend.position = "none")

#table about relation between music rating in RYM and Ranking of spotify 
ggplot(
  Music %>% filter(Ranking_On_Spotify != "N/A"),
  aes(x = as.numeric(Ranking_On_Spotify), y = Rating_RYM)
) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#albums with more presence in the most listened albums of all time in spotify
top_genres <- table_genre[order(-table_genre$Cuenta_Ranking_On_Spotify), ][1:10, ] 

ggplot(top_genres, aes(x = reorder(Genre, Cuenta_Ranking_On_Spotify), y = Cuenta_Ranking_On_Spotify, fill = Genre)) + 
  geom_col() + coord_flip() + 
  labs(x = "Género", y = "Cantidad de álbumes en el ranking de Spotify") + 
  theme_minimal() + 
  theme(legend.position = "none")

#artist with more presence in the most listened albums of all time in spotify
top_artist <- table_artist[order(-table_artist$Cuenta_Ranking_On_Spotify), ][1:10, ] 

ggplot(top_artist, aes(x = reorder(Artist_Name, Cuenta_Ranking_On_Spotify), y = Cuenta_Ranking_On_Spotify, fill = Artist_Name)) + 
  geom_col() + coord_flip() + 
  labs(x = "Artistas", y = "Cantidad de álbumes en el ranking de Spotify") + 
  theme_minimal() + 
  theme(legend.position = "none")

#relation of artist between his rating in RYM and his presence at spotify
ggplot(
  table_genre %>% filter(Cuenta_Ranking_On_Spotify != 0),
  aes(x = Promedio_Rating_RYM, y = as.numeric(Cuenta_Ranking_On_Spotify))
) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Boxplot comparando distribuciones
table_genre %>%
  mutate(Grupo = ifelse(Cuenta_Ranking_On_Spotify > 0, "Con Spotify", "Sin Spotify")) %>%
  ggplot(aes(x = Grupo, y = Promedio_Rating_RYM, fill = Grupo)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Comparación de ratings en RYM",
       x = "Grupo de álbumes",
       y = "Promedio Rating RYM") +
  theme_minimal()

