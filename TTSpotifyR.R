# Load in libraries
library(tidyverse)
library(janitor)
library(ggplot2)
library(spotifyr)

# read in the data
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

names(spotify_songs)
head(spotify_songs)

# what genres are there?

genre <- spotify_songs %>% 
  count(playlist_genre, playlist_subgenre) %>% 
  arrange(desc(n))

# number of differnt artists?
artist <- spotify_songs %>% 
  count(track_artist) %>% 
  arrange(desc(n))

#artist and genre?
art_gen <- spotify_songs %>% 
  count(track_artist, playlist_genre) %>% 
  arrange(desc(n))

# what is the most optimisitic rock song?

optimistic <- spotify_songs %>% 
  select(c(track_artist,track_name,playlist_genre, valence))  

optimistic2 <- optimistic %>% 
  filter(playlist_genre == "rock")

optimism <- optimistic2 %>% 
filter(valence >= 0.8) 

optimism$artist_and_track <- paste(optimism$track_artist, "-", optimism$track_name)

optimism2 <- optimism %>% 
  arrange(desc(valence)) %>% 
  head(50)

class(optimism2)

# plot the top 50 most opimistic songs

ggplot(optimism2, aes(x=reorder(artist_and_track, valence), y=valence)) +
    geom_point(stat = "identity") +
    coord_flip()       

ggplot(optimism2, aes(x=reorder(artist_and_track, valence), y=valence)) +
  geom_point(
    color="red",
    fill="#d80000",
    shape=18,
    alpha=0.5,
    size=5,
    stroke = 2
  ) +
  labs(title= "Most upbeat Rock tracks", y="Musical Positivity (valence)", x="Artist and Track", caption="data from spotifyr/Tidy Tuesday") +
  coord_flip()
  

#################
