spotify_data <- read.table("Spotify_Messy_210492989.txt", sep = "\t", header = TRUE)
spotify_data
View(spotify_data) #checking the untidy data set in table format

#installs and starts the packages
install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)


#clean column names
colnames(spotify_data) <- gsub("[0-9f]", "", colnames(spotify_data))

#make everything in the dataset lowercase
spotify_data %>% rename_with(tolower,everything())

#change column name to correct name
colnames(spotify_data)[colnames(spotify_data)== "TTrack_name"] <- "track_name"



# Separate 'danceability_energy' into two columns: 'danceability' and 'energy'
spotify_data <- spotify_data %>%
  separate_wider_delim("danceability.energy", "_",
                       names = c("danceability",
                                 "energy"))

#commit -----------------------------------------------------------------------

# Rename 'track_id' to 'track album and playlist id'
spotify_data <- spotify_data %>%
  rename("track_album_and_playlist_id" = track_id)

# Remove columns 'album_id' and 'playlist_id' as they are copies of 'track id'
spotify_data <- select(spotify_data, -c(track_album_id, playlist_id))

# Remove non-numeric characters from the 'mode' column
spotify_data <- spotify_data %>%
  mutate(mode = gsub("[^0-9.]", "", mode))

# Convert specific columns to 'double' data structure in spotify_data
spotify_data <- spotify_data %>%
  mutate(track_popularity = as.double(track_popularity),
         danceability = as.double(danceability),
         energy = as.double(energy),
         key = as.double(key),
         loudness = as.double(loudness),
         mode = as.double(mode),
         speechiness = as.double(speechiness),
         acousticness = as.double(acousticness),
         instrumentalness = as.double(instrumentalness),
         liveness = as.double(liveness),
         valence = as.double(valence),
         tempo = as.double(tempo),
         duration_ms = as.double(duration_ms))

#commit -----------------------------------------------------------------------

print(colnames(spotify_data))  
str(spotify_data)
glimpse(spotify_data)

#perform case-insensitive search and replacement for 'Bad Sunny'
spotify_data <- spotify_data %>%
  mutate(track_artist = case_when(
    tolower(track_artist) == "bad sunny" ~ "Bad Bunny",
    TRUE ~ track_artist
  ))

#commit -----------------------------------------------------------------------

#the numeric values will be shown in their full decimal format
options(scipen = 999)

spotify_data <- spotify_data %>%
  pivot_longer(cols = c("pop", "rap", "rock", "r.b", "edm"), names_to = "playlist_genre", values_to = "playlist_subgenre", values_drop_na = TRUE)

#commit -----------------------------------------------------------------------

#ensures each column follows the directory's description, else produces an NA result.
spotify_data <- spotify_data %>%
  mutate(
    danceability = case_when(
      danceability < 0 | danceability > 1 ~ NA_real_,
      TRUE ~ danceability
    ),
    energy = case_when(
      energy < 0 | energy > 1 ~ NA_real_,
      TRUE ~ energy
    ),
    key = case_when(
      !key %in% c(-1, 0:11) ~ NA_real_,
      TRUE ~ key
    ),
    loudness = case_when(
      loudness < -60 | loudness > 0 ~ NA_real_,
      TRUE ~ loudness
    ),
    mode = case_when(
      mode != 0 & mode != 1 ~ NA_real_,
      TRUE ~ mode
    ),
    speechiness = case_when(
      speechiness < 0 | speechiness > 1 ~ NA_real_,
      TRUE ~ speechiness
    ),
    acousticness = case_when(
      acousticness < 0 | acousticness > 1 ~ NA_real_,
      TRUE ~ acousticness
    ),
    instrumentalness = case_when(
      instrumentalness < 0 | instrumentalness > 1 ~ NA_real_,
      TRUE ~ instrumentalness
    ),
    liveness = case_when(
      liveness < 0 | liveness > 1 ~ NA_real_,
      TRUE ~ liveness
    ),
    valence = case_when(
      valence < 0 | valence > 1 ~ NA_real_,
      TRUE ~ valence
    )
  )


View(spotify_data)

                




