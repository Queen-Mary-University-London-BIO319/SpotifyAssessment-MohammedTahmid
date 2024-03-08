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
