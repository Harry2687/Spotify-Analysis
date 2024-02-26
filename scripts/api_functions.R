# Match artist name to artist id
search_artist <- function(artist_name) {
  url <- "https://api.spotify.com/v1/search"
  response <- GET(url, add_headers(Authorization = paste("Bearer", spotify_token)), query = list(q = artist_name, type = "artist", limit = 1))
  content(response)$artists$items[[1]]$id
}

# Match artist id to artist genres
get_genres <- function(artist_id) {
  url <- paste0("https://api.spotify.com/v1/artists/", artist_id)
  response <- GET(url, add_headers(Authorization = paste("Bearer", spotify_token)))
  content(response)$genres
}

# Match track and artist name to track id via search
search_track <- function(track_name, artist_name) {
  # build in delay to not get rate limited
  Sys.sleep(0.4)
  track_name <- track_name %>%
    str_replace_all("[^a-zA-Z\\s]", "") %>%
    trimws() %>%
    str_replace_all(" ", "%20")
  artist_name <- artist_name %>%
    str_replace_all("[^a-zA-Z\\s]", "") %>%
    trimws() %>%
    str_replace_all(" ", "%20")
  url <- paste0("https://api.spotify.com/v1/search?q=track:", 
                track_name, 
                "%20artist:", 
                artist_name, 
                "&type=track&limit=1")
  response <- GET(url, add_headers(Authorization = paste("Bearer", spotify_token)))
  fromJSON(content(response, "text", encoding = "UTF-8"))$tracks$items$id
}

# Match track id to autio features
get_audio_features <- function(track_id) {
  # build in delay to not get rate limited
  Sys.sleep(0.4)
  url = paste0("https://api.spotify.com/v1/audio-features/", track_id)
  response <- GET(url, add_headers(Authorization = paste("Bearer", spotify_token)))
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  if ("error" %in% names(data)) {
    source("scripts/generate_token.R")
    Sys.sleep(0.4)
    response <- GET(url, add_headers(Authorization = paste("Bearer", spotify_token)))
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    return(data)
  }
}