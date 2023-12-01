library(httr)

Sys.setenv(SPOTIFY_ID="18234d89e4a64d29acc464eea7bc45b4")
Sys.setenv(SPOTIFY_SECRET="6d420d9ff07c4872b49aba423c888da8")

spotify_artist_top_tracks <- function(artist_id) {
  if (!is.character(artist_id)) stop("Artist ID must be a character type.")
  
  token <- spotify_token()
  top_tracks_url <- paste0(
    "https://api.spotify.com/v1/artists/", artist_id, "/top-tracks?market=US"
  )
  
  response <- httr::GET(
    url = top_tracks_url,
    add_headers("Authorization" = token[[2]])
  )
  
  status_code <- status_code(response)
  
  if (status_code != 200) {
    error_msg <- httr::http_status(response)$message
    stop(paste("Error:", error_msg))
  }
  
  top_tracks <- httr::content(response, type = "application/json")
  
  # Şarkı verilerini alma
  tracks_data <- data.frame(
    id = sapply(top_tracks$tracks, function(x) x$id),
    name = sapply(top_tracks$tracks, function(x) x$name),
    album = sapply(top_tracks$tracks, function(x) x$album$name),
    year = sapply(top_tracks$tracks, function(x) substr(x$album$release_date, 1, 4))
  )
  
  # Sanatçı adını almak için ek bir API çağrısı
  artist_url <- paste0("https://api.spotify.com/v1/artists/", artist_id)
  artist_response <- httr::GET(
    url = artist_url,
    add_headers("Authorization" = token[[2]])
  )
  
  artist_info <- httr::content(artist_response, type = "application/json")
  artist_name <- artist_info$name
  
  # Artist sütununu güncelleme
  tracks_data$artist <- artist_name
  
  result <- list(
    status_code = status_code,
    tracks_data = tracks_data
  )
  
  return(result)
}
